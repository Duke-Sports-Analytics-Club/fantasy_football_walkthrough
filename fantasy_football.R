# Load in libraries
# install.packages("nflreadR")
# install.packages("tidyverse")
library(nflreadr)
library(tidyverse)

# Load play-by-play
pbp <- nflreadr::load_pbp(seasons = 2020:2021)
# Load rosters
roster <- nflreadr::load_rosters(seasons = 2020:2021)
# Load snaps
snaps <- nflreadr::load_snap_counts(seasons = 2020:2021)
# Calculate player stats
player_stats <- nflfastR::calculate_player_stats(pbp, weekly = TRUE)


roster %>% count(position) %>% View()

# filter for only the positions we care about
rb_wr_te_key <- roster %>%
  filter(position %in% c("RB", "WR", "TE", "FB")) %>%
  # select only variables we need
  select(season, team, position, full_name, gsis_id, pfr_id)


rb_wr_te <- player_stats %>%
  # filter join to just the players we care about
  inner_join(rb_wr_te %>%
               distinct(gsis_id),
             by = c("player_id" = "gsis_id")) %>%
  select(player_id, player_name, team = recent_team,
         season, week, season_type, receptions:fantasy_points_ppr,
         -c(special_teams_tds, receiving_2pt_conversions, receiving_epa,
            receiving_fumbles_lost))

rb_wr_te_shares <- rb_wr_te %>%
  group_by(team, season, week) %>%
  # filter(team == "CAR",
  #        week == 1,
  #        season == 2020) %>%
  mutate(target_share = targets/sum(targets),
         air_yards_share = receiving_air_yards/sum(receiving_air_yards)) %>%
  ungroup() %>%
  select(player_id:season_type, ends_with("_share"), fantasy_points_ppr) %>%
  arrange(season, week) %>%
  group_by(player_id) %>%
  # filter(player_name == "D.Hopkins") %>%
  # find `next_ppr_points` for each player
  mutate(next_ppr_points = lead(fantasy_points_ppr)) %>%
  # rolling 5 game average
  mutate(across(c(air_yards_share, target_share, fantasy_points_ppr),
                ~ slider::slide_dbl(., .f = mean, .before = 5),
                .names = "{.col}_rolling")) %>%
  ungroup() %>%
  # cap air_yards_share so it can't be negative
  mutate(air_yards_share = pmax(0, air_yards_share))

# pivot to long format
rb_wr_te_shares_long <- rb_wr_te_shares %>%
  tidyr::pivot_longer(cols = c(target_share:fantasy_points_ppr,
                               ends_with("_rolling")),
                      names_to = "metric",
                      values_to = "value")

rb_wr_te_shares_long %>%
  group_by(metric) %>%
  summarise(broom::tidy(cor.test(value, next_ppr_points))) %>%
  arrange(desc(estimate))

rb_wr_te_shares_long %>%
  ggplot(aes(value, next_ppr_points)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  facet_wrap(~ metric, scales = "free_x")

cor(rb_wr_te_shares$air_yards_share_rolling, rb_wr_te_shares$fantasy_points_ppr_rolling)

# simple model
lm_mod <- rb_wr_te_shares %>%
  lm(next_ppr_points ~ air_yards_share_rolling + fantasy_points_ppr_rolling,
     data = .)

# predict for this week
rb_wr_te_shares %>%
  filter(season == 2021, week == 3) %>%
  mutate(pred_ppr_points = predict(lm_mod, newdata = .)) %>% View()

games <- nflreadr::load_schedules()

# join in snaps
snaps_aug <- snaps %>%
  inner_join(rb_wr_te_key %>%
               distinct(gsis_id, pfr_id),
             by = "pfr_id") %>%
  left_join(games %>%
              select(game_id, season, week),
            by = c("game_id", "season")) %>%
  inner_join(rb_wr_te_shares,
             by = c("season", "week", "team", "gsis_id" = "player_id"))

