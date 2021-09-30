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




