# a way to quickly test the functions

source('R/helpers.R')

input <- NULL
input$qtr <- 3
input$mins <- 9
input$secs <- 49
input$posteam <- "SEA"
input$away <- "SEA"
input$home <- "ATL"
input$yardline <- 38
input$ydstogo <- 5
input$posteam_to <- 3
input$defteam_to <- 3
input$home_ko <- 1
input$score_diff <- 2
input$type <- "reg"
input$runoff <- 0
input$season <- 2020

# get the situation from user input
df <- 
    tibble::tibble(
      
      # reg or post
      "type" = "post",
      
      "qtr" = 4,
      
      # since user input is mins and seconds
      "time" = 60 * as.integer(2) + as.integer(9),
      
      'posteam' = as.character("GB"),
      'away_team' = as.character("TB"),
      'home_team' = as.character("GB"),
      'yardline_100' = as.integer(8),
      'ydstogo' = as.integer(8),
      'posteam_timeouts_remaining' = as.integer(3),
      'defteam_timeouts_remaining' = as.integer(3),
      
      # did home team receive opening kickoff?
      'home_opening_kickoff' = as.integer(0),
      
      'score_differential' = as.integer(-8),
      
      # user input for additional runoff in seconds after successful 4th down conversion
      'runoff' = as.integer(0),
      
      # season that the game happened in
      'yr' = as.integer(2020)
    ) %>%
      # helper function to look up point spread and roof type for given game
      prepare_df(games)

# main function in R/helpers.R
tab <- make_table_data(df, punt_df)
tab 

make_table(tab, df)

# end of half: https://rbsdm.com/stats/fourth_calculator/?_inputs_&posteam_to=%220%22&posteam=%22ARI%22&home_ko=%221%22&mins=0&update=0&defteam_to=%221%22&qtr=%222%22&season=%222020%22&away=%22ARI%22&yardline=1&secs=3&score_diff=3&home=%22NE%22&ydstogo=1&runoff=0
input <- NULL
input$qtr <- 2
input$mins <- 0
input$secs <- 3
input$posteam <- "ARI"
input$away <- "ARI"
input$home <- "NE"
input$yardline <- 1
input$ydstogo <- 1
input$posteam_to <- 0
input$defteam_to <- 1
input$home_ko <- 1
input$score_diff <- 3
input$type <- "reg"
input$runoff <- 0
input$season <- 2020

df <- tibble::tibble(
  "type" = as.character(input$type),
  "qtr" = as.integer(input$qtr),
  "time" = 60 * as.integer(input$mins) + as.integer(input$secs),
  'posteam' = as.character(input$posteam),
  'away_team' = as.character(input$away),
  'home_team' = as.character(input$home),
  'yardline_100' = as.integer(input$yardline),
  'ydstogo' = as.integer(input$ydstogo),
  'posteam_timeouts_remaining' = as.integer(input$posteam_to),
  'defteam_timeouts_remaining' = as.integer(input$defteam_to),
  'home_opening_kickoff' = as.integer(input$home_ko),
  'score_differential' = as.integer(input$score_diff),
  'runoff' = as.integer(input$runoff),
  'yr' = as.integer(input$season)
) %>%
  prepare_df(games)

df

make_table_data(df, punt_df)


# end of half: https://rbsdm.com/stats/fourth_calculator/?_inputs_&posteam_to=%220%22&posteam=%22NYJ%22&home_ko=%221%22&type=%22reg%22&mins=0&update=0&defteam_to=%221%22&qtr=%222%22&season=%222020%22&away=%22MIA%22&yardline=11&secs=3&score_diff=-10&home=%22NYJ%22&ydstogo=1&runoff=0
input <- NULL
input$qtr <- 2
input$mins <- 0
input$secs <- 3
input$posteam <- "NYJ"
input$away <- "MIA"
input$home <- "NYJ"
input$yardline <- 11
input$ydstogo <- 1
input$posteam_to <- 0
input$defteam_to <- 1
input$home_ko <- 1
input$score_diff <- -10
input$type <- "reg"
input$runoff <- 0
input$season <- 2020



# https://rbsdm.com/stats/fourth_calculator/?_inputs_&posteam_to=%220%22&posteam=%22CLE%22&home_ko=%221%22&mins=0&update=0&defteam_to=%221%22&qtr=%222%22&season=%222020%22&away=%22PHI%22&yardline=65&secs=4&score_diff=7&home=%22CLE%22&ydstogo=5&runoff=0
input <- NULL
input$qtr <- 2
input$mins <- 0
input$secs <- 4
input$posteam <- "CLE"
input$away <- "PHI"
input$home <- "CLE"
input$yardline <- 65
input$ydstogo <- 5
input$posteam_to <- 0
input$defteam_to <- 1
input$home_ko <- 1
input$score_diff <- 7
input$type <- "reg"
input$runoff <- 0
input$season <- 2020


# https://rbsdm.com/stats/fourth_calculator/?_inputs_&posteam_to=%220%22&posteam=%22CLE%22&home_ko=%220%22&mins=0&update=0&defteam_to=%220%22&qtr=%222%22&season=%222020%22&away=%22PIT%22&yardline=71&secs=5&score_diff=4&home=%22CLE%22&ydstogo=16&runoff=0
input <- NULL
input$qtr <- 2
input$mins <- 0
input$secs <- 5
input$posteam <- "CLE"
input$away <- "PIT"
input$home <- "CLE"
input$yardline <- 71
input$ydstogo <- 16
input$posteam_to <- 0
input$defteam_to <- 0
input$home_ko <- 0
input$score_diff <- 4
input$type <- "reg"
input$runoff <- 0
input$season <- 2020

# helper function that prepares data and most importantly creates indicator for 2nd half kickoff team
source("R/season_numbers_functions.R")
pbp <- readRDS(url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds")) %>%
  prepare_data() %>%
  filter(week == 20, posteam == "GB", play_id == 3728)

make_table_data(df, punt_df)


df <- readRDS(url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2016.rds")) %>%
  dplyr::group_by(game_id) %>%
  dplyr::mutate(
    # needed for WP model
    # this is actually used as "home opening kickoff" but named badly
    home_opening_kickoff = dplyr::if_else(home_team == dplyr::first(stats::na.omit(posteam)), 1, 0),
    # because games df looks for "yr" and not "season"
    type = if_else(week <= 17, "reg", "post"),
    runoff = 0
  ) %>%
  ungroup() %>%
  filter(is.na(down), posteam == "SEA", home_team == "NE", qtr == 4) %>%
  select(game_id, play_id, desc, season, posteam, type, home_team, away_team, qtr, quarter_seconds_remaining, ydstogo, yardline_100, 
         posteam_timeouts_remaining, defteam_timeouts_remaining, score_differential, home_opening_kickoff, runoff) %>%
  prepare_df()

current_situation <- df %>% dplyr::slice(2)

make_2pt_table_data(current_situation)
make_table_2pt(make_2pt_table_data(current_situation), current_situation)



library(tidyverse)
df <- readRDS(url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds")) %>%
  dplyr::group_by(game_id) %>%
  dplyr::mutate(
    # needed for WP model
    # this is actually used as "home opening kickoff" but named badly
    home_opening_kickoff = dplyr::if_else(home_team == dplyr::first(stats::na.omit(posteam)), 1, 0),
    # because games df looks for "yr" and not "season"
    type = if_else(week <= 17, "reg", "post"),
    runoff = 0
  ) %>%
  ungroup() %>%
  filter(down == 4, game_seconds_remaining > 10,
         !is.na(half_seconds_remaining), !is.na(qtr), !is.na(posteam)) %>%
  select(game_id, play_id, desc, season, posteam, type, home_team, away_team, qtr, quarter_seconds_remaining, ydstogo, yardline_100, 
         posteam_timeouts_remaining, defteam_timeouts_remaining, score_differential, home_opening_kickoff, runoff) %>%
  prepare_df()

pbp <- df %>% dplyr::slice(2)
pbp <- df

# prob  gain vegas_wp    wt_wp
# <dbl> <int>    <dbl>    <dbl>
#   1 0.0420    -10    0.172 0.00724

# (10:17) 4-A.Lee punts 53 yards to SF 12, Center-46-A.Brewer. 15-T.Taylor to SF 24 for 12 yards (45-D.Gard~                 1517

source("R/_helpers_redo.R")

current_situation <- df %>%
  filter(game_id == "2020_15_SF_DAL", play_id == 2083)

make_table_data(current_situation)
make_table(make_table_data(current_situation), current_situation)


pbp %>%
  dplyr::slice(1:2) %>%
  get_go_wp() %>%
  get_fg_wp() %>%
  get_punt_wp() %>%
  select(
    punt_wp,
    go_wp, first_down_prob, wp_fail, wp_succeed,
    fg_wp, miss_fg_wp, make_fg_wp
  ) %>%
  dplyr::slice(2)


new <- df %>%
  get_go_wp() %>%
  get_fg_wp() %>%
  get_punt_wp() %>%
  mutate(play_no = 1 : n()) %>%
  group_by(play_no) %>%
  mutate(
    punt_wp = if_else(is.na(punt_wp), 0, punt_wp),
    max_non_go = max(fg_wp, punt_wp, na.rm = T)
  ) %>%
  ungroup() %>%
  select(-play_no) %>%
  mutate(
    go_boost_new = go_wp - max_non_go,
  ) %>%
  select(game_id, play_id, go_boost_new)

old <- readRDS("data/current_season_decisions.rds") %>%
  select(game_id, play_id, go_boost, desc)


new %>%
  mutate(go_boost_new)
  left_join(old, by = c("game_id", "play_id")) %>%
  mutate(diff = 100 * go_boost_new - go_boost) %>%
  arrange(-diff)



# testing

df <- tibble::tibble(
  "type" = as.character(input$type),
  "qtr" = as.integer(input$qtr),
  "quarter_seconds_remaining" = 60 * as.integer(input$mins) + as.integer(input$secs),
  'posteam' = as.character(input$posteam),
  'away_team' = as.character(input$away),
  'home_team' = as.character(input$home),
  'yardline_100' = as.integer(input$yardline),
  'ydstogo' = as.integer(input$ydstogo),
  'posteam_timeouts_remaining' = as.integer(input$posteam_to),
  'defteam_timeouts_remaining' = as.integer(input$defteam_to),
  'home_opening_kickoff' = as.integer(input$home_ko),
  'score_differential' = as.integer(input$score_diff),
  'runoff' = as.integer(input$runoff),
  'season' = as.integer(input$season)
)


pbp <- df %>%
  prepare_df() %>%
  mutate(runoff = 0)
pbp

pbp %>%
  # mutate(yardline_100 = 50) %>%
  get_punt_wp() %>%
  select(punt_wp)

pbp %>%
  # mutate(yardline_100 = 50) %>%
  get_fg_wp() %>%
  select(prob, make_fg_wp, miss_fg_wp, fg_wp)



pbp %>%
  # mutate(yardline_100 = 50) %>%
  get_2pt_wp()