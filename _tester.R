# ##################################################################################################
# main app

source('R/helpers.R')
library(shiny)
library(shinythemes)

# get teams and logos
teams <- nflfastR::teams_colors_logos %>%
  filter(!team_abbr %in% c("LAR", "SD", "STL", "OAK"))

# team abbreviations for dropdown menus
ids <- teams %>%
  pull(team_abbr)

# uncomment only for testing
input <- NULL
input$qtr <- 5
input$mins <- 3
input$secs <- 22
input$posteam <- "LV"
input$away <- "LAC"
input$home <- "LV"
input$yardline <- 5
input$ydstogo <- 5
input$posteam_to <- 2
input$defteam_to <- 1
input$home_ko <- 0
input$first_ot_drive <- 1
input$score_diff <- 0
input$type <- "reg"
input$runoff <- 0
input$season <- 2020

df <-       tibble::tibble(
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
  'first_ot_drive' = as.integer(input$first_ot_drive),
  'score_differential' = as.integer(input$score_diff),
  'runoff' = as.integer(input$runoff),
  'yr' = as.integer(input$season)
) %>%
  prepare_df(games)


df

get_fg_wp(df)

go <- get_go_wp(df)
go




# ##################################################################################################
# bot

source('bot/bot_functions.R')

# get live games
live_games <- readRDS(url(
  "http://www.habitatring.com/games_alt.rds"
  # "https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true"
)) %>% 
  dplyr::filter(
    
    # this probably isn't necessary but whatever
    season == 2019
    
    # hasn't finished yet
    # is.na(result),
    
    # happening today
    # gameday == as.character(lubridate::today())
    
  ) %>%
  dplyr::mutate(
    # there's probably a better way to do this but it seems to work
    current_hour = lubridate::hour(lubridate::now()),
    current_minute = lubridate::minute(lubridate::now()),
    game_hour = as.integer(substr(gametime, 1, 2)),
    game_minute = as.integer(substr(gametime, 4, 5)),
    # has already started
    started = dplyr::case_when(
      current_hour > game_hour ~ 1,
      current_hour == game_hour & current_minute >= game_minute + 5 ~ 1,
      TRUE ~ 0
    ),
    espn = dplyr::case_when(
      game_id == "2020_07_PIT_TEN" ~ "401249063",
      game_id == "2020_10_WAS_DET" ~ "401220289",
      TRUE ~ espn
    )
  ) %>%
  # dplyr::filter(started == 1) %>%
  dplyr::select(game_id, espn, home_team, away_team, week)

live_games <- live_games %>%
  filter(home_team == "SF", week == 10)

df <- live_games

# get updated plays from ongoing games
for_tweeting <- map_df(1 : nrow(live_games), function(x) {
  message(glue::glue("{x}: game {live_games %>% dplyr::slice(x) %>% pull(game_id)}"))
  get_data(live_games %>% dplyr::slice(x))
})

# see the plays lined up
for_tweeting

x = 16

df <- for_tweeting %>% dplyr::slice(x)

df

fullInput <- df %>% 
  prepare_df(games)

tableData <- make_table_data(fullInput, punt_df) %>%
  arrange(-choice_prob)

play_desc <- df$desc %>%
  stringr::str_replace("\\([:digit:]*\\:[:digit:]+\\)\\s", "") %>%
  substr(1, 80)

choice_emoji <- dplyr::case_when(
  # football to punt
  fullInput$type_text %in% c("Blocked Punt", "Punt") ~ "\U0001f3c8\U0001f9B5",
  # field goal
  fullInput$type_text %in% c("Field Goal Good", "Field Goal Missed") ~ "\U0001f45F\U0001f3c8",
  # go for it
  fullInput$type_text %in% c("Pass Incompletion", "Pass Reception", "Passing Touchdown", "Rush", "Rushing Touchdown", "Sack") ~ "\U0001f449",
  # penalty
  fullInput$type_text %in% c("Penalty") ~ "\U0001f6A8",
  TRUE ~ ""
)


wp1 <- tableData %>% dplyr::slice(1) %>% pull(choice_prob)
wp2 <- tableData %>% dplyr::slice(2) %>% pull(choice_prob)

diff <- wp1 - wp2
choice <- tableData %>% dplyr::slice(1) %>% pull(choice)
choice <- if_else(abs(diff) < 1, "Toss-up", choice)

rec_emoji <- dplyr::case_when(
  choice == "Go for it" ~ "\U0001f449",
  choice == "Field goal attempt" ~ "\U0001f45F\U0001f3c8",
  choice == "Punt" ~ "\U0001f3c8\U0001f9B5",
  choice == "Toss-up" ~ "\U0001f937"
)

confidence <- case_when(
  abs(diff) < 1 ~ "",
  abs(diff) >= 1 & abs(diff) < 3 ~ "(MEDIUM)",
  abs(diff) >= 3 & abs(diff) <= 5 ~ "(STRONG)",
  abs(diff) >= 5 & abs(diff) <= 10 ~ "(VERY STRONG)",
  abs(diff) > 10 ~ "(YOU BETTER DO THIS)"
)

confidence <- if_else(
  confidence == "(MEDIUM)" & abs(wp1 / wp2) > 1.2,
  "(STRONG)",
  confidence
)

position <- if_else(
  !is.na(df$yardline), 
  glue::glue("at the {df$yardline}"),
  glue::glue("{df$yardline_100} yards from opponent end zone")
)

posteam <- df$posteam
defteam <- if_else(df$posteam == df$home_team, df$away_team, df$home_team)

table <- make_table(tableData, fullInput)

text <- 
  glue::glue(
    "
---> {df$away_team} ({df$away_score}) @ {df$home_team} ({df$home_score}) <---
{posteam} has 4th & {df$ydstogo} {position}
             
Recommendation {confidence}: {rec_emoji} {choice} (+{round(diff, 1)} WP)
Actual play: {choice_emoji} {play_desc}
")

text

table
