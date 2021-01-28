# a way to quickly test the functions

source('R/helpers.R')

input <- NULL
input$qtr <- 2
input$mins <- 0
input$secs <- 3
input$posteam <- "NYJ"
input$away <- "MIA"
input$home <- "NYJ"
input$yardline <- 11
input$ydstogo <- 1
input$posteam_to <- 3
input$defteam_to <- 3
input$home_ko <- 1
input$score_diff <- -10
input$type <- "reg"
input$runoff <- 0
input$season <- 2020

# get the situation from user input
df <- 
    tibble::tibble(
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

