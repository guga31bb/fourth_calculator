library(tidyverse)
library(splines)
library(mgcv)

# for getting data ready for the model
source('R/season_numbers_functions.R')

# **************************************************************************************

seasons <- 2011:2020
cleaned_all <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  ) %>%
    filter(down == 4)
})

model_data <- cleaned_all %>%
  filter(
    qb_kneel == 0,
    !is.na(posteam),
    !is.na(yardline_100),
    !is.na(score_differential),
    play_type_nfl %in% c("RUSH", "PASS", "SACK", "FIELD_GOAL", "PUNT")
  ) %>%
  mutate(
    label = if_else(play_type_nfl %in% c("RUSH", "PASS", "SACK"), 1, 0),
    # treat all post the same
    week = if_else(week > 17, as.integer(18), as.integer(week))
  ) %>%
  select(
    # comment this out, testing only
    # desc,
    # game_id,
    # posteam,
    
    label,
    score_differential,
    week,
    qtr,
    ydstogo,
    yardline_100,
    half_seconds_remaining,
    game_seconds_remaining,
    season
  )

model_data

# fit the model
fit_fourth <- glm(label ~ (ns(game_seconds_remaining, 5) + bin_ytg + season + bin_scorediff +
                             ns(yardline_100, 5))^2 + week, 
                  data = model_data %>% prepare_glm(), family = "binomial")


save(fit_fourth, file = 'data/observed_go_model.Rdata')


