library(splines)

# predicted go for it model
load('data/observed_go_model.Rdata', .GlobalEnv)


add_pred_go <- function(data) {
  
  data$pred_go <- predict(fit_fourth, data %>% prepare_glm(), type="response")
  
  return(data)
  
}

# for cleaning after getting the probs
clean_data <- function(fourth_downs) {
  
  fourth_downs %>%
    mutate(play_no = 1 : n()) %>%
    group_by(play_no) %>%
    mutate(
      punt_prob = if_else(is.na(punt_wp), 0, punt_wp),
      max_non_go = max(fg_wp, punt_prob, na.rm = T),
      mins = quarter_seconds_remaining %/% 60,
      seconds = quarter_seconds_remaining %% 60
    ) %>%
    ungroup() %>%
    select(-play_no) %>%
    mutate(
      go_boost = go_wp - max_non_go,
      should_go = if_else(go_boost > 0, 1, 0),
      go_boost = go_boost * 100
    ) %>%
    select(
      game_id, play_id, series, fixed_drive, season, week, pred_go, score_differential, prior_wp = vegas_wp, posteam, defteam, home_team, away_team, desc, play_type, go_boost, go, series_go, should_go, yardline_100, ydstogo, qtr, mins, seconds, quarter_seconds_remaining
    ) %>%
    return()
}

# wrapper function to get 4th down numbers for a season
# works for 2014-present
load_season <- function(s, rebuild = FALSE) {
  
  # read previously-saved plays if they exist and user doesn't want to start over ("rebuild")
  if (file.exists(glue::glue("data/decisions_{s}.rds")) & rebuild == FALSE) {
    message(glue::glue("Loading existing plays for {s}"))
    existing_plays <- readRDS(glue::glue("data/decisions_{s}.rds"))
  } else {
    message("No existing plays found for this season. Starting over...")

    existing_plays <- nflfastR::load_pbp(s) %>%
      prepare_nflfastr_data() %>%
      prepare_df() %>%
      add_probs() %>% 
      add_pred_go() %>%
      clean_data()
    
    saveRDS(existing_plays, glue::glue("data/decisions_{s}.rds"))
  }
  
  return(existing_plays)
  
}


# wrapper function to update 4th down numbers for an ongoing season
update_season <- function(s) {
  
  # read previously-saved plays if they exist
  if (file.exists(glue::glue("data/decisions_{s}.rds"))) {
    message(glue::glue("Loading existing plays for {s}"))
    existing_plays <- readRDS(glue::glue("data/decisions_{s}.rds"))
  } else {
    message("No existing plays found for this season. Starting over...")
    existing_plays <- tibble::tibble(
      "game_id" = "XXX",
      "play_id" = 0
    )
  }
  
  # get new plays that haven't been saved
  plays <- nflfastR::load_pbp(s) %>%
    prepare_nflfastr_data() %>%
    left_join(
      existing_plays %>% select(game_id, play_id) %>% mutate(already_scraped = 1), by = c("game_id", "play_id")
    ) %>%
    filter(is.na(already_scraped))
  
  # if there are new plays, calculate decisions for them
  if (nrow(plays) > 0) {
    
    message(glue::glue("Calculating probabilities for {nrow(plays)} plays"))
    
    new_plays <- plays %>%
      prepare_df() %>%
      add_probs() %>% 
      add_pred_go() %>%
      clean_data()

    # add to existing plays
    cleaned <- bind_rows(
      existing_plays,
      new_plays
    ) %>%
      # this is to drop the dummy row created if file is missing above
      filter(!is.na(posteam))
    
    saveRDS(cleaned, glue::glue("data/decisions_{s}.rds"))
    
  } else {
    message("No new plays found.")
  }
  
}


# get data ready for predicted 4th down model
prepare_glm <- function(df) {
  
  df %>%
    mutate(
      # this is all completely arbitrary
      # but not actually used in the "should go for it" model so YOLO
      # this is only used in some figures looking at when teams go versus when
      # they would be expected to go based on historical team behavior
      bin_ytg = case_when(
        ydstogo == 1 ~ 1,
        ydstogo == 2 ~ 2,
        ydstogo == 3 ~ 3,
        between(ydstogo, 4, 6) ~ 4,
        between(ydstogo, 7, 10) ~ 5,
        ydstogo > 10 ~ 6
      ),
      bin_ytg = as_factor(bin_ytg),
      bin_scorediff = case_when(
        score_differential < -16 ~ 0,
        between(score_differential, -16, -9) ~ 1,
        between(score_differential, -8, -6) ~ 2,
        between(score_differential, -5, -4) ~ 3,
        between(score_differential, -3, -1) ~ 4,
        score_differential == 0 ~ 5,
        between(score_differential, 1, 3) ~ 6,
        between(score_differential, 4, 5) ~ 7,
        between(score_differential, 6, 8) ~ 8,
        between(score_differential, 9, 16) ~ 9,
        score_differential > 16 ~ 10
      ),
      bin_scorediff = as_factor(bin_scorediff),
      week = if_else(week > 17, 18, as.double(week))
    )
  
}
