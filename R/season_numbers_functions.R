
# the function
get_probs <- function(p, games) {
  
  play_data <- tibble::tibble(
    "game_id" = p$game_id,
    "play_id" = p$play_id,
    "season" = p$season,
    "week" = p$week,
    "qtr" = p$qtr,
    "time" = p$quarter_seconds_remaining,
    'posteam' = p$posteam,
    'away_team' = p$away_team,
    'home_team' = p$home_team,
    'yardline_100' = p$yardline_100,
    'ydstogo' = p$ydstogo,
    'posteam_timeouts_remaining' = p$posteam_timeouts_remaining,
    'defteam_timeouts_remaining' = p$defteam_timeouts_remaining,
    'home_opening_kickoff' = p$receive_2h_ko,
    'score_differential' = p$score_differential,
    'runoff' = 0,
    'yr' = p$season,
    "desc" = p$desc,
    'play_type' = p$play_type_nfl,
    "type" = if_else(p$week <= 17, "reg", "post"),
    "go" = p$go,
    "prior_wp" = p$vegas_wp
  ) %>%
    prepare_df(games) 
  
  probs <- play_data %>%
    make_table_data(punt_df)
  
  return_probs <- tibble::tibble(
    "go_prob" = probs %>% filter(choice == "Go for it") %>% pull(choice_prob),
    "fg_prob" = probs %>% filter(choice == "Field goal attempt") %>% pull(choice_prob),
    "punt_prob" = probs %>% filter(choice == "Punt") %>% pull(choice_prob)
  )
  
  bind_cols(
    play_data,
    return_probs
  ) 
}

# prepare raw pbp data
prepare_data <- function(pbp) {
  
  # some prep
  pbp %>%
    dplyr::group_by(game_id) %>%
    dplyr::mutate(
      # needed for WP model
      receive_2h_ko = dplyr::if_else(home_team == dplyr::first(stats::na.omit(posteam)), 1, 0)
    ) %>%
    ungroup() %>%
    filter(down == 4, game_seconds_remaining > 30, 
           !is.na(half_seconds_remaining), !is.na(qtr), !is.na(posteam)) %>%
    mutate(go = rush + pass) %>%
    group_by(posteam, game_id, series) %>%
    mutate(go = max(go)) %>%
    dplyr::slice(1) %>%
    ungroup() %>%
    mutate_at(vars(home_team, away_team), funs(case_when(
      . %in% "JAC" ~ "JAX",
      . %in% "STL" ~ "LA",
      . %in% "LAR" ~ "LA",
      . %in% "SD" ~ "LAC",
      . %in% "OAK" ~ "LV",
      TRUE ~ .
    ))) %>%
    return()
}

# for cleaning after getting the probs
clean_data <- function(fourth_downs) {
  
  fourth_downs %>%
    mutate(play_no = 1 : n()) %>%
    group_by(play_no) %>%
    mutate(
      punt_prob = if_else(is.na(punt_prob), 0, punt_prob),
      max_non_go = max(fg_prob, punt_prob, na.rm = T),
      mins = time %/% 60,
      seconds = time %% 60,
      url = 
        glue::glue("https://rbsdm.com/stats/fourth_calculator/?_inputs_&posteam_to=%22{posteam_timeouts_remaining}%22&posteam=%22{posteam}%22&home_ko=%22{home_opening_kickoff}%22&mins={mins}&update=0&defteam_to=%22{defteam_timeouts_remaining}%22&qtr=%22{qtr}%22&season=%22{season}%22&away=%22{away_team}%22&yardline={yardline_100}&secs={seconds}&score_diff={score_differential}&home=%22{home_team}%22&ydstogo={ydstogo}&runoff=0")
    ) %>%
    ungroup() %>%
    select(-play_no) %>%
    mutate(
      go_boost = go_prob - max_non_go,
      should_go = if_else(go_boost > 0, 1, 0)
    ) %>%
    select(
      game_id, play_id, season, week, prior_wp, url, posteam, home_team, away_team, desc, play_type, go_boost, go, should_go, yardline_100, ydstogo, qtr, mins, seconds
    ) %>%
    return()
}

# wrapper function to get 4th down numbers for a season
# works for 2014-present
get_season <- function(s) {
  
  # get data
  plays <- readRDS(url(glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{s}.rds"))) %>%
    prepare_data()
  
  # add probs to data
  future::plan(multisession)
  cleaned <- furrr::future_map_dfr(1 : nrow(plays), function(x) {
    # only uncomment for debugging purposes
    # message(glue::glue("game {plays %>% dplyr::slice(x) %>% pull(game_id)} play {plays %>% dplyr::slice(x) %>% pull(play_id)}"))
    get_probs(plays %>% dplyr::slice(x), games)
  }) %>%
    clean_data()
  
  return(cleaned)
  
}


# wrapper function to get 4th down numbers for an ongoing season
# the reason for the separate function is storing a file of previously-obtained
# plays so that it doesn't have to run for the entire season every single time
get_current_season <- function(s) {
  
  # read previously-saved plays if they exist
  if (file.exists("data/current_season_decisions.rds")) {
    message("Loading existing plays for this season")
    existing_plays <- readRDS("data/current_season_decisions.rds")
  } else {
    message("No existing plays found for this season. Starting over...")
    existing_plays <- tibble::tibble(
      "game_id" = "XXX",
      "play_id" = 0
    )
  }
  
  # get new plays that haven't been saved
  plays <- readRDS(url(glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{s}.rds"))) %>%
    prepare_data() %>%
    left_join(
      existing_plays %>% select(game_id, play_id) %>% mutate(already_scraped = 1), by = c("game_id", "play_id")
    ) %>%
    filter(is.na(already_scraped))
  
  
  # if there are new plays, calculate decisions for them
  if (nrow(plays) > 0) {
    
    message(glue::glue("Need to calculate probabilities for {nrow(plays)} plays"))
    
    # add probs to data
    future::plan(multisession)
    cleaned <- furrr::future_map_dfr(1 : nrow(plays), function(x) {
      # only uncomment for debugging purposes
      # message(glue::glue("game {plays %>% dplyr::slice(x) %>% pull(game_id)} play {plays %>% dplyr::slice(x) %>% pull(play_id)}"))
      get_probs(plays %>% dplyr::slice(x), games)
    }) %>%
      clean_data()
    
    # add to existing plays
    cleaned <- bind_rows(
      existing_plays,
      cleaned
    ) %>%
      # this is to drop the dummy row created if file is missing above
      filter(!is.na(posteam))
    
    saveRDS(cleaned, "data/current_season_decisions.rds")
    
    return(cleaned)
    
  } else {
    message("Nothing to do. Returning already-saved data")
    return(existing_plays)
  }
  

  
  
}




