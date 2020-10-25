source('bot/bot_functions.R')

# get live games
live_games <- readRDS(url(
  "http://www.habitatring.com/games_alt.rds"
  # "https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true"
)) %>% 
  dplyr::filter(
    
    # this probably isn't necessary but whatever
    season == 2020,
    
    # hasn't finished yet
    is.na(result),
    
    # happening today
    gameday == as.character(lubridate::today())
    
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
    )
  ) %>%
  dplyr::filter(started == 1, game_id !="2020_07_PIT_TEN") %>%
  dplyr::select(game_id, espn, home_team, away_team, week)

# for testing
# live_games <- readRDS(url(
#   "http://www.habitatring.com/games_alt.rds"
#   # "https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true"
# )) %>%
#   dplyr::filter(
#     season == 2020,
#     week == 6,
#     !is.na(result)
#   ) %>%
#   head(2) %>%
#   dplyr::select(game_id, espn, home_team, away_team, week)

if (nrow(live_games) > 0) {
  
  source('R/helpers.R')
  
  # get list of old plays before we do anything
  old_plays <- readRDS("bot/old_plays.rds") %>%
    dplyr::select(game_id, index) %>%
    dplyr::mutate(old = 1)
  
  # get updated plays from ongoing games
  plays <- map_df(1 : nrow(live_games), function(x) {
    message(glue::glue("{x}: game {live_games %>% dplyr::slice(x) %>% pull(game_id)}"))
    get_data(live_games %>% dplyr::slice(x))
  })
  
  # save updated list of plays we've done
  saveRDS(plays, "bot/old_plays.rds")
  
  # get plays we haven't tweeted yet
  for_tweeting <- plays %>%
    left_join(old_plays, by = c("game_id", "index")) %>%
    filter(is.na(old))
  
  # see the plays lined up
  for_tweeting
  
  # for testing: limited to a few tweets
  # for_tweeting <- for_tweeting %>% head(5)
  
  if (nrow(for_tweeting) > 0) {
    
    library(rtweet)
    
    # do the thing
    for (x in 1 : nrow(for_tweeting)) {
      tweet_play(for_tweeting %>% dplyr::slice(x))
    }
    
  }
  
}






