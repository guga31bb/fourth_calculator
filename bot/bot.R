source('R/helpers.R')
source('bot/bot_functions.R')

# get live games
live_games <- readRDS(url(
  "http://www.habitatring.com/games_alt.rds"
  # "https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true"
)) %>% 
  filter(
    season == 2020,
    is.na(result),
    gameday == as.character(lubridate::today())
    ) %>%
  mutate(
    current_hour = lubridate::hour(lubridate::now()),
    current_minute = lubridate::minute(lubridate::now()),
    game_hour = as.integer(substr(gametime, 1, 2)),
    game_minute = as.integer(substr(gametime, 4, 5)),
    started = case_when(
      current_hour > game_hour ~ 1,
      current_hour == game_hour & current_minute > game_minute + 5 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  filter(started == 1) %>%
  select(game_id, espn, home_team, away_team, week)

# for testing
live_games <- readRDS(url(
  "http://www.habitatring.com/games_alt.rds"
  # "https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true"
)) %>% 
  filter(
    season == 2020,
    week == 1
    # gameday == as.character(lubridate::today())
  ) %>%
  head(2) %>%
  select(game_id, espn, home_team, away_team, week)

if (nrow(live_games) > 0) {
  
  # get list of old plays before we do anything
  old_plays <- readRDS("bot/old_plays.rds") %>%
    select(game_id, index) %>%
    mutate(old = 1)
  
  # get updated plays from ongoing games
  plays <- map_df(1 : nrow(live_games), function(x) {
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
  
  if (nrow(for_tweeting) > 0) {
    
    # do the thing
    for (x in 1 : nrow(for_tweeting)) {
      tweet_play(for_tweeting %>% dplyr::slice(x))
    }
    
  }
  
}






