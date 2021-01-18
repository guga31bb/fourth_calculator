message(glue::glue("------------------------------------------{lubridate::now()}"))

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
    ),
    espn = dplyr::case_when(
      game_id == "2020_07_PIT_TEN" ~ "401249063",
      game_id == "2020_10_WAS_DET" ~ "401220289",
      # wc playoffs
      game_id == "2020_18_IND_BUF" ~ "401220393",
      game_id == "2020_18_LA_SEA"  ~ "401220372",
      game_id == "2020_18_TB_WAS"  ~ "401220371",
      game_id == "2020_18_BAL_TEN" ~ "401220396",
      game_id == "2020_18_CHI_NO"  ~ "401220395",
      game_id == "2020_18_CLE_PIT" ~ "401220394",
      # div playoffs
      game_id == "2020_19_LA_GB"   ~ "401220398",
      game_id == "2020_19_BAL_BUF" ~ "401220397",
      game_id == "2020_19_CLE_KC"  ~ "401220400",
      game_id == "2020_19_TB_NO"   ~ "401220399",
      TRUE ~ espn
      )
    ) %>%
  dplyr::filter(started == 1) %>%
  dplyr::select(game_id, espn, home_team, away_team, week)

# for testing
# live_games <- readRDS(url(
#   "http://www.habitatring.com/games_alt.rds"
#   # "https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true"
# )) %>%
#   dplyr::filter(
#     season == 2020,
#     week == 7,
#     !is.na(result)
#   ) %>%
#   # head(10) %>%
#   dplyr::select(game_id, espn, home_team, away_team, week)

if (nrow(live_games) > 0) {
  
  source('R/helpers.R')
  
  # get list of old plays before we do anything
  if (file.exists("bot/old_plays.rds")) {
    
    # read the file if it exists
    old_plays <- readRDS("bot/old_plays.rds")
    
    # if it's just an empty df, just make an empty one
    if (!"game_id" %in% names(old_plays)) {
      old_plays <- tibble::tibble(
        "game_id" = as.character("XXXXXX"),
        "index" = as.integer(0),
        "old" = as.integer(1)
      )
    # if it already exists, take game id and index
    } else {
      old_plays <- old_plays %>%
        dplyr::select(game_id, index) %>%
        dplyr::mutate(old = 1)
    }
  } else {
    # this is so we can remove the file if we want to start over
    old_plays <- tibble::tibble(
      "game_id" = as.character("XXXXXX"),
      "index" = as.integer(0),
      "old" = as.integer(1)
    )
  }
  

  
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
    
    suppressMessages(
      library(rtweet)
    )
    
    # do the thing
    for (x in 1 : nrow(for_tweeting)) {
      tweet_play(for_tweeting %>% dplyr::slice(x))
    }
    
  }
  
}






