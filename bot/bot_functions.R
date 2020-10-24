library(rtweet)

if (grepl("Documents",getwd())){
  data_path <- "../"
} else { ### server
  data_path <- "/home/ben/data"
}


# get all the 4th downs for a game
get_data <- function(df) {
  
  espn_game_id <- df$espn
  home <- df$home_team
  away <- df$away_team
  week <- df$week

  pbp <- httr::GET(url = glue::glue("http://site.api.espn.com/apis/site/v2/sports/football/nfl/summary?event={espn_game_id}")) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE) 
  
  drives <- pbp$drives$previous
  
  plays <- drives %>%
    select(team.abbreviation, plays) %>%
    unnest(plays) %>%
    janitor::clean_names() %>%
    dplyr::rename(
      posteam = team_abbreviation,
      qtr = period_number,
      yardline_100 = start_yards_to_endzone,
      down = start_down,
      ydstogo = start_distance,
      desc = text,
      time = clock_display_value
    ) %>% 
    dplyr::filter(qtr <= 4) %>%
    dplyr::mutate(
      home_team = home,
      away_team = away,
      defteam = if_else(posteam == home_team, away_team, home_team),
      half = if_else(qtr <= 2, 1, 2),
      mins = if_else(nchar(time) == 5, substr(time, 1, 2), substr(time, 1, 1)),
      secs = if_else(nchar(time) == 5, substr(time, 4, 5), substr(time, 3, 4)),
      timeout_team = stringr::str_extract(desc, "(?<=Timeout #[:digit:] by )[:upper:]{2,3}"),
      home_timeout_used = case_when(
        timeout_team == home_team ~ 1,
        timeout_team != home_team ~ 0,
        is.na(timeout_team) ~ 0
      ),
      away_timeout_used = case_when(
        timeout_team == away_team ~ 1,
        timeout_team != away_team ~ 0,
        is.na(timeout_team) ~ 0
      ),
      home_timeouts_remaining = 3,
      away_timeouts_remaining = 3
    ) %>%
    dplyr::group_by(half) %>%
    dplyr::mutate(
      total_home_timeouts_used = dplyr::if_else(cumsum(home_timeout_used) > 3, 3, cumsum(home_timeout_used)),
      total_away_timeouts_used = dplyr::if_else(cumsum(away_timeout_used) > 3, 3, cumsum(away_timeout_used))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      home_timeouts_remaining = home_timeouts_remaining - total_home_timeouts_used,
      away_timeouts_remaining = away_timeouts_remaining - total_away_timeouts_used,
      posteam_timeouts_remaining = dplyr::if_else(
        posteam == home_team,
        home_timeouts_remaining,
        away_timeouts_remaining
      ),
      defteam_timeouts_remaining = dplyr::if_else(
        defteam == home_team,
        home_timeouts_remaining,
        away_timeouts_remaining
      ),
      time = 60 * as.integer(mins) + as.integer(secs),
      score_differential = if_else(posteam == home_team, home_score - away_score, away_score - home_score),
      runoff = 0,
      yr = 2020,
      home_opening_kickoff = if_else(dplyr::first(na.omit(posteam)) == home_team, 1, 0),
      week = week,
      type = if_else(week <= 17, "reg", "post")
    ) %>%
    filter(down == 4) %>%
    group_by(qtr, time, ydstogo) %>%
    dplyr::slice(1) %>%
    ungroup() %>%
    select(
      desc,
      type,
      qtr,
      time,
      posteam,
      away_team,
      home_team,
      yardline_100,
      ydstogo,
      posteam_timeouts_remaining,
      defteam_timeouts_remaining,
      home_opening_kickoff,
      score_differential,
      runoff,
      yr
    ) %>%
    arrange(qtr, desc(time)) %>%
    mutate(
      index = 1 : n(),
      game_id = df$game_id
      ) %>%
    return()
}


# function to tweet out one play
tweet_play <- function(df) {
  fullInput <- df %>% 
    prepare_df(games)
  
  tableData <- make_table_data(fullInput, punt_df) %>%
    arrange(-choice_prob)
  
  diff <- tableData %>% dplyr::slice(1) %>% pull(choice_prob) - tableData %>% dplyr::slice(2) %>% pull(choice_prob)
  choice <- tableData %>% dplyr::slice(1) %>% pull(choice)
  choice <- if_else(abs(diff) < 1, "Toss-up", choice)
  confidence <- case_when(
    abs(diff) < 1 ~ "low",
    abs(diff) >= 1 & abs(diff) < 3 ~ "medium",
    abs(diff) >= 3 ~ "high"
  )
  
  posteam <- df$posteam
  defteam <- if_else(df$posteam == df$home_team, df$away_team, df$home_team)
  
  table <- make_table(tableData, fullInput)
  
  table %>% gtsave("bot/post.png")
  
  text <- 
    glue::glue(
      "
  {posteam} has 4th & {df$ydstogo}, {df$yardline_100} yards from end zone
               
  Correct choice: {choice} 
  
  Confidence level: {confidence}, difference = {round(diff, 1)}%
  
  ")
  
  post_tweet(text, media = "bot/post.png")
  
}


