`%>%`<-magrittr::`%>%`

# get all the 4th downs for a game
# with thanks to espn for the api
get_data <- function(df) {
  
  espn_game_id <- df$espn
  home <- df$home_team
  away <- df$away_team
  week <- df$week
  
  plays <- data.frame()
  
  tryCatch(
    expr = {
      
      
      pbp <- httr::GET(url = glue::glue("http://site.api.espn.com/apis/site/v2/sports/football/nfl/summary?event={espn_game_id}")) %>%
        httr::content(as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(flatten = TRUE) 
      
      if ("code" %in% names(pbp)) {
        warning(warn <- 1)
      }
      
      # get plays out of the drives lists
      # i think current drive duplicates a drive in previous drive so might be ok to cut this
      if ("current" %in% names(pbp$drives) & "previous" %in% names(pbp$drives)) {
        current_drive <- pbp$drives$current
        current_drive <- current_drive[['plays']] %>% bind_rows() %>% as_tibble() %>% mutate(team.abbreviation = current_drive$team$abbreviation)
        
        previous_drives <- pbp$drives$previous
        
        drives <- bind_rows(
          previous_drives %>% select(team.abbreviation, plays) %>% unnest(plays),
          current_drive
        )
      } else if ("current" %in% names(pbp$drives)) {
        current_drive <- pbp$drives$current
        drives <- current_drive[['plays']] %>% bind_rows() %>% as_tibble() %>% mutate(team.abbreviation = current_drive$team$abbreviation)
      } else {
        previous_drives <- pbp$drives$previous
        drives <- previous_drives %>% select(team.abbreviation, plays) %>% unnest(plays)
      }
      
      suppressWarnings(
        
        plays <- drives %>%
          as_tibble() %>%
          group_by(id) %>%
          dplyr::slice(1) %>%
          ungroup() %>%
          janitor::clean_names() %>%
          dplyr::rename(
            posteam = team_abbreviation,
            qtr = period_number,
            yardline_100 = start_yards_to_endzone,
            yardline = start_possession_text,
            down = start_down,
            ydstogo = start_distance,
            desc = text,
            time = clock_display_value
          ) %>% 
          dplyr::filter(qtr <= 4) %>%
          dplyr::mutate(
            # time column is wacky so extract it from play description when possible
            play_time = stringr::str_extract(desc, "\\([^()]+(?=\\)\\s)"),
            play_time = substr(play_time, 2, nchar(play_time)),
            play_min = stringr::str_extract(play_time, "[^()]+(?=\\:)") %>% as.integer(),
            play_min = if_else(is.na(play_min) & !is.na(play_time), as.integer(0), play_min),
            play_sec = substr(play_time, nchar(play_time) - 1, nchar(play_time)) %>% as.integer(),
            mins = if_else(nchar(time) == 5, substr(time, 1, 2), substr(time, 1, 1)) %>% as.integer(),
            secs = if_else(nchar(time) == 5, substr(time, 4, 5), substr(time, 3, 4)) %>% as.integer(),
            mins = if_else(is.na(play_min), mins, play_min),
            secs = if_else(is.na(play_sec), secs, play_sec)
          ) %>%
          arrange(qtr, desc(mins), desc(secs), id) %>%
          dplyr::mutate(
            home_team = home,
            away_team = away,
            posteam = case_when(
              posteam == "WSH" ~ "WAS",
              posteam == "LAR" ~ "LA",
              TRUE ~ posteam
            ),
            defteam = if_else(posteam == home_team, away_team, home_team),
            half = if_else(qtr <= 2, 1, 2),
            timeout_team = stringr::str_extract(desc, "(?<=Timeout #[:digit:] by )[:upper:]{2,3}"),
            timeout_team = case_when(
              timeout_team == "WSH" ~ "WAS",
              timeout_team == "LAR" ~ "LA",
              timeout_team == "ARZ" ~ "ARI",
              timeout_team == "BLT" ~ "BAL",
              timeout_team == "CLV" ~ "CLE",
              timeout_team == "HST" ~ "HOU",
              TRUE ~ timeout_team
            ),
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
          arrange(qtr, desc(mins), desc(secs), id) %>%
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
            home_score = dplyr::lag(home_score),
            away_score = dplyr::lag(away_score),
            score_differential = if_else(posteam == home_team, home_score - away_score, away_score - home_score),
            runoff = 0,
            yr = 2020,
            home_opening_kickoff = if_else(dplyr::first(na.omit(posteam)) == home_team, 1, 0),
            week = week,
            type = if_else(week <= 17, "reg", "post")
          ) %>%
          filter(
            # testing. uncomment these
            down == 4, 
            !(time < 30 & qtr %in% c(2, 4)),
            is.na(timeout_team),
            type_text != "Two-minute warning",
            type_text != "End Period"
          ) %>%
          group_by(qtr, time, ydstogo) %>%
          dplyr::slice(1) %>%
          ungroup() %>%
          arrange(qtr, desc(time), ydstogo) %>%
          mutate(
            game_id = df$game_id,
            yardline_side = purrr::map_chr(
              stringr::str_split(yardline, " "),
              function(x) x[1]
            ),
            yardline_side = case_when(
              yardline_side == "WSH" ~ "WAS",
              yardline_side == "LAR" ~ "LA",
              TRUE ~ yardline_side
            ),
            yardline_number = as.numeric(purrr::map_chr(
              stringr::str_split(yardline, " "),
              function(x) x[2]
            )),
            temp_yardline = dplyr::if_else(
              yardline_side == posteam | yardline_100 == 50,
              100 - yardline_number, 
              yardline_number
            ),
            yardline_100 = if_else(
              !is.na(temp_yardline), as.integer(temp_yardline), yardline_100
            )
          ) %>%
          select(
            desc,
            type,
            qtr,
            time,
            posteam,
            timeout_team,
            away_team,
            home_team,
            yardline_100,
            yardline,
            ydstogo,
            posteam_timeouts_remaining,
            defteam_timeouts_remaining,
            home_opening_kickoff,
            score_differential,
            runoff,
            home_score,
            away_score,
            type_text,
            yr
          )
      )
      
      
      if (nrow(plays) > 0) {
        plays <- plays %>%
          mutate(
            index = 1 : n()
          ) 
      } else {
        plays$index <- NA_real_
      }
      
    },
    error = function(e) {
      message("The following error has occured:")
      message(e)
    },
    warning = function(w) {
      if (warn == 1) {
        message(glue::glue("Warning: The requested GameID {espn_game_id} ({df$game_id}) is invalid!"))
      } 
    },
    finally = {
    }
    
  )

    return(plays)
}


# function to tweet out one play
tweet_play <- function(df) {
  fullInput <- df %>% 
    prepare_df(games)
  
  tableData <- make_table_data(fullInput, punt_df) %>%
    arrange(-choice_prob)
  
  wp1 <- tableData %>% dplyr::slice(1) %>% pull(choice_prob)
  wp2 <- tableData %>% dplyr::slice(2) %>% pull(choice_prob)
  
  diff <- wp1 - wp2
  choice <- tableData %>% dplyr::slice(1) %>% pull(choice)
  choice <- if_else(abs(diff) < 1, "Toss-up", choice)
  confidence <- case_when(
    abs(diff) < 1 ~ "LOW",
    abs(diff) >= 1 & abs(diff) < 3 ~ "MEDIUM",
    abs(diff) >= 3 ~ "HIGH"
  )
  position <- if_else(
    !is.na(df$yardline), 
    glue::glue("at the {df$yardline}"),
    glue::glue("{df$yardline_100} yards from opponent end zone")
  )
  
  posteam <- df$posteam
  defteam <- if_else(df$posteam == df$home_team, df$away_team, df$home_team)
  
  table <- make_table(tableData, fullInput)
  
  table %>% gtsave("bot/post.png")
  
  text <- 
    glue::glue(
      "
  ---> {df$away_team} ({df$away_score}) @ {df$home_team} ({df$home_score}) <---
  {posteam} has 4th & {df$ydstogo} {position}
               
  Correct choice: {choice}. Actual play: {df$type_text}
  Confidence level: {confidence} (+{round(diff, 1)} percentage points)
  ")
  
  # don't post if every choice is < 1 or > 99
  if (wp1 > 1 & wp2 > 1 & wp1 < 99 & wp2 < 99) {
    post_tweet(text, media = "bot/post.png")
  }
  # post_tweet(text)
  
}


