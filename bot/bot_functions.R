`%>%`<-magrittr::`%>%`

# function to tweet out one play
tweet_play <- function(df, n_games) {
  
  seconds = df$quarter_seconds_remaining %% 60
  mins = (df$quarter_seconds_remaining / 60) |> floor()
  time = glue::glue("Q{df$qtr} {hms::hms(df$quarter_seconds_remaining) %>% substr(4, 8)}") |> as.character()

  tableData <- df %>%
    nfl4th::add_4th_probs() %>%
    nfl4th::make_table_data() %>%
    arrange(-choice_prob)

  # if you're outside the 50, don't show field goal
  if (df$yardline_100 > 50) {
    tableData <- tableData %>%
      dplyr::filter(choice != "Field goal attempt")
  }

  # if inside 35, don't show punt
  if (df$yardline_100 < 35) {
    tableData <- tableData %>%
      dplyr::filter(choice != "Punt")
  }

  play_desc <- df$desc %>%
    stringr::str_replace("\\([:digit:]*\\:[:digit:]+\\)\\s", "") %>%
    substr(1, 100)

  choice_emoji <- dplyr::case_when(
    # football to punt
    df$type_text %in% c("Blocked Punt", "Punt") ~ "\U0001f3c8\U0001f9B5",
    # field goal
    df$type_text %in% c("Field Goal Good", "Field Goal Missed") ~ "\U0001f45F\U0001f3c8",
    # go for it
    df$type_text %in% c("Pass Incompletion", "Pass Reception", "Passing Touchdown", "Rush", "Rushing Touchdown", "Sack") ~ "\U0001f449",
    # penalty
    df$type_text %in% c("Penalty") ~ "\U0001f6A8",
    TRUE ~ ""
  )

  # wp1 = preferred choice
  # wp2 = second choice
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
    abs(diff) >= 1 & abs(diff) < 2.5 ~ "(MEDIUM)",
    abs(diff) >= 2.5 & abs(diff) <= 5 ~ "(STRONG)",
    abs(diff) >= 5 & abs(diff) <= 10 ~ "(VERY STRONG)",
    abs(diff) > 10 ~ "(YOU BETTER DO THIS)"
  )

  confidence <- if_else(
    confidence == "(MEDIUM)" & (abs(wp1 / wp2) > 1.15 | (100 - wp2) / (100 - wp1) > 1.15),
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

  # chromote::set_chrome_args("--disable-crash-reporter")

  text <-
    glue::glue(
      "
  ---> {df$away_team} ({df$away_score}) @ {df$home_team} ({df$home_score}) <---
  {posteam} has 4th & {df$ydstogo} {position}, {time}

  Recommendation {confidence}: {rec_emoji} {choice} (+{round(diff, 1)} WP)
  Actual play: {choice_emoji} {play_desc}
  ")
  
  sink("bot/text.txt")
  cat(text)
  sink()
  
  log_text <-
    glue::glue(
      "{df$away_team} ({df$away_score}) @ {df$home_team} ({df$home_score}) | {posteam} 4th & {df$ydstogo} {position}, {time}
      {confidence}: {choice} (+{round(diff, 1)} WP)
      ")
  
  tweet_me <- 0
  
  # tweet if should go for it
  if (choice == "Go for it" & confidence %in% c("(STRONG)", "(VERY STRONG)", "(YOU BETTER DO THIS)")) {
    tweet_me <- 1 
  }
  
  # tweet all plays from days with 2 or fewer games (rate limit doesn't matter)
  if (n_games <= 2) {
    tweet_me <- 1
  }

  if (tweet_me == 1) {
    table <- make_table(tableData, df)
    table %>% gtsave("bot/post.png")
    system(glue::glue("python3 ../box_scores/tweet.py"))
    message("Tweet posted!")
  } else {
      message(glue::glue("Skipping play: {log_text}"))
  }

}


