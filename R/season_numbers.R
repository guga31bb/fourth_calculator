library(tidyverse)
library(gt)
library(future)
source('R/helpers.R')
source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_nflscrapr_mutations.R")

# **************************************************************************************
# get list of plays

# for getting spreads
games <- readRDS(url("https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true")) %>%
  mutate(game_type = if_else(game_type == "REG", "reg", "post"))

# get data
pbp <- readRDS(url(glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds")))

# some prep
plays <- pbp %>%
  dplyr::group_by(game_id) %>%
  dplyr::mutate(
    receive_2h_ko = dplyr::if_else(home_team == dplyr::first(stats::na.omit(posteam)), 1, 0)
  ) %>%
  ungroup() %>%
  filter(down == 4, vegas_wp > .05, vegas_wp < .95, game_seconds_remaining > 60)

# the function
get_probs <- function(p, games) {
  
  play_data <- tibble::tibble(
    "game_id" = p$game_id,
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
    "type" = if_else(p$week <= 17, "reg", "post")
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

# add probs to data
future::plan(multisession)
fourth_downs <- furrr::future_map_dfr(1 : nrow(plays), function(x) {
  # message(glue::glue("game {plays %>% dplyr::slice(x) %>% pull(game_id)}"))
  get_probs(plays %>% dplyr::slice(x), games)
})

# **************************************************************************************
# manipulate list

cleaned <- fourth_downs %>%
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
    go = if_else(play_type %in% c("PASS", "RUSH", "SACK"), 1, 0),
    should_go = if_else(go_boost > 0, 1, 0)
  ) %>%
  arrange(go_boost) %>%
  filter(!(mins < 1 & qtr == 4)) %>%
  select(
    game_id, url, posteam, home_team, away_team, desc, play_type, go_boost, go, should_go, yardline_100, ydstogo, qtr, mins, seconds
  )


t <- cleaned %>%
  filter(
    play_type != "PENALTY"
    # go_boost > 1 | go_boost < -1
  ) %>%
  mutate(type = case_when(
    go_boost >= 4 ~ "Definitely go for it",
    go_boost > 1 & go_boost < 4 ~ "Probably go for it",
    go_boost >= -1 & go_boost <= 1 ~ "Toss-up",
    go_boost < -1 & go_boost > -4 ~ "Probably kick",
    go_boost <= -4 ~ "Definitely kick"
  )) %>%
  group_by(type) %>%
  summarize(go = mean(go), n = n()) %>%
  ungroup() %>%
  arrange(-go) %>%
  mutate(go = 100 * go) %>%
  gt() %>%
  cols_label(
    type = "Recommendation",
    go = "Went for it %",
    n = "Plays"
  ) %>%
  tab_style(
    style = cell_text(color = "black", weight = "bold"),
    locations = list(
      cells_row_groups(),
      cells_column_labels(everything())
    )
  ) %>% 
  tab_options(
    row_group.border.top.width = px(3),
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.top.color = "black",
    table.border.top.width = px(1),
    table.border.bottom.color = "white",
    table.border.bottom.width = px(1),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(2)
  ) %>%
  fmt_number(
    columns = vars(go), decimals = 0
  ) %>%
  cols_align(
    columns = 2:3,
    align = "center"
  ) %>% 
  tab_header(
    title = md(glue::glue("NFL team decision-making by go recommendation")),
    subtitle = md(glue::glue("2020, win probability between 5 and 95 percent"))
  ) %>%
  tab_source_note(md('**Notes**: "Definitely" recommendations are greater than 4 percentage point advantage,<br> "probably" 1-4 percentage points'))


t

t %>% gtsave("figures/team_behavior.png")


t <- cleaned %>%
  filter(
    play_type != "PENALTY",
    go == 0
  ) %>%
  mutate(
    defteam = if_else(posteam == home_team, away_team, home_team)
  ) %>%
  arrange(-go_boost) %>%
  head(5) %>%
  select(posteam, defteam, qtr, ydstogo, go_boost, desc) %>%
  gt() %>%
  cols_label(
    posteam = "Team",
    defteam = "Opp.",
    qtr = "Qtr",
    ydstogo = "Yds to go",
    desc = "Play",
    go_boost = "Go gain (%)"
  ) %>%
  tab_style(
    style = cell_text(color = "black", weight = "bold"),
    locations = list(
      cells_row_groups(),
      cells_column_labels(everything())
    )
  ) %>% 
  tab_options(
    row_group.border.top.width = px(3),
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.top.color = "black",
    table.border.top.width = px(1),
    table.border.bottom.color = "white",
    table.border.bottom.width = px(1),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(2)
    
  ) %>%
  fmt_number(
    columns = vars(go_boost), decimals = 1
  ) %>%
  cols_align(
    columns = 1:5,
    align = "center"
  ) %>% 
  tab_header(
    title = md(glue::glue("Worst kick decisions of 2020"))
  )

t

t %>% gtsave("figures/team_worst.png")

cleaned %>%
  filter(
    play_type != "PENALTY",
    go == 0
  ) %>%
  mutate(
    defteam = if_else(posteam == home_team, away_team, home_team)
  ) %>%
  arrange(-go_boost) %>%
  head(5) %>% 
  select(game_id, url)




