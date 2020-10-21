# note: this is poorly commented because it's just used to get some numbers for the post

library(tidyverse)
library(viridis)
library(gt)
source('R/helpers.R')
source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_nflscrapr_mutations.R")

seasons <- 2014:2019

# **************************************************************************************
# field goals

pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  ) %>%
    filter(
      play_type_nfl == "FIELD_GOAL"
    )
})

fg_data <- tibble::tibble(
  "yardline_100" = rep(5:40, 3),
  "model_roof" = c(rep("dome", 36), rep("outdoors", 36), rep("retractable", 36))
)

mgcv::predict.bam(fg_model, newdata = fg_data, type="response") %>%
  as_tibble() %>%
  dplyr::rename(made = value) %>%
  bind_cols(fg_data) %>%
  dplyr::rename(roof = model_roof) %>%
  mutate(
    roof = if_else(roof == "retractable", "Retractable", roof),
    roof = if_else(roof == "outdoors", "Outdoors", roof),
    roof = if_else(roof == "dome", "Dome", roof),
    roof = as.factor(roof),
    made = 100 * made,
    distance = yardline_100 + 18
  ) %>%
  filter(distance >= 20, distance <= 60) %>%
  ggplot(aes(x = distance, y = made, color = as.factor(roof))) + 
  geom_point(size = 4) + 
  geom_line(size = 1) + 
  # geom_smooth(size = 2, se = F) +
  labs(x = "Kick distance",
       y = "Percentage made",
       color = "Roof",
       title = "Expected Field Goal % by Distance and Roof") +
  theme_bw() + 
  scale_y_continuous(expand=c(0,0), breaks = scales::pretty_breaks(5)) + 
  scale_x_continuous(expand=c(0,1), breaks = scales::pretty_breaks(15)) +
  theme(
    plot.title = element_text(size = 22, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.position = c(.80, .80))

ggsave('figures/fg.png', dpi=600)

# **************************************************************************************
# punts

pbp <- purrr::map_df(2010 : 2019, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  ) %>% filter(play_type_nfl == "PUNT")
})

# thanks to Thomas Mock for the function here
# https://themockup.blog/posts/2020-08-28-heatmaps-in-ggplot2/#get-the-raw-density-estimates
get_density <- function(x, y, ...) {
  density_out <- MASS::kde2d(x, y, ...)
  int_x <- findInterval(x, density_out$x)
  int_y <- findInterval(y, density_out$y)
  comb_int <- cbind(int_x, int_y)
  return(density_out$z[comb_int])
}

# do some cleanup for the individual punts
punts <- pbp %>%
  select(desc, yardline_100, kick_distance, return_yards) %>%
  mutate(
    # give return yards too for final location
    yardline_after = yardline_100 - kick_distance + return_yards,
    yardline_after = 
      if_else(
        # find touchbacks
        stringr::str_detect(desc, "end zone") & is.na(kick_distance), 20, yardline_after
      ),
    # for blocked punts, just give them the ball there
    yardline_after = if_else(stringr::str_detect(desc, "BLOCKED") & is.na(yardline_after), yardline_100, yardline_after),
    # make it in the actual field of play
    yardline_after = if_else(yardline_after > 100, 100, yardline_after),
    # there's 2 safeties that are too annoying to deal with
    yardline_after = if_else(yardline_after == 0, 1, yardline_after),
    blocked = if_else(stringr::str_detect(desc, "BLOCKED") == 1, 1, 0),
    return_td = if_else(yardline_after == 100, 1, 0)
  ) %>%
  # there's like 10 of these for some reason
  filter(!is.na(yardline_after)) %>%
  select(desc, yardline_100, yardline_after, blocked, return_td)

punts


# show the data first
density_map_all <- punts %>%
  mutate(density = get_density(yardline_100, yardline_after, n = 100))

density_map_all %>% 
  filter(yardline_100 >= 35) %>%
  ggplot(aes(x = yardline_100, y = yardline_after, color = density)) +
  geom_point(alpha = 0.2) +
  scale_color_gradient(low = "red", high = "yellow") + 
  geom_smooth(size = 1, color = "gray", se = F) +
  labs(x = "Beginning yard line (distance from opp. end zone)",
       y = "End yard line",
       color = "Density",
       title = "Where do punts go?") +
  theme_bw() + 
  scale_y_continuous(expand=c(0,1), breaks = scales::pretty_breaks(5)) + 
  scale_x_continuous(expand=c(0,2), breaks = scales::pretty_breaks(10)) +
  theme(
    plot.title = element_text(size = 22, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.position = c(.10, .85))

ggsave('figures/punt.png', dpi=600)

# **************************************************************************************
# fourth downs

load('data/fd_model.Rdata', .GlobalEnv)

importance <- xgboost::xgb.importance(feature_names = colnames(fd_model), model = fd_model)
xgboost::xgb.ggplot.importance(importance_matrix = importance)

ggsave('figures/go.png', dpi=600)


readRDS(url("https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true")) %>%
  filter(week == 1, season == 2020, home_team == "ATL") %>%
  select(game_id, spread_line, total_line, roof) %>%
  mutate(posteam = "SEA", 
         home_team = "ATL",
         posteam_spread = if_else(posteam == home_team, spread_line, -spread_line),
         home_total = (total_line + spread_line) / 2,
         away_total = (total_line - spread_line) / 2,
         posteam_total = if_else(posteam == home_team, home_total, away_total),
         posteam_spread = dplyr::if_else(posteam == home_team, spread_line, -1 * spread_line)
         )


df <- tibble::tibble(
  "down" = 4,
  "ydstogo" = 5,
  "yardline_100" = 38,
  "era3" = 0,
  "era4" = 1,
  "outdoors" = 0,
  "retractable" = 1,
  "dome" = 0,
  "posteam_spread" = -1,
  "total_line" = 49.5,
  "posteam_total" = 24.25,
  "posteam_timeouts_remaining" = 3,
  "defteam_timeouts_remaining" = 3,
  "score_differential" = 2,
  "posteam" = "SEA",
  "home_team" = "ATL",
  "away_team" = "SEA",
  "half_seconds_remaining" = 589,
  "game_seconds_remaining" = 589,
  "receive_2h_ko" = 0,
  "runoff" = 0,
  "qtr" = 3,
  "season" = 2020,
  "roof" = "retractable",
  "spread_line" = 1
)

data <- df %>%
  select(
    down,    ydstogo,     yardline_100,  era3,     era4,     outdoors, 
    retractable,  dome,    posteam_spread, total_line,  posteam_total 
  )


preds <- stats::predict(
  fd_model,
  as.matrix(data)
)  %>%
  tibble::as_tibble() %>%
  dplyr::rename(prob = "value") %>%
  bind_cols(df[rep(1, 76), ]) %>%
  mutate(
    gain = -10:65,
    gain = if_else(gain > yardline_100, as.integer(yardline_100), as.integer(gain))
  ) %>%
  group_by(gain) %>%
  mutate(prob = sum(prob)) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  mutate(
    yardline_100 = yardline_100 - gain,
    posteam_timeouts_pre = posteam_timeouts_remaining,
    defeam_timeouts_pre = defteam_timeouts_remaining,
    turnover = dplyr::if_else(gain < ydstogo, as.integer(1), as.integer(0)),
    down = 1,
    # if now goal to go, use yardline, otherwise it's 1st and 10 either way
    ydstogo = dplyr::if_else(yardline_100 < 10, as.integer(yardline_100), as.integer(10)),
    # possession change if 4th down failed
    # flip yardline_100, timeouts for turnovers
    yardline_100 = dplyr::if_else(turnover == 1, as.integer(100 - yardline_100), as.integer(yardline_100)),
    posteam_timeouts_remaining = dplyr::if_else(turnover == 1 | yardline_100 == 0,
                                                defeam_timeouts_pre,
                                                posteam_timeouts_pre),
    defteam_timeouts_remaining = dplyr::if_else(turnover == 1 | yardline_100 == 0,
                                                posteam_timeouts_pre,
                                                defeam_timeouts_pre),
    score_differential = if_else(turnover == 1, -score_differential, score_differential),
    half_seconds_remaining = half_seconds_remaining - 6,
    game_seconds_remaining = game_seconds_remaining - 6,
    half_seconds_remaining = if_else(half_seconds_remaining < 0, 0, half_seconds_remaining),
    game_seconds_remaining = if_else(game_seconds_remaining < 0, 0, game_seconds_remaining),
    # additional runoff after successful non-td conversion
    half_seconds_remaining = if_else(turnover == 0 & df$yardline_100 > gain, half_seconds_remaining - df$runoff, half_seconds_remaining),
    game_seconds_remaining = if_else(turnover == 0 & df$yardline_100 > gain, game_seconds_remaining - df$runoff, game_seconds_remaining),
    # flip receive_2h_ko var
    receive_2h_ko = case_when(
      qtr <= 2 & receive_2h_ko == 0 & (yardline_100 == 0 | turnover == 1) ~ 1,
      qtr <= 2 & receive_2h_ko == 1 & (yardline_100 == 0 | turnover == 1) ~ 0,
      TRUE ~ receive_2h_ko
    ),
    # switch posteam
    posteam = case_when(
      home_team == posteam & (turnover == 1 | yardline_100 == 0) ~ away_team, 
      away_team == posteam & (turnover == 1 | yardline_100 == 0) ~ home_team,
      TRUE ~ posteam
    ),
    # deal with touchdown
    score_differential = if_else(yardline_100 == 0, as.integer(-score_differential - 7), as.integer(score_differential)),
    yardline_100 = if_else(yardline_100 == 0, as.integer(75), as.integer(yardline_100))
  ) %>%
  nflfastR::calculate_expected_points() %>%
  nflfastR::calculate_win_probability() %>%
  mutate(
    # flip for possession change
    vegas_wp = if_else(posteam != df$posteam, 1 - vegas_wp, vegas_wp),
    # fill in end of game situation when team can kneel out clock
    vegas_wp = case_when(
      score_differential > 0 & turnover == 0 & df$yardline_100 > gain & game_seconds_remaining < 120 & defteam_timeouts_remaining == 0 ~ 1,
      score_differential > 0 & turnover == 0 & df$yardline_100 > gain & game_seconds_remaining < 80 & defteam_timeouts_remaining == 1 ~ 1,
      score_differential > 0 & turnover == 0 & df$yardline_100 > gain & game_seconds_remaining < 40 & defteam_timeouts_remaining == 2 ~ 1,
      TRUE ~ vegas_wp
    )
  ) %>%
  mutate(wt_wp = prob * vegas_wp) %>%
  mutate(fd = if_else(gain < df$ydstogo, 0, 1)) %>%
  group_by(fd) %>%
  mutate(fd_pct = sum(prob), 
         new_prob = prob / fd_pct,
         wt_wp = new_prob * vegas_wp
  ) %>%
  ungroup()

preds %>%
  filter(between(gain, 2, 5)) %>%
  select(gain, prob, vegas_wp)


# **************************************************************************************
# get list of plays

# for getting spreads
games <- readRDS(url("https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true")) %>%
  mutate(game_type = if_else(game_type == "REG", "reg", "post"))

# get data
pbp <- readRDS(url(glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds")))

# some prep
plays <- pbp %>%
  filter(down == 4, vegas_wp > .05, vegas_wp < .95) %>%
  dplyr::group_by(game_id) %>%
  dplyr::mutate(
    receive_2h_ko = dplyr::if_else(home_team == dplyr::first(stats::na.omit(posteam)), 1, 0)
  ) %>%
  ungroup()


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
    'yr' = 2020,
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
library(future)
future::plan(multisession)

fourth_downs <- furrr::future_map_dfr(1 : nrow(plays), function(x) {
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
    play_type != "PENALTY",
    go_boost > 1 | go_boost < -1
  ) %>%
  mutate(type = case_when(
    go_boost >= 4 ~ "Recommendation: definitely go for it",
    go_boost > 1 & go_boost < 4 ~ "Recommendation: probably go for it",
    go_boost < -1 & go_boost > -4 ~ "Recommendation: probably kick",
    go_boost <= -4 ~ "Recommendation: definitely kick"
  )) %>%
  group_by(type) %>%
  summarize(go = mean(go), n = n()) %>%
  ungroup() %>%
  arrange(-go) %>%
  mutate(go = 100 * go) %>%
  gt() %>%
  cols_label(
    type = "",
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

t %>% gtsave("figures/team_worst.png")


# **************************************************************************************
# the play

df <- tibble::tibble(
  "type" = "reg",
  "qtr" = as.integer(3),
  "time" = 60 * as.integer(9) + as.integer(49),
  'posteam' = as.character("SEA"),
  'away_team' = as.character("SEA"),
  'home_team' = as.character("ATL"),
  'yardline_100' = as.integer(38),
  'ydstogo' = as.integer(5),
  'posteam_timeouts_remaining' = as.integer(3),
  'defteam_timeouts_remaining' = as.integer(3),
  'home_opening_kickoff' = as.integer(1),
  'score_differential' = as.integer(2),
  'runoff' = as.integer(0),
  'yr' = as.integer(2020)
) %>%
  prepare_df(games)

punt_probs <- punt_df %>%
  filter(yardline_100 == df$yardline_100) %>%
  select(yardline_after, pct)

probs <- punt_probs %>%
  bind_cols(df[rep(1, nrow(punt_probs)), ]) %>%
  flip_team() %>%
  mutate(
    yardline_100 = 100 - yardline_after,
    
    # deal with punt return TD
    # we want punting team to be receiving a kickoff
    posteam = if_else(yardline_after == 100, df$posteam, posteam),
    yardline_100 = if_else(yardline_after == 100, as.integer(75), as.integer(yardline_100)),
    posteam_timeouts_remaining = dplyr::if_else(yardline_after == 100,
                                                df$posteam_timeouts_remaining,
                                                posteam_timeouts_remaining),
    defteam_timeouts_remaining = dplyr::if_else(yardline_after == 100,
                                                df$defteam_timeouts_remaining,
                                                defteam_timeouts_remaining),
    score_differential = if_else(yardline_after == 100, as.integer(-score_differential - 7), as.integer(score_differential)),
    receive_2h_ko = case_when(
      qtr <= 2 & receive_2h_ko == 0 & (yardline_after == 100) ~ 1,
      qtr <= 2 & receive_2h_ko == 1 & (yardline_after == 100) ~ 0,
      TRUE ~ receive_2h_ko
    )
  )

# have to flip bc other team
probs %>%
  nflfastR::calculate_expected_points() %>%
  nflfastR::calculate_win_probability() %>%
  mutate(
    # for the punt return TD case
    vegas_wp = if_else(yardline_after == 100, 1 - vegas_wp, vegas_wp),
    
    # fill in end of game situation when team can kneel out clock
    # discourages punting when the other team can end the game
    vegas_wp = case_when(
      score_differential > 0 & game_seconds_remaining < 120 & defteam_timeouts_remaining == 0 ~ 1,
      score_differential > 0 & game_seconds_remaining < 80 & defteam_timeouts_remaining == 1 ~ 1,
      score_differential > 0 & game_seconds_remaining < 40 & defteam_timeouts_remaining == 2 ~ 1,
      TRUE ~ vegas_wp
    )
    )

