library(tidyverse)
library(gt)
library(future)
library(ggtext)
library(ggpmisc)

source('R/helpers.R')
source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_nflscrapr_mutations.R")

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

# **************************************************************************************
# get list of plays

# for getting spreads
games <- readRDS(url("https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true")) %>%
  mutate(game_type = if_else(game_type == "REG", "reg", "post"))

# which season do you want?
s = 2019

# get data
pbp <- readRDS(url(glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{s}.rds")))

# some prep
plays <- pbp %>%
  dplyr::group_by(game_id) %>%
  dplyr::mutate(
    receive_2h_ko = dplyr::if_else(home_team == dplyr::first(stats::na.omit(posteam)), 1, 0)
  ) %>%
  ungroup() %>%
  filter(down == 4, game_seconds_remaining > 30, 
         !is.na(half_seconds_remaining), !is.na(qtr), !is.na(posteam))

# get down to one play per series
# and get the initial situation
plays <- plays %>%
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
  )))


# add probs to data
future::plan(multisession)
fourth_downs <- furrr::future_map_dfr(1 : nrow(plays), function(x) {
  message(glue::glue("game {plays %>% dplyr::slice(x) %>% pull(game_id)} play {plays %>% dplyr::slice(x) %>% pull(play_id)}"))
  get_probs(plays %>% dplyr::slice(x), games)
})

# **************************************************************************************
# cleaning

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
    should_go = if_else(go_boost > 0, 1, 0)
  ) %>%
  arrange(go_boost) %>%
  # filter(!(mins < 1 & qtr == 4)) %>%
  select(
    game_id, prior_wp, url, posteam, home_team, away_team, desc, play_type, go_boost, go, should_go, yardline_100, ydstogo, qtr, mins, seconds
  )

# **************************************************************************************
# decision-making table

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
    title = md(glue::glue("NFL team decision-making by go recommendation, 2020"))
  ) %>%
  tab_source_note(md('**Notes**: "Definitely" recommendations are greater than 4 percentage point advantage,<br> "probably" 1-4 percentage points'))


t

t %>% gtsave("figures/team_behavior.png")

# **************************************************************************************
# worst decisions table

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


# **************************************************************************************
# team results

my_title <- glue::glue("NFL Go-for-it Rate on <span style='color:red'>4th down</span>")
plot <- cleaned %>%
  filter(prior_wp > .05 & prior_wp < .95) %>%
  mutate(go_boost = round(go_boost, 0)) %>%
  group_by(go_boost) %>%
  summarize(go = 100 * mean(go)) %>%
  ungroup() %>%
  filter(between(go_boost, -10, 10)) %>%
  mutate(
    should_go = if_else(go_boost >= 0, 1, 0)
  )

plot %>%
  ggplot(aes(go_boost, go, color = as.factor(should_go))) + 
  geom_point(size = 5, color = "black", alpha = .5) +
  geom_vline(xintercept = 0)+
  geom_smooth(method = "lm", show.legend = F, se = F, size = 3)+
  theme_bw()+
  labs(x = "Gain in win probability by going for it",
       y = "Go-for-it percentage",
       caption = paste0("Figure: @benbbaldwin | Win prob between 5% and 95%"),
       subtitle = "By strength of @ben_bot_baldwin recommendation, 2020",
       title = my_title) +
  theme(
    legend.position = "none",
    plot.title = element_markdown(size = 22, hjust = 0.5),
    plot.subtitle = element_markdown(size = 12, hjust = 0.5)
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  annotate("text",x=-4, y= 90, label = "Should\nkick", color="red", size = 5) +
  annotate("text",x=3, y= 90, label = "Should\ngo for it", color="red", size = 5) +
  annotate("label",x=-6, y= 15, label = "Teams almost always kick\nwhen they should...", size = 5) +
  annotate("label",x=6, y= 25, label = "...but frequently\n kick when they\nshould go for it", size = 5)
  
ggsave("figures/league_behavior.png")

# ###########
# go by WP
my_title <- glue::glue("NFL Go-for-it Rate on <span style='color:red'>4th down</span>")
plot <- cleaned %>%
  filter(go_boost > 2) %>%
  mutate(wp = round(100 * prior_wp, 0)) %>%
  group_by(wp) %>%
  summarize(go = 100 * mean(go), n = n()) %>%
  ungroup()

cleaned %>%
  filter(go_boost > 2) %>%
  mutate(prior_wp = 100 * prior_wp) %>%
  ggplot(aes(prior_wp, go)) + 
  # geom_point(size = 5, color = "black", alpha = .5) +
  geom_smooth(show.legend = F, se = F, size = 3, color = "black")+
  theme_bw()+
  labs(x = "Win probability prior to play",
       y = "Go-for-it percentage",
       caption = paste0("Figure: @benbbaldwin"),
       subtitle = "@ben_bot_baldwin gain in win prob by going for it > 2 percentage points",
       title = my_title) +
  theme(
    legend.position = "none",
    plot.title = element_markdown(size = 22, hjust = 0.5),
    plot.subtitle = element_markdown(size = 12, hjust = 0.5)
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(0, 1)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) 

ggsave("figures/league_behavior_wp.png")

# ###################################################### #######################
# ############## team bar chart
current <- cleaned %>%
  filter(go_boost > 2) %>%
  filter(prior_wp > .2) %>%
  group_by(posteam) %>%
  summarize(go = mean(go), n = n()) %>%
  ungroup() %>%
  left_join(nflfastR::teams_colors_logos, by=c('posteam' = 'team_abbr')) %>%
  arrange(-go) %>%
  mutate(rank = 1:n()) %>%
  arrange(posteam)


ids <- nflfastR::teams_colors_logos %>%
  filter(!team_abbr %in% c('LAR', 'OAK', 'SD', 'STL'))
images <- magick::image_read(ids%>% pull(team_logo_espn)) 

logos <- tibble(
  x = current$rank + .25, 
  y = current$go + .02,
  width = .035,
  grob = 
    map(1:32, function(x) {
      grid::rasterGrob(images[x])
    })
)

my_title <- glue::glue("Which teams <span style='color:red'>go for it</span> when they <span style='color:red'>should?</span>")
ggplot(data = current, aes(x = reorder(posteam, -go), y = go)) +
  geom_col(data = current, aes(fill = ifelse(posteam=="SEA", team_color2, team_color)), 
           width = 0.5, alpha = .6, show.legend = FALSE
  ) +
  geom_grob(data = logos, 
            aes(x, y, label = grob, vp.width = width),
            hjust = 0.7) +
  scale_fill_identity(aesthetics = c("fill", "colour")) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_markdown(size=22,face = 2,hjust=.5),
    plot.subtitle = element_text(size=8, hjust=.5),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
    ) +
  # scale_y_continuous(expand=c(0,0), limits=c(0, max(current$go + 5))) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    x = "",
    y = "Go rate",
    title= my_title,
    subtitle = "When @ben_bot_baldwin recommends going for it (gain in win prob. at least 2 percentage points)",
    caption = glue::glue("Sample size in parentheses\nExcl. final 30 seconds of game. Win prob >20%")
  ) +
  geom_text(data = current, aes(x = rank, y = -.015, size=.04, label = glue::glue("({n})")), show.legend = FALSE, nudge_x = 0, color="black")

ggsave("figures/teams_2020.png")




current <- cleaned %>%
  filter(go_boost > 0, go == 0) %>%
  group_by(posteam) %>%
  summarize(go = sum(go_boost), n = n()) %>%
  ungroup() %>%
  left_join(nflfastR::teams_colors_logos, by=c('posteam' = 'team_abbr')) %>%
  arrange(-go) %>%
  mutate(rank = 1:n()) %>%
  arrange(posteam)


logos <- tibble(
  x = current$rank + .25, 
  y = current$go + .02,
  width = .035,
  grob = 
    map(1:32, function(x) {
      grid::rasterGrob(images[x])
    })
)

my_title <- glue::glue("Expected win probability <span style='color:red'>lost by kicking in go situations</span>")
ggplot(data = current, aes(x = reorder(posteam, -go), y = go)) +
  geom_col(data = current, aes(fill = ifelse(posteam=="SEA", team_color2, team_color)), 
           width = 0.5, alpha = .6, show.legend = FALSE
  ) +
  geom_grob(data = logos, 
            aes(x, y, label = grob, vp.width = width),
            hjust = 0.7) +
  scale_fill_identity(aesthetics = c("fill", "colour")) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_markdown(size=22,face = 2,hjust=.5),
    plot.subtitle = element_text(size=8, hjust=.5),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  ) +
  # scale_y_continuous(expand=c(0,0), limits=c(0, max(current$go + 5))) +
  scale_y_continuous(n.breaks = 10) +
  labs(
    x = "",
    y = "Win probability lost",
    title= my_title,
    caption = glue::glue("@benbbaldwin | Excl. final 30 seconds of game")
  )
  # geom_text(data = current, aes(x = rank, y = -.015, size=.04, label = glue::glue("({n})")), show.legend = FALSE, nudge_x = 0, color="black")

ggsave("figures/teams_lost_2020.png")




