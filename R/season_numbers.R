library(tidyverse)
library(gt)
library(future)
library(ggtext)
library(ggpmisc)
library(DescTools)
library(ggthemes)
library(magick)
library(cowplot)

source('R/helpers.R')
source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_nflscrapr_mutations.R")
source('R/season_numbers_functions.R')

theme_ben <- theme_fivethirtyeight() +
  theme(
    legend.position = "none",
    plot.title = element_markdown(size = 22, hjust = 0.5),
    plot.subtitle = element_markdown(size = 12, hjust = 0.5),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=12, face="bold")
  )

# **************************************************************************************
# the first part: numbers for one season only (2020 here)
# get list of plays

# first, make sure df is up to date
update_season(2020)

# this will load the dataframe
cleaned <- load_season(2020, rebuild = TRUE)

# **************************************************************************************
# decision-making table
# do coaches follow the bot?

t <- cleaned %>%
  arrange(game_id, series, play_id) %>%
  group_by(game_id, series) %>%
  mutate(go = max(go)) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
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
    title = md(glue::glue("NFL team decision-making by go recommendation, {max(cleaned$season)}"))
  ) %>%
  tab_source_note(md('**Notes**: "Definitely" recommendations are greater than 4 percentage point advantage,<br> "probably" 1-4 percentage points'))


t

t %>% gtsave("figures/team_behavior.png")

# **************************************************************************************
# worst decisions table

# note: ben changed this to just look at playoff games for now
# remove the week filter to look at whole season
t <- cleaned %>%
  filter(
    # FOR PLAYOFFS ONLY
    week > 17,
    # END FOR PLAYOFFS ONLY
    
    go == 0,
    # they tried to go for it
    !(posteam == "ARI" & week == 3 & play_id == 2364)
  ) %>%
  mutate(
    defteam = if_else(posteam == home_team, away_team, home_team)
  ) %>%
  arrange(-go_boost) %>%
  mutate(rank = 1 : n()) %>%
  head(10) %>%
  select(rank, posteam, defteam, week, qtr, ydstogo, diff = score_differential, go_boost, desc) %>%
  gt() %>%
  cols_label(
    rank = "",
    posteam = "Team",
    defteam = "Opp",
    week = "Week",
    qtr = "Qtr",
    ydstogo = "YTG",
    diff = "Diff",
    desc = "Play",
    go_boost = "WP loss"
  ) %>%
  tab_style(
    style = cell_text(color = "black", weight = "bold"),
    locations = list(
      cells_row_groups(),
      cells_column_labels(everything())
    )
  ) %>% 
  text_transform(
    locations = cells_body(vars(posteam)),
    fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
  ) %>% 
  text_transform(
    locations = cells_body(vars(defteam)),
    fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
  ) %>% 
  cols_width(
    everything() ~ px(400),
    ) %>% 
  cols_width(
    vars(rank) ~ px(30),
    vars(posteam) ~ px(50),
    vars(defteam) ~ px(50),
    vars(week) ~ px(50),
    vars(diff) ~ px(50),
    vars(qtr) ~ px(50),
    vars(ydstogo) ~ px(50),
    vars(go_boost) ~ px(70)
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
    column_labels.border.bottom.width = px(2),
    row.striping.background_color = '#FFFFFF',
    row.striping.include_table_body = TRUE,
    table.background.color = '#F2F2F2'
  ) %>%
  fmt_number(
    columns = vars(go_boost), decimals = 1
  ) %>%
  cols_align(
    columns = 1:8,
    align = "center"
  ) %>% 
  tab_header(
    title = md(glue::glue("Worst kick decisions of {max(cleaned$season)} playoffs"))
  )

t

t %>% gtsave("figures/team_worst.png")


# **************************************************************************************
# best decisions table

# remove the week filter to look at whole season
t <- cleaned %>%
  filter(
    # FOR PLAYOFFS ONLY
    # week > 17,
    # END FOR PLAYOFFS ONLY
    
    go == 1,
    # they tried to go for it
    !(posteam == "ARI" & week == 3 & play_id == 2364)
  ) %>%
  mutate(
    defteam = if_else(posteam == home_team, away_team, home_team)
  ) %>%
  arrange(-go_boost) %>%
  mutate(rank = 1 : n()) %>%
  head(10) %>%
  select(rank, posteam, defteam, week, qtr, ydstogo, diff = score_differential, go_boost, desc) %>%
  gt() %>%
  cols_label(
    rank = "",
    posteam = "Team",
    defteam = "Opp",
    week = "Week",
    qtr = "Qtr",
    ydstogo = "YTG",
    diff = "Diff",
    desc = "Play",
    go_boost = "WP gain"
  ) %>%
  tab_style(
    style = cell_text(color = "black", weight = "bold"),
    locations = list(
      cells_row_groups(),
      cells_column_labels(everything())
    )
  ) %>% 
  text_transform(
    locations = cells_body(vars(posteam)),
    fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
  ) %>% 
  text_transform(
    locations = cells_body(vars(defteam)),
    fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
  ) %>% 
  cols_width(
    everything() ~ px(400),
  ) %>% 
  cols_width(
    vars(rank) ~ px(30),
    vars(posteam) ~ px(50),
    vars(defteam) ~ px(50),
    vars(week) ~ px(50),
    vars(diff) ~ px(50),
    vars(qtr) ~ px(50),
    vars(ydstogo) ~ px(50),
    vars(go_boost) ~ px(70)
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
    column_labels.border.bottom.width = px(2),
    row.striping.background_color = '#FFFFFF',
    row.striping.include_table_body = TRUE,
    table.background.color = '#F2F2F2',
    table.font.size = px(13L),
    data_row.padding = px(3)
  ) %>%
  fmt_number(
    columns = vars(go_boost), decimals = 1
  ) %>%
  cols_align(
    columns = 1:8,
    align = "center"
  ) %>% 
  tab_header(
    title = md(glue::glue("Best go-for-it decisions of {max(cleaned$season)}"))
  )

t

t %>% gtsave("figures/team_best.png")



# **************************************************************************************
# league behavior by go recommendation

my_title <- glue::glue("NFL Go-for-it Rate on <span style='color:red'>4th down</span>")
plot <- cleaned %>%
  mutate(go_boost = RoundTo(go_boost, 0.5)) %>%
  group_by(go_boost) %>%
  summarize(go = 100 * mean(go)) %>%
  ungroup() %>%
  filter(between(go_boost, -10, 10)) %>%
  mutate(
    should_go = case_when(go_boost > .5 ~ 1,
                          go_boost < -.5 ~ 0,
                          TRUE ~ 2)
  )

plot %>%
  ggplot(aes(go_boost, go, color = as.factor(should_go))) + 
  geom_point(size = 5, color = "black", alpha = .5) +
  geom_vline(xintercept = 0) +
  geom_smooth(method = "lm", show.legend = F, se = F, size = 3)+
  theme_ben +
  labs(x = "Gain in win probability by going for it",
       y = "Go-for-it percentage",
       caption = paste0("Figure: @benbbaldwin"),
       subtitle = "By strength of @ben_bot_baldwin recommendation, 2020",
       title = my_title) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0,2)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20), limits = c(-10, 10), expand = c(0,0)) +
  annotate("text",x=-4, y= 90, label = "Should\nkick", color="red", size = 7) +
  annotate("text",x=3, y= 90, label = "Should\ngo for it", color="red", size = 7) +
  annotate("label",x=-6, y= 15, label = "Teams almost always kick\nwhen they should...", size = 6) +
  annotate("label",x=6, y= 25, label = "...but frequently\n kick when they\nshould go for it", size = 6)
  
ggsave("figures/league_behavior.png")

# ###########
# go by WP

min <- 2
max <- 4

my_title <- glue::glue("NFL Go-for-it Rate on <span style='color:red'>4th down</span>")

cleaned %>%
  filter(go_boost > min & go_boost < max) %>%
  mutate(prior_wp = 100 * prior_wp) %>%
  ggplot(aes(prior_wp, go)) + 
  # geom_point(size = 5, color = "black", alpha = .5) +
  geom_smooth(show.legend = F, se = F, size = 3, color = "black")+
  theme_bw()+
  labs(x = "Win probability prior to play",
       y = "Go-for-it percentage",
       caption = paste0("Figure: @benbbaldwin | 2020 season"),
       subtitle = glue::glue("@ben_bot_baldwin gain in win prob by going {min}-{max} percentage points"),
       title = my_title) +
  theme(
    legend.position = "none",
    plot.title = element_markdown(size = 22, hjust = 0.5),
    plot.subtitle = element_markdown(size = 12, hjust = 0.5)
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(0, 1)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) 

ggsave("figures/league_behavior_wp.png")

# #########################################################################
# team go by predicted go

get_fig <- function(tm) {
  
  nick <- nflfastR::teams_colors_logos %>% filter(team_abbr == tm) %>% pull(team_nick)
  my_title <- glue::glue("<span style='color:red'>{nick}</span> fourth down decisions, 2020")
  cleaned %>% 
    filter(posteam == tm) %>%
    mutate(go = if_else(go == 1, "Yes", "No")) %>%
    select(game_id, go, pred_go, yardline_100, score_differential, ydstogo, desc) %>%
    ggplot(aes(x = go, y=pred_go, color=go, shape=go)) +
    geom_jitter(aes(y = pred_go, fill = go), 
                size = 6, width = 0.25, show.legend=FALSE, alpha=.8) +
    scale_y_continuous(name = "Predicted likelihood of going for it", breaks = scales::pretty_breaks(n = 5)) +
    theme_ben +
    labs(title = my_title,
         x = "Went for it",
         caption = "Predicted go based on observed team behavior\nbased on time, score, field position, week, and season"
    )
}


get_fig("SEA")

ggsave("figures/go_vs_exp.png")

# ############################### above but by conference
# get divisions
divs <- teamcolors::teamcolors %>%
  filter(league == "nfl") %>%
  select(name, division) %>%
  mutate(name = case_when(
    name == "Washington Redskins" ~ "Washington Football Team",
    name == "Oakland Raiders" ~ "Las Vegas Raiders",
    TRUE ~ name
  ))

nfl_logos_df <- nflfastR::teams_colors_logos %>%
  filter(!(team_abbr %in% c("LAR", "SD", "OAK"))) %>%
  left_join(divs, by = c("team_name" = "name"))


get_conf <- function(conf) {
  
  logos_conf <- image_read(nfl_logos_df %>% filter(substr(division, 1, 3) == conf) %>% pull(team_logo_espn))
  my_title <- glue::glue("<span style='color:red'>{conf}</span> fourth down decisions, 2020")
  
  cleaned %>% 
    left_join(nfl_logos_df, by = c("posteam" = "team_abbr")) %>%
    filter(substr(division, 1, 3) == conf) %>%
    mutate(go = if_else(go == 1, "Yes", "No")) %>%
    select(game_id, posteam, team_logo_espn, go, pred_go, yardline_100, score_differential, ydstogo, desc) %>%
    ggplot(aes(x = posteam, y=pred_go, color=go)) +
    geom_jitter(aes(y = pred_go, fill = go), 
                size = 4, width = 0.125, show.legend=FALSE, alpha=.5) +
    scale_y_continuous(name = "Predicted likelihood of going for it", breaks = scales::pretty_breaks(n = 5), expand = c(0,.03)) +
    theme_ben +
    labs(title = my_title,
         x = "Went for it",
         subtitle = "<span style='color:#F8766D'>Kicked</span> versus <span style='color:#00BFC4'>went for it</span>",
         caption = "Predicted go based on past league behavior given\nyardage, time, score, field position, week, and season"
    ) +
    theme(
      panel.grid.major = element_blank(),
      plot.subtitle = element_markdown(size = 16, hjust = 0.5),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    ) +
    geom_hline(yintercept = -0.1, color = "gray", alpha = .1) +
    # doing this in a smart way is annoying so just brute forcing it
    draw_image(logos_conf[1],  x = .5, y = -.55, scale = .95) +
    draw_image(logos_conf[2],  x = 1.5, y = -.55, scale = .95) +
    draw_image(logos_conf[3],  x = 2.5, y = -.55, scale = .95) +
    draw_image(logos_conf[4],  x = 3.5, y = -.55, scale = .95) +
    draw_image(logos_conf[5],  x = 4.5, y = -.55, scale = .95) +
    draw_image(logos_conf[6],  x = 5.5, y = -.55, scale = .95) +
    draw_image(logos_conf[7],  x = 6.5, y = -.55, scale = .95) +
    draw_image(logos_conf[8],  x = 7.5, y = -.55, scale = .95) +
    draw_image(logos_conf[9],  x = 8.5, y = -.55, scale = .95) +
    draw_image(logos_conf[10],  x = 9.5, y = -.55, scale = .95) +
    draw_image(logos_conf[11],  x = 10.5, y = -.55, scale = .95) +
    draw_image(logos_conf[12],  x = 11.5, y = -.55, scale = .95) +
    draw_image(logos_conf[13],  x = 12.5, y = -.55, scale = .95) +
    draw_image(logos_conf[14],  x = 13.5, y = -.55, scale = .95) +
    draw_image(logos_conf[15],  x = 14.5, y = -.55, scale = .95) +
    draw_image(logos_conf[16],  x = 15.5, y = -.55, scale = .95)
  
}

get_conf("AFC")
ggsave("figures/go_vs_exp_afc.png")


get_conf("NFC")
ggsave("figures/go_vs_exp_nfc.png")


# ############################### above but by bot rec rather than predicted


follow_bot <- function(conf) {
  
  logos_conf <- image_read(nfl_logos_df %>% filter(substr(division, 1, 3) == conf) %>% pull(team_logo_espn))
  my_title <- glue::glue("<span style='color:red'>{conf}</span>: when did teams follow the bot? 2020")
  
  cleaned %>% 
    left_join(nfl_logos_df, by = c("posteam" = "team_abbr")) %>%
    filter(substr(division, 1, 3) == conf) %>%
    filter(between(go_boost, -5, 15)) %>%
    mutate(go = as_factor(go)) %>%
    select(game_id, posteam, team_logo_espn, go, go_boost, pred_go, yardline_100, score_differential, ydstogo, desc) %>%
    ggplot(aes(x = posteam, y=go_boost, color=go)) +
    geom_hline(yintercept = 0) +
    geom_jitter(aes(y = go_boost, fill = go), 
                size = 3, width = 0.2, show.legend=FALSE, alpha=.5) +
    scale_y_continuous(name = "4th down bot gain in going for it", breaks = scales::pretty_breaks(n = 5), expand = c(0,.03)) +
    theme_ben +
    labs(title = my_title,
         x = "Went for it",
         subtitle = "<span style='color:#F8766D'>Kicked</span> versus <span style='color:#00BFC4'>went for it</span>",
         caption = "Predicted go based on past league behavior given\nyardage, time, score, field position, week, and season"
    ) +
    theme(
      panel.grid.major = element_blank(),
      plot.subtitle = element_markdown(size = 16, hjust = 0.5),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    ) +
    geom_hline(yintercept = -6, color = "gray", alpha = .1) +
    # doing this in a smart way is annoying so just brute forcing it
    draw_image(logos_conf[1],  x = .5, y = -5.75, scale = 2) +
    draw_image(logos_conf[2],  x = 1.5, y = -5.75, scale = 2) +
    draw_image(logos_conf[3],  x = 2.5, y = -5.75, scale = 2) +
    draw_image(logos_conf[4],  x = 3.5, y = -5.75, scale = 2) +
    draw_image(logos_conf[5],  x = 4.5, y = -5.75, scale = 2) +
    draw_image(logos_conf[6],  x = 5.5, y = -5.75, scale = 2) +
    draw_image(logos_conf[7],  x = 6.5, y = -5.75, scale = 2) +
    draw_image(logos_conf[8],  x = 7.5, y = -5.75, scale = 2) +
    draw_image(logos_conf[9],  x = 8.5, y = -5.75, scale = 2) +
    draw_image(logos_conf[10],  x = 9.5, y = -5.75, scale = 2) +
    draw_image(logos_conf[11],  x = 10.5, y = -5.75, scale = 2) +
    draw_image(logos_conf[12],  x = 11.5, y = -5.75, scale = 2) +
    draw_image(logos_conf[13],  x = 12.5, y = -5.75, scale = 2) +
    draw_image(logos_conf[14],  x = 13.5, y = -5.75, scale = 2) +
    draw_image(logos_conf[15],  x = 14.5, y = -5.75, scale = 2) +
    draw_image(logos_conf[16],  x = 15.5, y = -5.75, scale = 2)
  
}

follow_bot("AFC")
ggsave("figures/follow_bot_afc.png")

follow_bot("NFC")
ggsave("figures/follow_bot_nfc.png")

# ###################################################### #######################
# ############## team bar chart
current <- cleaned %>%
  filter(go_boost > 1.5) %>%
  filter(prior_wp > .2) %>%
  filter(week <= 17) %>%
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

my_title <- glue::glue("Which teams <span style='color:red'>go for it</span> when they <span style='color:red'>should?</span> {max(cleaned$season)}")
ggplot(data = current, aes(x = reorder(posteam, -go), y = go)) +
  geom_col(data = current, aes(fill = ifelse(posteam=="SEA", team_color2, team_color)), 
           width = 0.5, alpha = .6, show.legend = FALSE
  ) +
  geom_grob(data = logos, 
            aes(x, y, label = grob, vp.width = width),
            hjust = 0.7) +
  scale_fill_identity(aesthetics = c("fill", "colour")) +
  theme_ben +
  theme(
    panel.grid.major.x = element_blank(),
    #plot.title = element_markdown(size=22,face = 2,hjust=.5),
    #plot.subtitle = element_text(size=8, hjust=.5),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
    ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    x = "",
    y = "Go rate",
    title= my_title,
    subtitle = "When @ben_bot_baldwin recommends going for it (gain in win prob. at least 1.5 percentage points)",
    caption = glue::glue("Sample size in parentheses\nExcl. final 30 seconds of game. Win prob >20%")
  ) +
  geom_text(data = current, aes(x = rank, y = -.015, size=.04, label = glue::glue("({n})")), show.legend = FALSE, nudge_x = 0, color="black")

ggsave(glue::glue("figures/teams_{max(cleaned$season)}.png"))


# total WP lost per game figure
current <- cleaned %>%
  # FOR PLAYOFFS ONLY
  # filter(week > 17) %>%
  # FOR PLAYOFFS ONLY
  group_by(posteam) %>%
  mutate(
    games = n_distinct(game_id),
  ) %>%
  ungroup() %>%
  filter(go_boost > 0, go == 0) %>%
  group_by(posteam) %>%
  summarize(
    go = sum(go_boost), 
    n = n(),
    games = dplyr::first(games),
    go = go/games
    ) %>%
  ungroup() %>%
  left_join(nflfastR::teams_colors_logos, by=c('posteam' = 'team_abbr')) %>%
  arrange(-go) %>%
  mutate(rank = 1:n()) %>%
  arrange(posteam)

ids <- nflfastR::teams_colors_logos %>%
  filter(!team_abbr %in% c('LAR', 'OAK', 'SD', 'STL')) %>%
  filter(team_abbr %in% current$posteam)
images <- magick::image_read(ids%>% pull(team_logo_espn)) 

logos <- tibble(
  x = current$rank + .25, 
  y = current$go + .02,
  width = .035,
  grob = 
    map(1:length(images), function(x) {
      grid::rasterGrob(images[x])
    })
)

my_title <- glue::glue("Expected win probability per game <span style='color:red'>lost by kicking in go situations</span>, {max(cleaned$season)}")
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
    plot.title = element_markdown(size=20,face = 2,hjust=.5),
    plot.subtitle = element_text(size=8, hjust=.5),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  ) +
  # scale_y_continuous(expand=c(0,0), limits=c(0, max(current$go + 5))) +
  scale_y_continuous(n.breaks = 10) +
  labs(
    x = "",
    y = "Win probability lost per game",
    title= my_title,
    caption = glue::glue("@benbbaldwin | Excl. final 30 seconds of game")
  )

ggsave(glue::glue("figures/teams_lost_{max(cleaned$season)}.png"))


# ***********************************************************************************************************************
# ***********************************************************************************************************************
# ***********************************************************************************************************************
# ***********************************************************************************************************************

# the second part: numbers for every season

cleaned_old <- map_df(2014:2019, function(x) {
    message(glue::glue("Getting season {x}. . ."))
    load_season(x)
  })

# bind 2020 data to the prior season data
cleaned_all <- bind_rows(
  cleaned,
  cleaned_old
)


# for looking at a team over time
current <- cleaned_all %>%
  filter(go_boost > 1.5) %>%
  filter(prior_wp > .2) %>%
  group_by(posteam, season) %>%
  summarize(go = mean(go), n = n()) %>%
  ungroup() %>%
  left_join(nflfastR::teams_colors_logos, by=c('posteam' = 'team_abbr')) %>%
  arrange(-go) %>%
  mutate(rank = 1:n()) %>%
  arrange(posteam, season)

means <- current %>%
  group_by(season) %>%
  summarize(league_go = mean(go)) %>%
  ungroup()

# function to make team timeline
make_timeline <- function(team) {
  
  prim <- nflfastR::teams_colors_logos %>% filter(team_abbr == team) %>% pull(team_color)
  sec <- nflfastR::teams_colors_logos %>% filter(team_abbr == team) %>% pull(team_color2)
  name <- nflfastR::teams_colors_logos %>% filter(team_abbr == team) %>% pull(team_nick)
  
  chart <- current %>%
    filter(posteam==team)
  teams <- current %>%
    filter(posteam != team)
  
  ### pass downs over time
  my_title <- glue::glue("How often do the <span style='color:red'>{name}</span> go for it when they <span style='color:red'>should?</span>")
  fig <- ggplot(data=chart, aes(x=season,y=go)) +
    geom_line(data=chart,
              aes(x=season,y=go),color=prim,size=3) +
    geom_point(data=chart,
               aes(x=season,y=go),color=sec,size=8) +
    geom_line(data=means,
              aes(x=season,y=league_go),color="black",size=1, linetype="dashed", alpha=.6) +
    geom_jitter(data=teams,
                aes(x=season,y=go), color=teams$team_color, size=4, alpha=.6, width = .045) +
    labs(
      x = "",
      y = "Go rate",
      title= my_title,
      subtitle = "When @ben_bot_baldwin recommends going for it (gain in win prob. at least 1.5 percentage points)",
      caption = glue::glue("Excl. final 30 seconds of game. Win prob >20%")
    ) +
    scale_x_continuous(breaks=c(min(chart$season):max(chart$season))) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme_bw()+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 18),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16),
          plot.title = element_markdown(size=24,face = 2,hjust=.5),
          plot.subtitle = element_text(size=12, hjust=.5))
  
  
  ggsave(glue::glue("figures/team_timelines/teams_timeline_{team}.png"), plot = fig)
  
  return(fig)
  
}

# can change this to any team. automatically saves imgine in figures/team_timelines/
make_timeline("BAL")


# for getting lost WP in an older season. change y to what you want (back to 2014)
y = 2018
# total WP lost
current <- cleaned_all %>%
  filter(go_boost > 0, go == 0, season == y) %>%
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

my_title <- glue::glue("Expected win probability <span style='color:red'>lost by kicking in go situations</span>, {y}")
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

ggsave(glue::glue("figures/teams_lost_{y}.png"))



# ********************************************************************
# total WP lost in playoff games
current <- cleaned_all %>%
  filter(go_boost > 0, go == 0, week > 17) %>%
  group_by(game_id, posteam, defteam) %>%
  summarize(
    go = sum(go_boost), 
    n = n(),
    season = dplyr::first(season),
    week = dplyr::first(week)
    ) %>%
  ungroup() %>%
  left_join(nflfastR::teams_colors_logos, by=c('posteam' = 'team_abbr')) %>%
  arrange(-go) %>%
  mutate(rank = 1:n()) %>%
  arrange(rank) %>%
  head(20) %>%
  select(rank, posteam, defteam, season, week, go) %>%
  mutate(week = case_when(
    week == 18 ~ "WC",
    week == 19 ~ "DIV",
    week == 20 ~ "CONF",
    week == 21 ~ "SB"
  ))

# to split decisions up into two columns so the figure isn't so long
d <- bind_cols(
  current %>% dplyr::slice(1:10),
  current %>% dplyr::slice(11:20)
)

t <- d %>%
  gt() %>%
    cols_label(
      rank = " ",
      posteam = "Team",
      defteam = "Opp",
      week = "Week",
      season = "Season",
      go = "WP Lost", 
      rank1 = " ",
      posteam1 = "Team",
      defteam1= "Opp",
      week1 = "Week",
      season1 = "Season",
      go1 = "WP Lost"
    ) %>%
    tab_style(
      style = cell_text(color = "black", weight = "bold"),
      locations = list(
        cells_row_groups(),
        cells_column_labels(everything())
      )
    ) %>% 
    text_transform(
      locations = cells_body(vars(posteam, posteam1, defteam, defteam1)),
      fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
    ) %>% 
    cols_width(
      everything() ~ px(400),
    ) %>% 
    cols_width(
      vars(posteam, posteam1) ~ px(50),
      vars(defteam, defteam1) ~ px(50),
      vars(week, week1) ~ px(50),
      vars(rank, rank1) ~ px(50),
      vars(season, season1) ~ px(70),
      vars(go, go1) ~ px(80)
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
      column_labels.border.bottom.width = px(2),
      row.striping.background_color = '#FFFFFF',
      row.striping.include_table_body = TRUE,
      table.background.color = '#F2F2F2'
    ) %>%
    fmt_number(
      columns = vars(go, go1), decimals = 1
    ) %>%
    cols_align(
      columns = 1:12,
      align = "center"
    ) %>% 
    tab_header(
      title = md(glue::glue("Win probability lost by kicking in playoff games, 2014-2020"))
    ) %>%
    tab_style(
      style = cell_borders(
        sides = c("left"),
        color = "#BBBBBB",
        weight = px(1.5),
        style = "solid"
      ),
      locations = cells_body(
        columns = vars(rank1),
        rows = everything()
      )
    )

t

t %>% gtsave("figures/team_worst_playoffs.png")


# ########### increasing aggressiveness over time

# labels on the plot
text_df <- tibble(
  label = c(
    "NFL coaches<br>in <span style='color:#00BFC4'>**2020**</span>",
    "NFL coaches<br>in <span style='color:#F8766D'>**2014**</span>"
  ),
  x = c(6, 8.2),
  y = c(80, 37),
  angle = c(10, 10),
  color = c("black", "black")
)


my_title <- glue::glue("How <span style='color:red'>math</span> is changing football")
cleaned_all %>%
  mutate(go = go * 100) %>%
  filter(prior_wp > .2) %>%
  filter(between(go_boost, -10, 10)) %>%
  filter(season %in% c(2014, 2020)) %>%
  ggplot(aes(go_boost, go, color = as.factor(season))) + 
  geom_richtext(data = text_df,   
                aes(
                  x, y, label = label, angle = angle
                ), color = "black", fill = NA, label.color = NA, size = 5
  ) + 
  
  geom_vline(xintercept = 0)+
  stat_smooth(method = "gam", method.args = list(gamma = 1), formula = y ~ s(x, bs = "cs", k = 10), show.legend = F, se = F, size = 4) +
  # this is just to get the plot to draw the full 0 to 100 range
  geom_hline(yintercept = 100, alpha = 0) +
  geom_hline(yintercept = 0, alpha = 0) +
  theme_fivethirtyeight()+
  labs(x = "Gain in win probability by going for it",
       y = "Go-for-it percentage",
       subtitle = "4th down decisions in 2020 versus 2014",
       caption = paste0("Figure: @benbbaldwin | WP gain from @ben_bot_baldwin\nWin prob. > 20%"),
       title = my_title) +
  theme(
    legend.position = "none",
    plot.title = element_markdown(size = 22, hjust = 0.5),
    plot.subtitle = element_markdown(size = 12, hjust = 0.5),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=12, face="bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4), expand = c(0,0)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(-10, 10), expand = c(0,0)) +
  

  annotate("text",x=-1.2, y= 70, label = "Should\nkick", color="black", size = 5) +
  annotate("text",x=1.2, y= 70, label = "Should\ngo for it", color="black", size = 5) +

  geom_segment(
    aes(x = -.1, y = 80, xend = -2, yend = 80),
    arrow = arrow(length = unit(0.05, "npc")),
    color = "black",
    size = 2
    ) +
  geom_segment(
    aes(x = .1, y = 80, xend = 2, yend = 80),
    arrow = arrow(length = unit(0.05, "npc")),
    color = "black",
    size = 2
  )


ggsave("figures/league_behavior_2014_2020.png")


