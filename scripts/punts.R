library(tidyverse)
library(viridis)
options(scipen = 999999)

# using more seasons for the punt model to help smooth out blocks/td returns some
seasons <- 2010:2019
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  ) %>% filter(play_type_nfl == "PUNT")
})

# thank you to Thomas Mock for the function
# https://themockup.blog/posts/2020-08-28-heatmaps-in-ggplot2/
# maybe there's a better way to do this but calling it good enough for now
get_density <- function(x, y, ...) {
  density_out <- MASS::kde2d(x, y, ...)
  int_x <- findInterval(x, density_out$x)
  int_y <- findInterval(y, density_out$y)
  comb_int <- cbind(int_x, int_y)
  return(density_out$z[comb_int])
}

# figure out before and after of each punt
points <- pbp %>%
  select(desc, yardline_100, kick_distance, return_yards, fumble_lost) %>%
  mutate(
    # give return yards too
    yardline_after = yardline_100 - kick_distance + return_yards,
    yardline_after = 
      if_else(
        stringr::str_detect(desc, "end zone") & is.na(kick_distance), 20, yardline_after
      ),
    # for blocked punts, just give them the ball there
    yardline_after = if_else(stringr::str_detect(desc, "BLOCKED") & is.na(yardline_after), yardline_100, yardline_after),
    # make it in the actual field of play
    yardline_after = if_else(yardline_after > 100, 100, yardline_after),
    # there's 2 safeties that are too annoying to deal with
    yardline_after = if_else(yardline_after == 0, 1, yardline_after),
    blocked = if_else(stringr::str_detect(desc, "BLOCKED") == 1, 1, 0),
    return_td = if_else(yardline_after == 100, 1, 0),
    # there's 1 play where there was a fumble lost after a blocked punt
    # this isn't a muffed punt
    fumble_lost = if_else(blocked == 1, 0, fumble_lost)
  ) %>%
  # there's like 10 of these for some reason
  filter(!is.na(yardline_after)) %>%
  select(desc, yardline_100, yardline_after, blocked, return_td, fumble_lost)

points

# first, bin by yardline to get blocked and return TD pct
# another way to do this would be a smoother but these are so rare
# hopefully it doesn't matter
outliers <- points %>%
  group_by(yardline_100) %>%
  summarize(
    muffed = sum(fumble_lost),
    blocked = sum(blocked),
    return_td = sum(return_td),
    n = n()
  ) %>%
  ungroup() %>%
  mutate(
    bin = case_when(
      yardline_100 < 40 ~ 0,
      between(yardline_100, 40, 49) ~ 1,
      between(yardline_100, 50, 59) ~ 2,
      between(yardline_100, 60, 69) ~ 3,
      between(yardline_100, 70, 79) ~ 4,
      between(yardline_100, 80, 89) ~ 5,
      between(yardline_100, 90, 99) ~ 6
    )
  ) %>%
  group_by(bin) %>%
  mutate(
    muffed = sum(muffed),
    blocked = sum(blocked),
    return_td = sum(return_td),
    n = sum(n),
    bin_muffed_pct = muffed / n,
    bin_blocked_pct = blocked / n,
    bin_td_pct = return_td / n,
  ) %>%
  ungroup()

return_tds <- outliers %>%
  mutate(
    yardline_after = 100,
    density = bin_blocked_pct
  ) %>%
  select(yardline_100, yardline_after, density) %>%
  filter(density > 0)

blocks <- outliers %>%
  mutate(
    # not used for anything except to pick these out later
    yardline_after = 999,
    density = bin_td_pct
  ) %>%
  select(yardline_100, yardline_after, density) %>%
  filter(density > 0)

# get density excluding blocks and returns. will add those later
density_map_normal <- points %>%
  filter(blocked == 0 & return_td == 0) %>%
  select(yardline_100, yardline_after) %>%
  mutate(density = get_density(yardline_100, yardline_after, n = 100))

# get final percentages
df <- density_map_normal %>%
  group_by(yardline_100, yardline_after) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  arrange(yardline_100, yardline_after) %>%
  group_by(yardline_100) %>%
  mutate(
    tot_dens = sum(density),
    pct = density / tot_dens
  ) %>%
  ungroup() %>%
  bind_rows(blocks) %>%
  bind_rows(return_tds) %>%
  arrange(yardline_100, yardline_after) %>%
  group_by(yardline_100) %>%
  mutate(
    outlier_pct = sum(density * (yardline_after == 100)) + sum(density * (yardline_after == 999)),
    non_outlier_pct = 1 - outlier_pct,
    pct = pct * non_outlier_pct,
    pct = if_else(is.na(pct), density, pct),
    yardline_after = if_else(yardline_after == 999, yardline_100, yardline_after)
  ) %>%
  ungroup() %>%
  left_join(
    outliers %>% select(yardline_100, bin_muffed_pct), by = "yardline_100"
  ) %>%
  arrange(yardline_100, yardline_after) %>%
  select(yardline_100, yardline_after, pct, bin_muffed_pct) %>%
  filter(yardline_100 > 30) 

bind_rows(
  # get a df without the return and blocked probs
  df %>% 
    filter(yardline_after != 100 & yardline_100 != yardline_after),
  df
) %>%
  arrange(yardline_100, yardline_after) %>%
  group_by(yardline_100, yardline_after) %>%
  mutate(
    muff = 1 : n() - 1,
    pct = if_else(muff == 1, bin_muffed_pct * pct, pct),
    pct = if_else(
      muff == 0 & yardline_after != 100 & yardline_100 != yardline_after, (1 - bin_muffed_pct) * pct, pct
    )
  ) %>%
  # one last making sure all the pct add up to 1
  group_by(yardline_100) %>%
  mutate(tot_pct = sum(pct), pct = pct / tot_pct) %>%
  ungroup() %>%
  select(
    yardline_100, yardline_after, pct, muff
  ) %>%
  saveRDS('data/punt_data.rds')


# **************************************************************************************
# field goals
seasons <- 2014:2019

pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  ) %>%
    filter(
      play_type_nfl == "FIELD_GOAL"
    )
}) %>%
  mutate(
    roof = if_else(roof %in% c("open", "closed"), "retractable", roof),
    model_roof = as.factor(roof)
  )

#estimate model
fg_model <- mgcv::bam(sp ~ s(yardline_100, by = interaction(model_roof)) + model_roof,
                      data = pbp, family = "binomial")

save(fg_model, file = 'data/fg_model.Rdata')

