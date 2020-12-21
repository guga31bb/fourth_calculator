library(tidyverse)
library(tidymodels)
source('https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_ep_wp.R')
source('https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_nflscrapr_mutations.R')

set.seed(2013)

# read in data from data repo
# just doing 20 season to make fold splitting easier
pbp_data <- purrr::map_df(2000 : 2019, function(x) {
  readRDS(
    # from repo
    url(glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds"))
  ) %>% filter(qtr > 4)
}) %>%
  mutate(
    Winner = if_else(home_score > away_score, home_team,
                     if_else(home_score < away_score, away_team, "TIE"))
  )

#for estimating the models, apply some filters
pbp_data <- pbp_data %>%
  filter(play_type %in% c("field_goal", "no_play", "pass", "punt", "run",
                          "qb_spike") & is.na(two_point_conv_result) & is.na(extra_point_result) &
           !is.na(down) & !is.na(game_seconds_remaining)) %>%
  #to keep file size manageable
  select(
    game_id,
    game_seconds_remaining,
    quarter_seconds_remaining,
    yardline_100,
    fixed_drive,
    roof,
    posteam,
    defteam,
    home_team,
    ydstogo,
    season,
    qtr,
    down,
    week,
    ep,
    score_differential,
    posteam_timeouts_remaining,
    defteam_timeouts_remaining,
    desc,
    Winner,
    spread_line,
    total_line
  )

#for doing calibation etc
saveRDS(pbp_data, 'data/cal_data_overtime.rds')


model_data <-
  readRDS('data/cal_data_overtime.rds') %>%
  make_model_mutations() %>%
  prepare_wp_data() %>%
  mutate(
    label = case_when(
      Winner == posteam ~ 0,
      Winner == defteam ~ 1,
      Winner == "TIE" ~ 2
    ),
    regime = case_when(
      # old regime where FG on opening drive won game
      season < 2010 | (between(season, 2010, 2011) & week <= 17) ~ 0,
      TRUE ~ 1
    ),
    game_type_reg = if_else(week <= 17, 1, 0)
  ) %>%
  group_by(game_id) %>%
  mutate(
    first_ot_drive = if_else(fixed_drive == min(fixed_drive), 1, 0),
    # if team is in field goal range and can win game with field goal
    can_win = if_else(
      (first_ot_drive == 0 | regime == 0) & yardline_100 < 35 & score_differential == 0,
      1, 0
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(ep) & !is.na(score_differential) & !is.na(label) & !is.na(yardline_100)) %>%
  select(
    label,
    # just for checking data
    # game_id, posteam, Winner,
    first_ot_drive,
    can_win,
    regime,
    spread_time,
    game_type_reg,
    home,
    quarter_seconds_remaining,
    score_differential,
    down,
    ydstogo,
    yardline_100,
    posteam_timeouts_remaining,
    defteam_timeouts_remaining,
    season
  )


folds <- map(0:9, function(x) {
  f <- which(model_data$season %in% c(2000 + x, 2010 + x))
  return(f)
})


full_train = xgboost::xgb.DMatrix(model.matrix(~.+0, data = model_data %>% select(-label, -season)),
                                  label = model_data$label)

#params
nrounds = 15000

grid <- dials::grid_latin_hypercube(
  dials::finalize(dials::mtry(), model_data %>% select(-season, -label)),
  dials::min_n(),
  # dials::tree_depth(),
  dials::learn_rate(range = c(-3, -1), trans = scales::log10_trans()),
  dials::loss_reduction(),
  # sample_size = dials::sample_prop(),
  size = 20
) %>%
  mutate(
    # has to be between 0 and 1
    mtry = mtry / length(model_data  %>% select(-season, -label)),
    sample_size = 0.965,
    tree_depth = 2
  )


grid %>%
  head(20)


# function to search over hyperparameter grid
get_metrics <- function(df, row = 1) {
  
  # testing only
  # df <- grid %>% dplyr::slice(1)
  
  params <-
    list(
      booster = "gbtree",
      objective = "multi:softprob",
      eval_metric = c("mlogloss"),
      num_class = 3,
      eta = df$learn_rate,
      gamma = df$loss_reduction,
      subsample= df$sample_size,
      colsample_bytree= df$mtry,
      max_depth = df$tree_depth,
      min_child_weight = df$min_n
    )
  
  #train
  wp_cv_model <- xgboost::xgb.cv(data = full_train, params = params, nrounds = nrounds,
                                 folds = folds, metrics = list("mlogloss"),
                                 early_stopping_rounds = 20, print_every_n = 50)
  
  output <- params
  output$iter = wp_cv_model$best_iteration
  output$logloss = wp_cv_model$evaluation_log[output$iter]$test_mlogloss_mean
  
  this_param <- bind_rows(output)
  
  if (row == 1) {
    saveRDS(this_param, "modeling.rds")
  } else {
    prev <- readRDS("modeling.rds")
    for_save <- bind_rows(prev, this_param)
    saveRDS(for_save, "modeling.rds")
  }
  
  return(this_param)
  
}

# do this piece by piece so server doesn't die
# actual code:
# 1 : nrow(grid)


# get results
results <- map_df(1 : nrow(grid), function(x) {
  
  gc()
  message(glue::glue("Row {x}"))
  get_metrics(grid %>% dplyr::slice(x), row = x)
  
})

results <- readRDS('modeling.rds')

# plot
results %>%
  select(logloss, eta, gamma, subsample, colsample_bytree, max_depth, min_child_weight) %>%
  pivot_longer(eta:min_child_weight,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, logloss, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE, size = 3) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "logloss") +
  theme_minimal()

results %>% arrange(logloss) %>% dplyr::slice(1)
results %>% arrange(logloss) %>% dplyr::slice(1) %>% dplyr::pull(logloss)
# [1] 0.7530039

nrounds <- 1040
params <-
  list(
    booster = "gbtree",
    objective = "multi:softprob",
    eval_metric = c("mlogloss"),
    num_class = 3,
    eta = 0.00738,
    gamma = 0.271,
    subsample= 0.965,
    colsample_bytree= 0.462,
    max_depth = 2,
    min_child_weight = 14
  )

wp_model_ot <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)

importance <- xgboost::xgb.importance(feature_names = colnames(wp_model_ot), model = wp_model_ot)
xgboost::xgb.ggplot.importance(importance_matrix = importance)

save(wp_model_ot, file = 'data/wp_model_ot.Rdata')


