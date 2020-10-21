library(tidyverse)

# for getting data ready for the model
source('https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_nflscrapr_mutations.R')

# **************************************************************************************
# data
seasons <- 2014:2019
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
}) %>%
  filter(
    down %in% c(3,4), 
    qb_kneel == 0,
    rush == 1 | pass == 1, 
    !is.na(posteam),
    !is.na(yardline_100),
    !is.na(score_differential),
    week <= 17
  ) %>%
  make_model_mutations() 

model_vars <- pbp %>% 
  mutate(yards_gained = 
           
           # we need a way to account for defensive penalties that give auto first downs
           # we're saying here that a penalty that gives a first down goes for the yards to go
           # unless the actual penalty yardage is higher
           # the draw back is that a defensive holding on eg 4th and 8 is coded as an 8 yard gain
           # but the alternative is to estimate a separate model for penalties and that is too much
           case_when(
             first_down_penalty == 1 & penalty_yards < ydstogo ~ ydstogo, 
             first_down_penalty == 1 & penalty_yards >= ydstogo ~ penalty_yards, 
             TRUE ~ yards_gained
           ),
         # truncate to make model training easier
         yards_gained = if_else(yards_gained < -10, -10, yards_gained),
         yards_gained = if_else(yards_gained > 65, 65, yards_gained),
         home_total = (spread_line + total_line) / 2,
         away_total = (total_line - spread_line) / 2,
         posteam_total = if_else(posteam == home_team, home_total, away_total),
         posteam_spread = dplyr::if_else(posteam == home_team, spread_line, -1 * spread_line)
  ) %>%
  # look at when an actual play is run or a defensive penalty gives a first down
  filter(play_type_nfl %in% c("RUSH", "PASS", "SACK") | first_down_penalty == 1) %>%
  mutate(label = yards_gained) %>%
  select(
    label,
    down,
    ydstogo,
    yardline_100,
    era3, era4,
    outdoors, retractable, dome,
    posteam_spread, total_line, posteam_total
  ) %>%
  # 0 = 10 yard loss
  mutate(label = label + 10)

# **************************************************************************************
# tune
set.seed(2013)

full_train = xgboost::xgb.DMatrix(model.matrix(~.+0, data = model_vars %>% dplyr::select(-label)), label = as.integer(model_vars$label))

#params
nrounds = 2000

x = c(2) #max depth
y = c(.01) #something else to tune

search <- map_df(cross2(x, y), function(x) {
  
  depth = x[[1]]
  eta = x[[2]]
  
  print(message(glue::glue('max depth {depth} and eta {eta}')))
  
  params <-
    list(
      booster = "gbtree",
      objective = "multi:softprob",
      eval_metric = c("merror", "mlogloss"),
      num_class = 76,
      eta = eta,
      gamma = 2,
      subsample=0.8,
      colsample_bytree=0.8,
      max_depth = 2,
      min_child_weight = 0.8
    )
  
  #train
  fd_cv_model <- xgboost::xgb.cv(data = full_train, params = params, nrounds = nrounds,
                                 nfold = 10, metrics = list("merror", "mlogloss"),
                                 early_stopping_rounds = 5, print_every_n = 10)
  
  iter = fd_cv_model$best_iteration
  
  result <- data.frame(
    'eta' = eta,
    'iter' = iter,
    'logloss' = fd_cv_model$evaluation_log[iter]$test_mlogloss_mean,
    'error' = fd_cv_model$evaluation_log[iter]$test_merror_mean,
    'gamma' = 2,
    'max_depth' = 2,
    'min_child_weight' = 0.8,
    'subsample' = 0.8,
    'colsample' = 0.8
  ) %>%
    as_tibble()
  
  return(result)
  
})


search %>%
  arrange(logloss) 


# best
best <- search %>% 
  arrange(logloss) %>%
  dplyr::slice(1)

message(
  glue::glue("
  merror: {best$error}
  mloglos: {best$logloss}
  iter: {best$iter}
  eta: {best$eta}
  gamma: {best$gamma}
  depth: {best$max_depth}
  weight: {best$min_child_weight}
             ")
)

# [1124] test-merror:0.676584+0.008136	test-mlogloss:2.858089+0.027665

# **************************************************************************************
# train

nrounds = 1124
params <-
  list(
    booster = "gbtree",
    objective = "multi:softprob",
    eval_metric = c("mlogloss"),
    num_class = 76,
    eta = .01,
    gamma = 2,
    subsample=0.8,
    colsample_bytree=0.8,
    max_depth = 2,
    min_child_weight = 0.8
  )

full_train = xgboost::xgb.DMatrix(model.matrix(~.+0, data = model_vars %>% dplyr::select(-label)), label = as.integer(model_vars$label))
fd_model <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)

save(fd_model, file = 'data/fd_model.Rdata')

importance <- xgboost::xgb.importance(feature_names = colnames(fd_model), model = fd_model)
xgboost::xgb.ggplot.importance(importance_matrix = importance)

