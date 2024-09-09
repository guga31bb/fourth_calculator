
# model made here
# https://github.com/nflverse/nfl4th/blob/master/data-raw/_go_for_it_and_2pt_models.R#L181

# place in nfl4th where models are loaded
# https://github.com/nflverse/nfl4th/blob/master/R/helpers.R#L219

load_fd_model <- function() {
  fd_model <- NULL
  con <- url("https://github.com/guga31bb/fourth_calculator/blob/main/data/fd_model_v2.Rdata?raw=true")
  try(load(con), silent = TRUE)
  close(con)
  fd_model
}

load_wp_model <- function() {
  wp_model <- NULL
  con <- url("https://github.com/guga31bb/fourth_calculator/blob/main/data/home_wp_model.Rdata?raw=true")
  try(load(con), silent = TRUE)
  close(con)
  wp_model
}

# load models
  fd_model <- load_fd_model()
  wp_model <- load_wp_model()

# xgboost >= 1.6.0 warned the user because of old serialization formats.
# So we save the models in the suggested serialized json format, read them
# back in and save the in the package again.
  xgboost::xgb.save(fd_model, "data/fd_model.ubj")
  xgboost::xgb.save(wp_model, "data/wp_model.ubj")

# load
  fd_model <- xgboost::xgb.load("data/fd_model.ubj") |> xgboost::xgb.Booster.complete()
  wp_model <- xgboost::xgb.load("data/wp_model.ubj") |> xgboost::xgb.Booster.complete()

# re-save
  save(fd_model, file = 'data/fd_model_v2.Rdata')
  save(wp_model, file = 'data/home_wp_model.Rdata')
  