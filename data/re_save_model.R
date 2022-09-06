
# model made here
# https://github.com/nflverse/nfl4th/blob/master/data-raw/_go_for_it_and_2pt_models.R#L181

load_model <- function() {
  fd_model <- NULL
  con <- url("https://github.com/guga31bb/fourth_calculator/blob/main/data/fd_model.Rdata?raw=true")
  try(load(con), silent = TRUE)
  close(con)
  fd_model
}

fd_model <- load_model()

# xgboost >= 1.6.0 warned the user because of old serialization formats.
# So we save the models in the suggested serialized json format, read them
# back in and save the in the package again.
xgboost::xgb.save(fd_model, "data/fd_model.ubj")

fd_model <- xgboost::xgb.load("data/fd_model.ubj") |> xgboost::xgb.Booster.complete()

save(fd_model, file = 'data/fd_model_v2.Rdata')
