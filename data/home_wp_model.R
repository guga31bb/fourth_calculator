# # # # # # # # # # # # # #
# 1. build the data

  library(tidyverse)
  future::plan("multisession")

  data <- nflfastR::load_pbp(2001:2021) %>%
    mutate(
      # to fix any bugs
      result = home_score - away_score,
      home_posteam = ifelse(posteam == home_team, 1, 0),
      label = ifelse(result > 0, 1, 0)
    )

  #for estimating the models, apply some filters
  pbp_data <- data %>%
    filter(
      # 1st & 10 or 1st & goal
      down == 1,
      ydstogo == 10 | ydstogo == yardline_100,
      !is.na(game_seconds_remaining),
      !is.na(yardline_100),
      !is.na(score_differential),
      qtr <= 4,
      # game finished
      !is.na(result),
      # no ties *shrug emoji*
      result != 0,
      !is.na(posteam),
      posteam != "",
      !is.na(epa)
    ) %>%
    mutate(
      home_score_differential = total_home_score - total_away_score,
      home_ep = ifelse(posteam == home_team, ep, -ep),
      home_yardline_100 = ifelse(posteam == home_team, yardline_100, 100 - yardline_100)
    ) %>%
    #to keep file size manageable
    select(
      game_id,
      play_type,
      game_seconds_remaining,
      half_seconds_remaining,
      home_yardline_100,
      roof,
      posteam,
      defteam,
      home_team,
      away_team,
      home_posteam,
      ydstogo,
      season,
      qtr,
      down,
      week,
      drive,
      home_ep,
      home_score_differential,
      home_timeouts_remaining,
      away_timeouts_remaining,
      desc,
      label,
      spread_line,
      total_line
    )

  prepare_wp_data <- function(pbp) {

    pbp <- pbp %>%
      dplyr::group_by(.data$game_id) %>%
      dplyr::mutate(
        home_receive_2h_ko = dplyr::case_when(
          # home got it first
          .data$qtr <= 2 & .data$home_team == dplyr::first(stats::na.omit(.data$posteam)) ~ -1,
          # away got it first
          .data$qtr <= 2 & .data$away_team == dplyr::first(stats::na.omit(.data$posteam)) ~ 1,
          # it's the 2nd half
          TRUE ~ 0
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        elapsed_share = (3600 - .data$game_seconds_remaining) / 3600,
        spread_time = .data$spread_line * exp(-4 * .data$elapsed_share),
        Diff_Time_Ratio = .data$home_score_differential / (exp(-4 * .data$elapsed_share))
      )

    return(pbp)

  }

  df <- pbp_data %>%
    prepare_wp_data()

  #for doing calibation etc
  saveRDS(df, 'data/cal_data_home.rds')


# # # #
# tune the model

  library(tidyverse)
  set.seed(2013)

  # get everything ready
  model_data <- readRDS('data/cal_data_home.rds')

  folds <- splitTools::create_folds(model_data$game_id, k = 10, type = "grouped", invert = TRUE)

  model_data <- model_data %>%
    select(
      label,
      home_receive_2h_ko,
      spread_time,
      home_posteam,
      half_seconds_remaining,
      game_seconds_remaining,
      Diff_Time_Ratio,
      home_score_differential,
      home_ep,
      ydstogo,
      home_yardline_100,
      home_timeouts_remaining,
      home_timeouts_remaining
    )


  full_train = xgboost::xgb.DMatrix(model.matrix(~.+0, data = model_data %>% select(-label)),
                                    label = model_data$label)

  #params
  nrounds = 15000

  grid <- dials::grid_latin_hypercube(
    dials::finalize(dials::mtry(range = c(2, 7)), model_data %>% select(-label)),
    dials::min_n(),
    # dials::tree_depth(range = c(4, 6)),
    # dials::learn_rate(range = c(-2.5, -1.5), trans = scales::log10_trans()),
    dials::loss_reduction(range = c(.5, 1.3), trans = scales::log10_trans()),
    sample_size = dials::sample_prop(range = c(0.2, 1)),
    size = 40
  ) %>%
    mutate(
      # has to be between 0 and 1
      mtry = mtry / length(model_data  %>% select(-label)),
      tree_depth = 5,
      # probably use 0.01 later
      learn_rate = 0.01
    )

  grid %>%
    head(20)

  get_metrics <- function(df, row = 1) {

    # testing only
    # df <- grid %>% dplyr::slice(1)

    params <-
      list(
        booster = "gbtree",
        objective = "binary:logistic",
        eval_metric = c("logloss"),
        eta = df$learn_rate,
        gamma = df$loss_reduction,
        subsample= df$sample_size,
        colsample_bytree= df$mtry,
        max_depth = df$tree_depth,
        min_child_weight = df$min_n,
        monotone_constraints = "(0, 0, 1, 0, 0, 1, 1, 1, 0, -1, 1, -1)"
      )

    # home_receive_2h_ko, 0
    # spread_time, 0
    # home_posteam, 1

    # half_seconds_remaining, 0
    # game_seconds_remaining, 0
    # Diff_Time_Ratio, 1

    # home_score_differential, 1
    # home_ep, 1
    # ydstogo, 0

    # home_yardline_100, -1
    # home_timeouts_remaining, 1
    # away_timeouts_remaining, -1

    #train
    wp_cv_model <- xgboost::xgb.cv(data = full_train, params = params, nrounds = nrounds,
                                   folds = folds, metrics = list("logloss"),
                                   early_stopping_rounds = 50, print_every_n = 50)

    output <- params
    output$iter = wp_cv_model$best_iteration
    output$logloss = wp_cv_model$evaluation_log[output$iter]$test_logloss_mean
    output$error = wp_cv_model$evaluation_log[output$iter]$test_error_mean

    this_param <- bind_rows(output)

    if (row == 1) {
      saveRDS(this_param, "data/modeling.rds")
    } else {
      prev <- readRDS("data/modeling.rds")
      for_save <- bind_rows(prev, this_param)
      saveRDS(for_save, "data/modeling.rds")
    }

    return(this_param)

  }


  # get results
  results <- map_df(1 : nrow(grid), function(x) {

    gc()
    message(glue::glue("Row {x}"))
    get_metrics(grid %>% dplyr::slice(x), row = x)

  })

  results <- readRDS("data/modeling.rds")

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
    theme_minimal() +
    theme(
      strip.text = element_text(size = 16, face = "bold"),
      panel.background = element_rect(color = "black", linetype = "solid")
    )

# # # # # #
# train final model


  # data for train
  model_data <-
    readRDS('data/cal_data_home.rds')

  model_data <- model_data %>%
    select(
      label,
      home_receive_2h_ko,
      spread_time,
      home_posteam,
      half_seconds_remaining,
      game_seconds_remaining,
      Diff_Time_Ratio,
      home_score_differential,
      home_ep,
      ydstogo,
      home_yardline_100,
      home_timeouts_remaining,
      home_timeouts_remaining
    )

  train_data <- model_data %>% select(-label)
  train_labels <- model_data %>% select(label)

  # load model

  results <- readRDS("data/modeling.rds")

  best_model <- results %>%
    arrange(logloss) %>%
    dplyr::slice(1)

  best_model

  best_model$logloss

  params <-
    list(
      booster = "gbtree",
      objective = "binary:logistic",
      eval_metric = c("logloss"),
      eta = best_model$eta,
      gamma = best_model$gamma,
      subsample = best_model$subsample,
      colsample_bytree = best_model$colsample_bytree,
      max_depth = best_model$max_depth,
      min_child_weight = best_model$min_child_weight,
      monotone_constraints = best_model$monotone_constraints
    )

  nrounds <- best_model$iter

  params

  nrounds

  # train

  wp_model <- xgboost::xgboost(
    params = params,
    data = as.matrix(train_data),
    label = train_labels$label,
    nrounds = nrounds,
    verbose = 2
  )

  xgboost::xgb.save(wp_model, "data/home_wp_model.ubj")
  wp_model <- xgboost::xgb.load("data/home_wp_model.ubj") |> xgboost::xgb.Booster.complete()
  save(wp_model, file = 'data/home_wp_model.Rdata')
