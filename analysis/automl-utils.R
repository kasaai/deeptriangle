automl_data_prep <- function(schedule_p_data, group_codes, train_validation_cutoff = 1995) {
  schedule_p_data %>%
    dplyr::right_join(group_codes, by = c("lob", "group_code")) %>%
    dplyr::mutate(case_reserves = .data$incurred_loss - .data$cumulative_paid_loss) %>%
    dplyr::group_by(.data$lob, .data$group_code, .data$accident_year) %>%
    dplyr::arrange(.data$lob, .data$group_code, .data$accident_year, .data$development_lag) %>%
    dplyr::mutate(
      incremental_paid_actual = .data$incremental_paid_loss,
      incremental_paid = ifelse(.data$calendar_year <= 1997,
                                .data$incremental_paid_actual, NA_real_),
      case_reserves_actual = .data$case_reserves,
      case_reserves = ifelse(.data$calendar_year <= 1997,
                             .data$case_reserves_actual, NA_real_)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      bucket = dplyr::case_when(
        .data$calendar_year <= !!train_validation_cutoff & .data$development_lag > 1 ~ "train",
        .data$calendar_year > !!train_validation_cutoff & .data$calendar_year <= 1997 &
          .data$development_lag > 1 ~ "validation",
        .data$calendar_year > 1997 ~ "test"
      )) %>%
    dplyr::mutate(
      incremental_paid = .data$incremental_paid / .data$earned_premium_net,
      incremental_paid_actual = .data$incremental_paid_actual / .data$earned_premium_net,
      case_reserves = .data$case_reserves / .data$earned_premium_net,
      case_reserves_actual = .data$case_reserves_actual / .data$earned_premium_net
    ) %>%
    dplyr::ungroup() %>%
    (function(x) split(x, x$lob)) %>%
    purrr::map(~ .x %>%
                 dplyr::mutate(group_code = factor(.data$group_code))
    )
}


lags <- function(var, n = 10){
  var <- rlang::enquo(var)
  indices <- seq_len(n)
  map( indices, ~rlang::quo(lag(!!var, !!.x)) ) %>%
    set_names(sprintf("lag_%s_%02d", rlang::quo_text(var), indices))
}

automl_train_validation_split <- function(data) {
  training_data <- dplyr::filter(
    data,
    .data$bucket == "train",
    .data$development_lag > 1
  )

  validation_data <- dplyr::filter(
    data,
    .data$bucket == "validation",
    .data$development_lag > 1
  )

  full_training_data <- dplyr::filter(
    data,
    .data$bucket %in% c("train", "validation")
  )

  list(training_data, validation_data, full_training_data)
}

automl_predict <- function(model, data) {
  results <- dplyr::tibble(
    lob = character(),
    group_code = factor(levels = levels(data$group_code)),
    accident_year = integer(), development_lag = integer(),
    predicted_loss = numeric()
  )

  purrr::walk(1998:2006, function(cy) {
    df <- data %>%
      dplyr::filter(.data$calendar_year <= !!cy) %>%
      dplyr::left_join(results,
                       by =  c("lob", "group_code", "accident_year",
                               "development_lag")
      ) %>%
      dplyr::mutate(
        incremental_paid = ifelse(is.na(.data$predicted_loss),
                                  .data$incremental_paid,
                                  .data$predicted_loss)
      ) %>%
      dplyr::select(-.data$predicted_loss) %>%
      mutate(!!!lags(incremental_paid, 9)) %>%
      dplyr::filter(.data$calendar_year == !!cy)
    df_h2o <- as.h2o(df)
    predictions <- model %>%
      stats::predict(df_h2o) %>%
      as_tibble() %>%
      rename(predicted_loss = predict)
    h2o.rm(df_h2o)
    results <<- dplyr::bind_rows(results, dplyr::bind_cols(df, predictions)) %>%
      dplyr::select(.data$lob, .data$group_code, .data$accident_year,
                    .data$development_lag, .data$predicted_loss)
  })
  results
}

automl_compute_predictions <- function(model, data) {
  predictions <- automl_predict(model, data)

  predictions_df <- data %>%
    dplyr::left_join(
      dplyr::select(predictions, .data$group_code, .data$accident_year,
                    .data$development_lag, .data$predicted_loss),
      by = c("group_code", "accident_year", "development_lag")
    ) %>%
    dplyr::group_by(.data$group_code, .data$accident_year) %>%
    dplyr::arrange(.data$group_code, .data$accident_year, .data$development_lag) %>%
    dplyr::mutate(
      predicted_loss = ifelse(is.na(.data$predicted_loss),
                              .data$incremental_paid, .data$predicted_loss)
    ) %>%
    dplyr::mutate(
      predicted_cumulative_loss = cumsum(.data$predicted_loss) * .data$earned_premium_net
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(group_code = as.character(.data$group_code)) %>%
    tidyr::gather("type", "value", .data$predicted_cumulative_loss,
                  .data$cumulative_paid_loss,
                  na.rm = TRUE) %>%
    dplyr::mutate(obs_type = dplyr::case_when(
      grepl("predicted", .data$type) ~ "prediction",
      .data$calendar_year <= 1997 ~ "observed",
      .data$calendar_year > 1997 ~ "holdout",
      TRUE ~ "observed"
    ))
}

automl_compute_metrics <- function(predictions) {
  predictions %>%
    dplyr::filter(
      .data$development_lag == 10,
      .data$type %in% c("cumulative_paid_loss", "predicted_cumulative_loss")
    ) %>%
    dplyr::group_by(.data$lob, .data$group_code, .data$type) %>%
    dplyr::summarize(ultimate = sum(.data$value)) %>%
    tidyr::spread(.data$type, .data$ultimate) %>%
    dplyr::mutate(
      pct_error = (.data$predicted_cumulative_loss - .data$cumulative_paid_loss) /
        .data$cumulative_paid_loss) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$lob) %>%
    dplyr::summarize(
      mape = mean(abs(.data$pct_error)),
      rmspe = sqrt(mean(.data$pct_error ^ 2))
    ) %>%
    dplyr::mutate(model = "AutoML")
}
