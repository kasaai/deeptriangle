#' Given a time series, return a list
#'  where each element is a vector representing a window
#'  of the time series determined by the offsets
make_series <- function(v, start_offset, end_offset, na_pad = -99) {
  prepad_mask <- function(v, l = 9) {
    length_diff <- l - length(v)
    if (length_diff > 0) {
      c(rep(na_pad, length_diff), v)
    } else {
      v
    }
  }

  purrr::map(
    seq_along(v),
    function(x) {
      start <- max(0, x + start_offset)
      end <- max(0, x + end_offset)
      out <- v[start:end]
      ifelse(is.na(out), na_pad, out)
    } %>%
      prepad_mask()
  )
}

mutate_series <- function(data, timesteps = 9) {
  data %>%
    dplyr::group_by(.data$lob, .data$group_code, .data$accident_year) %>%
    dplyr::arrange(.data$lob, .data$group_code, .data$accident_year, .data$development_lag) %>%
    mutate(
      paid_lags = make_series(incremental_paid, -timesteps, -1),
      case_lags = make_series(case_reserves, -timesteps, -1),
      paid_target = make_series(incremental_paid, 0, timesteps - 1),
      case_target = make_series(case_reserves, 0, timesteps - 1),
    ) %>%
    ungroup()
}

prep_keras_data <- function(data, company_index_recipe) {
  lags <- data %>%
    select(.data$paid_lags, .data$case_lags) %>%
    purrr::transpose() %>%
    purrr::map(~ array(unlist(.x), dim = c(1, 9, 2))) %>%
    abind::abind(along = 1) %>%
    unname()

  target_paid <- data %>%
    pull(.data$paid_target) %>%
    flatten_dbl() %>%
    array_reshape(c(nrow(data), 9, 1))

  target_case <- data %>%
    pull(.data$case_target) %>%
    flatten_dbl() %>%
    array_reshape(c(nrow(data), 9, 1))

  company_input <- bake(company_index_recipe, data) %>% as.matrix()

  list(
    x = list(
      ay_seq_input = lags, company_input = company_input
    ),
    y = list(
      paid_output = target_paid,
      case_reserves_output = target_case
    )
  )
}

masked_mse <- function(mask_value) {
  function(y_true, y_pred) {
    keep_value <- k_cast(k_not_equal(y_true, mask_value), k_floatx())
    sum_squared_error <- k_sum(
      k_square(keep_value * (y_true - y_pred)),
      axis = 2
    )
    sum_squared_error / k_sum(keep_value, axis = 2)
  }
}

transform_preds <- function(preds) {
  rows <- 1:dim(preds[[1]])[[1]]
  list(
    # predicted_cumulative_loss = map(rows, ~ preds[[1]][, , 2][.x, ]),
    predicted_loss = map(rows, ~ preds[[1]][, , 1][.x, ]),
    predicted_os = map(rows, ~ preds[[2]][, , 1][.x, ])
  ) %>%
    as_tibble()
}

extract_keras_data <- function(data, lob) {
  data %>%
    filter(lob == !!lob) %>%
    pull(.data$keras_data) %>%
    flatten()
}

extract_data <- function(data, lob) {
  data %>%
    filter(lob == !!lob) %>%
    unnest(.data$data)
}

compute_predictions <- function(model, test_data, lob) {
  predictions <- model %>%
    predict(extract_keras_data(test_data, lob)$x) %>%
    transform_preds()

  test_data %>%
    unnest(data) %>%
    select(group_code, accident_year, development_lag) %>%
    bind_cols(predictions) %>%
    unnest(predicted_loss, predicted_os) %>%
    group_by(group_code, accident_year) %>%
    mutate(development_lag = development_lag + row_number() - 1) %>%
    filter(development_lag <= 10) %>%
    right_join(
      data_with_features %>%
        filter(lob == !!lob),
      by = c("group_code", "accident_year", "development_lag")
    ) %>%
    arrange(group_code, accident_year, development_lag) %>%
    mutate(
      predicted_os = if_else(
        is.na(predicted_os),
        case_reserves, predicted_os
      ),
      predicted_loss = if_else(
        is.na(predicted_loss),
        incremental_paid, predicted_loss
      )
    ) %>%
    mutate(
      predicted_cumulative_loss = cumsum(predicted_loss) * earned_premium_net,
      predicted_os = predicted_os * earned_premium_net,
      case_reserves_actual = case_reserves_actual * earned_premium_net
    ) %>%
    ungroup() %>%
    gather(
      "type", "value", predicted_cumulative_loss, predicted_os,
      cumulative_paid_loss, case_reserves_actual,
      na.rm = TRUE
    ) %>%
    mutate(
      obs_type = case_when(
        grepl("predicted", type) ~ "prediction",
        calendar_year <= 1997 ~ "observed",
        calendar_year > 1997 ~ "holdout",
        TRUE ~ "observed"
      )
    )
}
