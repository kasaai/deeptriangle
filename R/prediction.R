#' Computes predictions
#'
#' @param model A trained DeepTriangle model.
#' @param data A data frame, i.e. output of \code{dt_data_prep()}.
#' @export
dt_compute_predictions <- function(model, data) {

  predictions <- dt_predict(model, data)

  data %>%
    dplyr::left_join(
      dplyr::select(predictions, .data$group_code, .data$accident_year,
                    .data$development_lag, .data$predicted_loss, .data$predicted_os),
      by = c("group_code", "accident_year", "development_lag")
    ) %>%
    dplyr::group_by(.data$group_code, .data$accident_year) %>%
    dplyr::arrange(.data$group_code, .data$accident_year, .data$development_lag) %>%
    dplyr::mutate(
      predicted_loss = ifelse(is.na(.data$predicted_loss),
                              .data$incremental_paid, .data$predicted_loss),
      predicted_os = ifelse(is.na(.data$predicted_os),
                            .data$case_reserves, .data$predicted_os)
    ) %>%
    dplyr::mutate(
      predicted_cumulative_loss = cumsum(.data$predicted_loss) * .data$earned_premium_net,
      predicted_os = .data$predicted_os * .data$earned_premium_net,
      case_reserves_actual = .data$case_reserves_actual * .data$earned_premium_net
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(group_code = as.character(.data$group_code)) %>%
    tidyr::gather("type", "value", .data$predicted_cumulative_loss, .data$predicted_os,
                  .data$cumulative_paid_loss, .data$case_reserves_actual,
                  na.rm = TRUE) %>%
    dplyr::mutate(obs_type = dplyr::case_when(
      grepl("predicted", .data$type) ~ "prediction",
      .data$calendar_year <= 1997 ~ "observed",
      .data$calendar_year > 1997 ~ "holdout",
      TRUE ~ "observed"
    ))

}

dt_predict <- function(model, data) {
  results <- dplyr::tibble(
    lob = character(),
    group_code = factor(levels = levels(data$group_code)),
    accident_year = integer(), development_lag = integer(),
    predicted_loss = numeric(), predicted_os = numeric())

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
                                  .data$predicted_loss),
        case_reserves = ifelse(is.na(.data$predicted_os),
                               .data$case_reserves,
                               .data$predicted_os)
      ) %>%
      dplyr::select(-.data$predicted_loss, -.data$predicted_os) %>%
      dt_mutate_time_series() %>%
      dplyr::filter(.data$calendar_year == !!cy)
      # dplyr::mutate(group_code = as.character(.data$group_code))
    predictions <- model %>%
      stats::predict(convert_df_to_list(df)$x) %>%
      purrr::map(as.vector) %>%
      purrr::set_names(c("predicted_loss", "predicted_os")) %>%
      dplyr::as_tibble()
    results <<- dplyr::bind_rows(results, dplyr::bind_cols(df, predictions)) %>%
      dplyr::select(.data$lob, .data$group_code, .data$accident_year,
                    .data$development_lag, .data$predicted_loss, .data$predicted_os)
  })
  results
}
