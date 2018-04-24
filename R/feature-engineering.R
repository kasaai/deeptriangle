compute_history_time_series <- function(column, impute_values = column) {

  # prepend 0s to vector until desired length
  pad_zero <- function(v, l) {
    v[is.na(v)] <- 0
    length_diff <- l - length(v)
    if (length_diff > 0)
      c(rep(0, length_diff), v)
    else
      v
  }

  purrr::map(seq_along(column),
      ~ ifelse(is.na(column), impute_values, column)[0:(.x-1)] %>%
        pad_zero(9))
}

#' Add time series feature to data frame
#'
#' @param x A data frame.
#' @export
dt_mutate_time_series <- function(x) {
  x %>%
    dplyr::group_by(.data$group_code, .data$accident_year) %>%
    dplyr::arrange(.data$group_code, .data$accident_year, .data$development_lag) %>%
    dplyr::mutate(
      prior_paid_along_ay = compute_history_time_series(.data$incremental_paid),
      prior_case_reserves_along_ay = compute_history_time_series(.data$case_reserves)
    ) %>%
    dplyr::ungroup()
}
