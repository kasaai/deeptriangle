#' Compute performance metrics
#'
#' @param predictions Predictions data frame.
#' @export
dt_compute_metrics <- function(predictions) {
  predictions %>%
    dplyr::filter(
      .data$development_lag == 10,
      .data$type %in% c("cumulative_paid_loss", "predicted_cumulative_loss")
    ) %>%
    dplyr::group_by(.data$lob, .data$group_code, .data$type, .data$run_id) %>%
    dplyr::summarize(ultimate = sum(.data$value)) %>%
    dplyr::group_by(.data$lob, .data$group_code, .data$type) %>%
    dplyr::summarize(ultimate = mean(.data$ultimate)) %>%
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
    dplyr::mutate(model = "DeepTriangle")
}

#' Tabulate performance metrics for all models
#'
#' @param data Model results in tidy format.
#' @param metric Performance metric.
#' @export
dt_tabulate_metrics <- function(data, metric = c("mape", "rmspe")) {
  metric <- rlang::arg_match(metric)
  data %>%
    dplyr::filter(metric == !!metric) %>%
    dplyr::select(-.data$metric) %>%
    tidyr::spread(.data$model,.data$ value) %>%
    dplyr::select(
      .data$lob, .data$Mack, .data$ODP,
      .data$CIT, .data$LIT, .data$AutoML, .data$DeepTriangle
    )
}
