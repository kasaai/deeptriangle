utils::globalVariables(c("type_actual", "type_prediction"))

#' Plot predictions
#'
#' @param predictions Predictions data frame. Output of \code{dt_compute_predictions()}.
#' @param group_code Company code to plot.
#' @param lob LOB to plot
#' @param type One of \code{"paid_loss"} and \code{"claims_outstanding"}.
#' @export
dt_plot_predictions <- function(
  predictions, group_code, lob, type = c("paid_loss", "claims_outstanding")) {
  type <- rlang::arg_match(type)
  y_lab <- if (identical(type, "paid_loss")) "Loss Ratio" else "Claims Outstanding"

  c(type_actual, type_prediction) %<-% (
    switch(type,
           paid_loss = c("cumulative_paid_loss", "predicted_cumulative_loss"),
           claims_outstanding = c("case_reserves_actual", "predicted_os")
    ))
  predictions <- predictions %>%
    dplyr::mutate(value = .data$value / .data$earned_premium_net)

  predictions %>%
    dplyr::filter(.data$group_code == !!group_code,
                  .data$type == type_actual,
                  .data$lob == !!lob) %>%
    ggplot2::ggplot(ggplot2::aes_(x = ~development_lag, y = ~value, shape = ~obs_type,
                                  color = ~obs_type)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ accident_year, nrow = 2) +
    ggplot2::theme_light() +
    ggplot2::geom_point(data = dplyr::filter(predictions,
                                             .data$group_code == !!group_code,
                                             .data$type == type_prediction,
                                             .data$lob == !!lob,
                                             .data$calendar_year > 1997)) +
    ggplot2::scale_shape_manual(values = c(1, 19, 3)) +
    ggplot2::scale_color_manual(values = c("black", "black", "red")) +
    ggplot2::scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 2, by = 0.2)) +
    # ggplot2::coord_fixed(ratio = 10, xlim = c(0, 10), ylim = c(0, 1.5)) +
    ggplot2::theme(legend.title=ggplot2::element_blank()) +
    ggplot2::ylab(y_lab) +
    ggplot2::labs(x = "Development Lag")

}
