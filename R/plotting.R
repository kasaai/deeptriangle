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
    filter(
      .data$group_code == !!group_code,
      .data$type == type_prediction,
      .data$lob == !!lob
    ) %>%
    mutate(run_id = as.character(.data$run_id)) %>%
    ggplot(aes_(x = ~development_lag, y = ~value)) +
    ggfan::geom_fan(intervals = c(0.05, 0.25, 0.5, 0.75, 0.95)) + scale_fill_distiller(name = "Pred. qtiles") +
    stat_summary(fun.y = "mean", geom="line", linetype = "dashed", alpha = 0.5, aes(color = "mean")) +
    scale_color_manual("Mean pred.", values=c("mean" = "blue"), label = NULL) +
    facet_wrap(~accident_year, nrow = 2) +
    guides(
      shape = guide_legend(title = "Actual")
    ) +
    geom_point(
      mapping = ggplot2::aes_(x = ~development_lag, y = ~value, shape = ~obs_type),
      data = predictions %>%
        filter(
          .data$group_code == !!group_code,
          .data$type == type_actual,
          .data$lob == !!lob
        ) %>%
        filter(.data$run_id == 1),
      inherit.aes = FALSE
    ) +
    scale_shape_manual(values = c(1, 19)) +
    scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
    scale_y_continuous(breaks = seq(0, 2, by = 0.2)) +
    ylab(y_lab) +
    labs(x = "Development Lag") +
    theme_light()

}
