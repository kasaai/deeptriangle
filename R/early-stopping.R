CustomEarlyStopping <- R6::R6Class(
  "CustomEarlyStopping",
  inherit = keras::KerasCallback,
  public = list(
    val_loss = NULL,
    train_loss = NULL,
    ma_val_loss = NULL,
    window_width = NULL,
    patience = NULL,
    stopped_epoch = NULL,
    initialize = function(window_width, patience) {
      self$window_width <- window_width
      self$patience <- patience
    },
    on_epoch_end = function(epoch, logs = list()) {
      self$val_loss <- c(self$val_loss, logs[["val_loss"]])
      self$train_loss <- c(self$train_loss, logs[["loss"]])

      if (epoch < self$window_width) {
        self$ma_val_loss <- c(self$ma_val_loss, mean(self$val_loss))
      } else {
        ma <- mean(tail(self$val_loss, self$window_width))
        self$ma_val_loss <- c(self$ma_val_loss, ma)

        if (epoch > (self$patience + self$window_width) &&
            (self$train_loss[[epoch]] < self$val_loss[[epoch]]) &&
            # self$ma_val_loss[[epoch - self$patience]] ==
            # min(self$ma_val_loss[(epoch - self$patience) : epoch])
            self$val_loss[[epoch - self$patience]] <=
            min(self$val_loss[(epoch - self$patience) : epoch])
        ) {
          self$stopped_epoch <- epoch
          self$model$stop_training <- TRUE
        }
      }
    }
  )
)

#' Custom early stopping callback
#'
#' Implements the early stopping callback detailed in the paper.
#'
#' @param window_width Window width for moving average calcuation, defaults to \code{10}.
#' @param patience Patience, defaults to \code{100}.
#' @export
dt_early_stopping <- function(window_width = 10, patience = 100) {
  CustomEarlyStopping$new(window_width, patience)
}
