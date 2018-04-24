#' Optimize number of epochs to train
#'
#' @param model Keras model object, i.e. output of \code{dt_model()}.
#' @param training_data Training dataset.
#' @param validation_data Validation dataset.
#' @param batch_size Batch size.
#' @export
dt_optimize_epochs <- function(model, training_data, validation_data, batch_size = 128) {
  custom_stopping_callback <- dt_early_stopping()
  history <- model %>%
    keras::fit(x = training_data$x,
        y = training_data$y,
        batch_size = batch_size,
        epochs = 1000,
        validation_data = unname(validation_data),
        shuffle = TRUE,
        callbacks = c(custom_stopping_callback),
        verbose = 0
    )

  which.min(custom_stopping_callback$ma_val_loss)
}
