library(recipes)
library(insurance)
library(tidyverse)
library(deeptriangle)
library(keras)
library(tensorflow)

source("analysis/analysis-utils.R")
source("analysis/model.R")
source("analysis/data-prep.R")

lobs <- c("commercial_auto","other_liability", "private_passenger_auto", "workers_compensation")

results <- map_df(lobs, function(lob) {
  data_lob <- data_keras %>%
    filter(lob == !!lob)
  full_training_data_keras <- data_lob %>%
    filter(data_type == "full_training_data") %>%
    pull(keras_data) %>%
    flatten()
  validation_data_keras <- data_lob %>%
    filter(data_type == "validation_data") %>%
    pull(keras_data) %>%
    flatten()
  test_data <- data_lob %>%
    filter(data_type == "test_data")

  map_df(1:100, function(run_id) {
    if (run_id %% 10 == 1) k_clear_session()
    cat(sprintf("Training LOB %s run %s: ", lob, run_id))
    start_time <- Sys.time()
    model <- dt_model()
    model %>%
      compile(
        optimizer = optimizer_adam(lr = 0.0005, amsgrad = TRUE),
        loss = list(masked_mse(-99), masked_mse(-99)),
        loss_weights = c(0.5, 0.5)
      )

    cb <- callback_early_stopping(min_delta = 0.001, patience = 200, mode = "min", restore_best_weights = TRUE)

    history <- model %>%
      fit(
        x = full_training_data_keras$x,
        y = full_training_data_keras$y,
        validation_data = unname(validation_data_keras),
        batch_size = 2250,
        epochs = 1000,
        callbacks = list(cb),
        verbose = 0
      )

    training_time <- as.integer(Sys.time() - start_time)

    cat(sprintf("training for %d epochs took %d seconds at %s", cb$stopped_epoch, training_time, Sys.time()), "\n")

    predictions_table <- compute_predictions(model, test_data, lob) %>%
      mutate(lob = !!lob, run_id = !!run_id)

    tibble(
      lob = lob,
      run_id = run_id,
      training_time = training_time,
      predictions_table = list(predictions_table),
      trained_epochs = cb$stopped_epoch
    )
  })
})

predictions_table <- results  %>%
  pull(predictions_table) %>%
  bind_rows()
