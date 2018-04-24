#' Instantiate and compile DeepTriangle model
#'
#' @export
dt_model <- function() {
  ay_seq_input <- keras::layer_input(shape = list(9, 2), name = "ay_seq_input")
  ay_seq_output <- ay_seq_input %>%
    keras::layer_masking() %>%
    keras::layer_gru(units = 64)

  embedding_input <-  keras::layer_input(shape = 1, name = "embedding_input")
  embedding_output <- embedding_input %>%
    keras::layer_embedding(50, 49) %>%
    keras::layer_flatten()

  concat <-  keras::layer_concatenate(list(ay_seq_output, embedding_output))

  case_reserves_output <- concat %>%
    keras::layer_dense(units = 64, activation = "relu") %>%
    keras::layer_dense(units = 1) %>%
    dt_activation_scaled_sigmoid(name = "case_reserves_output")

  paid_output <-  keras::layer_concatenate(list(concat, case_reserves_output))  %>%
    keras::layer_dense(units = 64, activation = "relu") %>%
    keras::layer_dense(units = 1) %>%
    dt_activation_scaled_sigmoid(name = "paid_output")

  model <-  keras::keras_model(
    inputs = c(ay_seq_input, embedding_input
    ),
    outputs = c(paid_output, case_reserves_output)
  )

  model %>%
    keras::compile(
      optimizer = keras::optimizer_adam(amsgrad = TRUE),
      loss = "mae",
      loss_weights = c(0.8, 0.2)
    )
  model
}

