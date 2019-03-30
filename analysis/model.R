dt_model <- function() {
  ay_seq_input <- layer_input(shape = list(9, 2), name = "ay_seq_input")
  company_code_input <- layer_input(shape = 1, name = "company_input")
  company_code_embedding <- company_code_input %>%
    layer_embedding(200, 49, name = "company_code_embedding") %>%
    layer_flatten()%>%
    layer_repeat_vector(9)

  encoded <- ay_seq_input %>%
    layer_masking(-99) %>%
    layer_gru(units = 128, dropout = 0.2, recurrent_dropout = 0.2)

  decoded <- encoded %>%
    layer_repeat_vector(9) %>%
    layer_gru(128, return_sequences = TRUE, dropout = 0.2, recurrent_dropout = 0.2) %>%
    layer_lambda(f = function(x) layer_concatenate(list(x, company_code_embedding)))  
    #layer_lambda(f = function(x) layer_concatenate(list(x, k_repeat(company_code_embedding, 9))))

  case_reserves_output <- decoded %>%
    time_distributed(layer_dense(units = 64, activation = "relu")) %>%
    time_distributed(layer_dropout(rate = 0.2)) %>%
    time_distributed(layer_dense(units = 1, activation = "relu"), name = "case_reserves_output")

  paid_output <- decoded %>%
    time_distributed(layer_dense(units = 64, activation = "relu")) %>%
    time_distributed(layer_dropout(rate = 0.2)) %>%
    time_distributed(layer_dense(units = 1, activation = "relu"), name = "paid_output")

  model <- keras_model(
    inputs = c(ay_seq_input, company_code_input),
    outputs = c(paid_output, case_reserves_output)
  )

  model
}
