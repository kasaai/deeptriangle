library(deeptriangle)
library(tidyverse)
library(h2o)
source("analysis/automl-utils.R")

automl_data <- automl_data_prep(insurance::schedule_p, dt_group_codes)

h2o.init()

automl_results <- map_df(automl_data, function(d) {
  df_with_lags <- d %>%
    group_by(group_code, accident_year) %>%
    arrange(group_code, accident_year, development_lag) %>%
    mutate(!!!lags(incremental_paid, 9))

  c(automl_training, automl_validation, automl_full_training) %<-%
    automl_train_validation_split(df_with_lags)

  automl_full_training_h2o <- as.h2o(automl_full_training)
  response <- "incremental_paid"
  predictors <- c("group_code", paste0("lag_incremental_paid_0", 1:9))
  automl_model <- h2o.automl(x = predictors, y = response,
                        training_frame = automl_full_training_h2o,
                        max_runtime_secs = 5*60, seed = 2018)

  predictions_df <- automl_compute_predictions(automl_model, d)
  automl_compute_metrics(predictions_df)
}) %>%
  bind_rows()
write_csv(automl_results, "analysis/automl_results.csv")
h2o.shutdown(FALSE)
