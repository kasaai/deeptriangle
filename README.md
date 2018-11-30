
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.org/kevinykuo/deeptriangle.svg?branch=master)](https://travis-ci.org/kevinykuo/deeptriangle)

# DeepTriangle: A Deep Learning Approach to Loss Reserving

This is the companion repository to the DeepTriangle paper. A preprint
can be found at <https://arxiv.org/abs/1804.09253>.

## Reproducing experiments

**Note: Due to changes in some library dependencies over time, the
results may not reproduce exactly depending on when you installed
TensorFlow, but the conclusions of the paper remain unchanged. The
results below are generated using TensorFlow 1.10 and Keras 2.2.**

To get started, either clone the repo and build the R package, or
install with

``` r
devtools::install_github("kevinykuo/deeptriangle")
```

You will also need the
[insurance](https://github.com/kevinykuo/insurance) package, which can
be installed with

``` r
devtools::install_github("kevinykuo/insurance")
```

The following lines of code will instantiate and fit the model for each
line of business then combine the forecasts in a data frame:

``` r
library(deeptriangle)
library(tidyverse)
library(keras)

use_implementation("tensorflow")

# set seed for reproducibility
use_session_with_seed(2018)

data <- dt_data_prep(insurance::schedule_p, dt_group_codes)

lobs <- c("workers_compensation", "commercial_auto",
          "private_passenger_auto", "other_liability")

predictions <- lobs %>%
  map(
    function(x) {
      # reinstantiate model
      model <- dt_model()
      
      model %>%
        keras::compile(
          optimizer = keras::optimizer_adam(amsgrad = TRUE),
          loss = "mae",
          loss_weights = c(0.8, 0.2)
      )
      
      c(training_data, validation_data, full_training_data) %<-%
        dt_train_validation_split(data[[x]])
      
      message("Training - ", x)
      
      # determine number of epochs
      epochs_to_train <- dt_optimize_epochs(
        model, training_data, validation_data
      )
      
      model <- dt_model()
      
      model %>% keras::compile(
        optimizer = keras::optimizer_adam(amsgrad = TRUE),
        loss = "mae",
        loss_weights = c(0.8, 0.2)
      )
      
      # fit model to all training data
      history <- model %>%
        fit(x = full_training_data$x,
            y = full_training_data$y,
            batch_size = 128,
            epochs = epochs_to_train,
            verbose = 0)
      dt_compute_predictions(model, data[[x]])
    }) %>%
  bind_rows()
```

We can then compute performance metrics…

``` r
model_results <- dt_compute_metrics(predictions) %>%
  bind_rows(stochastic_model_results) %>%
  bind_rows(read_csv("analysis/automl_results.csv")) %>%
  gather(metric, value, mape, rmspe)
```

and tabulate the results:

``` r
dt_tabulate_metrics(model_results, metric = "mape") %>%
  knitr::kable(booktabs = "T", digits = 3)
```

| lob                      |  Mack |   ODP |   CIT |   LIT | AutoML | DeepTriangle |
| :----------------------- | ----: | ----: | ----: | ----: | -----: | -----------: |
| commercial\_auto         | 0.060 | 0.217 | 0.052 | 0.052 |  0.068 |        0.050 |
| other\_liability         | 0.134 | 0.223 | 0.165 | 0.152 |  0.142 |        0.120 |
| private\_passenger\_auto | 0.038 | 0.039 | 0.038 | 0.040 |  0.036 |        0.023 |
| workers\_compensation    | 0.053 | 0.105 | 0.054 | 0.054 |  0.067 |        0.046 |

``` r
dt_tabulate_metrics(model_results, metric = "rmspe") %>%
  knitr::kable(booktabs = "T", digits = 3)
```

| lob                      |  Mack |   ODP |   CIT |   LIT | AutoML | DeepTriangle |
| :----------------------- | ----: | ----: | ----: | ----: | -----: | -----------: |
| commercial\_auto         | 0.080 | 0.822 | 0.076 | 0.074 |  0.096 |        0.080 |
| other\_liability         | 0.202 | 0.477 | 0.220 | 0.209 |  0.181 |        0.167 |
| private\_passenger\_auto | 0.061 | 0.063 | 0.057 | 0.060 |  0.059 |        0.035 |
| workers\_compensation    | 0.079 | 0.368 | 0.080 | 0.080 |  0.099 |        0.078 |

To create actual vs. predicted plots, use the `dt_plot_predictions()`
function.

``` r
# devtools::install_github("thomasp85/patchwork")
library(patchwork)
paid_plot <- dt_plot_predictions(predictions, "337", "workers_compensation",
                                 "paid_loss")
case_plot <- dt_plot_predictions(predictions, "337", "workers_compensation",
                                 "claims_outstanding")
paid_plot + case_plot + plot_layout(ncol = 1)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="60%" />

## Testing different architectures

If you would like to try out different architectures or hyperparameters,
you can do so by providing a function that returns a compiled keras
model. See the source code of `dt_model()` for a template. In order to
utilize other functions in this package, the inputs and outputs of your
custom function have to match those of `dt_model()`. You can also
implement different early stopping criteria by providing a function
similar to `dt_optimize_epochs()`.

For more details on the **keras** R package, visit
<https://keras.rstudio.com/>.
