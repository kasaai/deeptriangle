library(tidyverse)
library(readxl)

temp_file <- tempfile(fileext = ".xlsx")
download.file("http://www.casact.org/pubs/monographs/meyers/Monograph_Tables_and_Scripts.xlsx",
              temp_file)

compute_metrics <- function(x) {
  estimate_column <- colnames(x) %>%
    grep("Estimate$", ., value = TRUE)
  pred <- estimate_column %>%
    rlang::sym()
  m <- estimate_column %>%
    gsub(" Estimate", "", .)
  actual <- "Outcome" %>%
    rlang::sym()
  x %>%
    mutate(pct_error = (!!pred - !!actual) / !!actual) %>%
    group_by(Line) %>%
    summarize(mape = mean(abs(pct_error)),
              rmspe = sqrt(mean(pct_error ^ 2))
    ) %>%
    mutate(model = m)
}

stochastic_models <- c("Multi ODP Paid", "Multi Mack Paid", "Multi CCL Paid",
                   "Multi CIT Paid", "Multi LIT Paid", "Multi CSR Paid")
stochastic_model_results <- stochastic_models %>%
  map(~ read_xlsx(temp_file, sheet = .x, range = "A5:F205") %>%
        compute_metrics) %>%
  bind_rows() %>%
  mutate(lob = case_when(
    Line == "CA" ~ "commercial_auto",
    Line == "PA" ~ "private_passenger_auto",
    Line == "WC" ~ "workers_compensation",
    Line == "OL" ~ "other_liability")) %>%
  select(-Line)

usethis::use_data(stochastic_model_results)
