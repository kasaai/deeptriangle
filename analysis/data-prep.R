library(deeptriangle)
library(tidyverse)
library(recipes)
library(insurance)

data(schedule_p)

data_with_features <- schedule_p %>%
  right_join(dt_group_codes, by = c("lob", "group_code")) %>%
  mutate(case_reserves = incurred_loss - cumulative_paid_loss) %>%
  group_by(lob, group_code, accident_year) %>%
  arrange(lob, group_code, accident_year, development_lag) %>%
  mutate(
    incremental_paid_actual = incremental_paid_loss,
    incremental_paid = ifelse(
      calendar_year <= 1997,
      incremental_paid_actual, NA_real_
    ),
    cumulative_paid_actual = cumulative_paid_loss,
    cumulative_paid = ifelse(
      calendar_year <= 1997,
      cumulative_paid_actual, NA_real_
    ),
    case_reserves_actual = case_reserves,
    case_reserves = ifelse(
      calendar_year <= 1997,
      case_reserves_actual,
      NA_real_
    )
  ) %>%
  ungroup() %>%
  mutate(
    bucket = case_when(
      calendar_year <= 1995 & development_lag > 1 ~ "train",
      calendar_year > 1995 & calendar_year <= 1997 &
        development_lag > 1 ~ "validation",
      calendar_year > 1997 ~ "test"
    )
  ) %>%
  mutate(
    incremental_paid = incremental_paid / earned_premium_net,
    incremental_paid_actual = incremental_paid_actual / earned_premium_net,
    cumulative_paid = cumulative_paid / earned_premium_net,
    cumulative_paid_actual = cumulative_paid_actual / earned_premium_net,
    case_reserves = case_reserves / earned_premium_net,
    case_reserves_actual = case_reserves_actual / earned_premium_net
  )

# Recipe for indexing company code
company_index_recipe <- recipe(~ group_code, data = data_with_features) %>%
  step_integer(group_code, zero_based = TRUE) %>%
  prep()

data_keras <- bind_rows(
  validation_data = data_with_features %>%
    filter(bucket %in% c("train", "validation") | development_lag == 1) %>%
    mutate_series() %>%
    filter(bucket == "validation") %>%
    group_by(lob) %>%
    nest() %>%
    mutate(keras_data = map(data, ~ prep_keras_data(.x, company_index_recipe))),
  full_training_data = data_with_features %>%
    filter(bucket %in% c("train", "validation") | development_lag == 1) %>%
    mutate_series() %>%
    filter(bucket %in% c("train", "validation")) %>%
    group_by(lob) %>%
    nest() %>%
    mutate(keras_data = map(data, ~ prep_keras_data(.x, company_index_recipe))),
  test_data = data_with_features %>%
    filter(calendar_year <= 1998) %>%
    mutate_series() %>%
    filter(bucket == "test", calendar_year == 1998) %>%
    group_by(lob) %>%
    nest() %>%
    mutate(keras_data = map(data, ~ prep_keras_data(.x, company_index_recipe))),
  .id = "data_type"
)

