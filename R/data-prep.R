#' Data preparation for DeepTriangle
#'
#' Performs basic feature engineering on raw data and labels each record "train", "valid" or "test".
#'
#' @param schedule_p_data Schedule P data, i.e. \code{insurance::schedule_p}.
#' @param group_codes Data frame of LOB and group codes, i.e. \code{dt_group_codes}.
#' @param train_validation_cutoff Calendar year end of the temporal split of training
#'   and validation data. The paper uses \code{1995}.
#'
#' @return A list of data frames, one for each line of business.
#'
#' @export
dt_data_prep <- function(schedule_p_data, group_codes, train_validation_cutoff = 1995) {
  schedule_p_data %>%
    dplyr::right_join(group_codes, by = c("lob", "group_code")) %>%
    dplyr::mutate(case_reserves = .data$incurred_loss - .data$cumulative_paid_loss) %>%
    dplyr::group_by(.data$lob, .data$group_code, .data$accident_year) %>%
    dplyr::arrange(.data$lob, .data$group_code, .data$accident_year, .data$development_lag) %>%
    dplyr::mutate(
      incremental_paid_actual = .data$incremental_paid_loss,
      incremental_paid = ifelse(.data$calendar_year <= 1997,
                                      .data$incremental_paid_actual, NA_real_),
      case_reserves_actual = .data$case_reserves,
      case_reserves = ifelse(.data$calendar_year <= 1997,
                                   .data$case_reserves_actual, NA_real_)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      bucket = dplyr::case_when(
      .data$calendar_year <= !!train_validation_cutoff & .data$development_lag > 1 ~ "train",
      .data$calendar_year > !!train_validation_cutoff & .data$calendar_year <= 1997 &
        .data$development_lag > 1 ~ "validation",
      .data$calendar_year > 1997 ~ "test"
    )) %>%
    dplyr::mutate(
      incremental_paid = .data$incremental_paid / .data$earned_premium_net,
      incremental_paid_actual = .data$incremental_paid_actual / .data$earned_premium_net,
      case_reserves = .data$case_reserves / .data$earned_premium_net,
      case_reserves_actual = .data$case_reserves_actual / .data$earned_premium_net
    ) %>%
    dplyr::ungroup() %>%
    (function(x) split(x, x$lob)) %>%
    purrr::map(~ .x %>%
                 dplyr::mutate(group_code = factor(.data$group_code))
    ) %>%
    purrr::map(dt_mutate_time_series)
}

#' @importFrom keras array_reshape to_categorical
convert_df_to_list <- function(data) {

  num_obs <- nrow(data)
  x <- list(
    ay_seq_input =  mapply(
      cbind,
      matrix(unlist(data$prior_paid_along_ay), 9),
      matrix(unlist(data$prior_case_reserves_along_ay), 9)
    ) %>%
      t() %>%
      array_reshape(c(nrow(data), 9, 2)),
    embedding_input = as.integer(data$group_code) - 1
  )

  y <- list(paid_output = data$incremental_paid_actual,
            case_reserves_output = data$case_reserves_actual)

  list(x = x, y = y)
}

#' Convert data frame into train-validation lists for training
#'
#' @param data Data frame corresponding to a single LOB, typically the output of \code{dt_data_prep()}.
#' @export
dt_train_validation_split <- function(data) {
  # data_with_features <- purrr::map(data, dt_mutate_time_series)

  training_data <- convert_df_to_list(
    dplyr::filter(data,
                  .data$bucket == "train",
                  .data$development_lag > 1)
  )

  validation_data <- convert_df_to_list(
    dplyr::filter(data,
                  .data$bucket == "validation",
                  .data$development_lag > 1)
  )

  full_training_data <- convert_df_to_list(
    dplyr::filter(data,
                  .data$bucket %in% c("train", "validation"))
  )

  list(training_data, validation_data, full_training_data)
}
