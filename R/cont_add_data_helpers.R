# Helpers----

#' Get column names
#'
#' `get_col_names()` combines the information provided with `var_names` and
#' `elic_types` to construct the column names.
#'
#' @inheritParams cont_start
#'
#' @return Character vector with the column names.
#' @noRd
#'
#' @author Sergio Vignali
get_col_names <- function(var_names,
                          elic_types) {
  n <- length(var_names)
  r <- gsub("p", "", elic_types, fixed = TRUE) |>
    as.integer()
  labels <- get_labels(n = n,
                       elic_types = elic_types)

  var_columns <- rep(var_names, r) |>
    paste0("_", labels)

  c("id", var_columns)
}

#' Get labels
#'
#' Get label to build column names.
#'
#' @param n integer, number of variables.
#' @inheritParams cont_start
#'
#' @return Character vector with the labels
#' @noRd
#'
#' @author Sergio Vignali
get_labels <- function(n,
                       elic_types) {

  if (n > 1L && length(elic_types) == 1L) {
    # Recycle variable type
    elic_types <- rep(elic_types, n)
  }

  # Labels are stored on the `var_labels` data object
  raw_labels <- unlist(var_labels[elic_types],
                       use.names = FALSE)

  raw_labels
}

#' Add NAs to data
#'
#' Adds rows with NAs, used when datasets have less data than experts.
#'
#' @param data tibble with data belonging to the first round.
#' @param experts numeric providing the number of experts.
#'
#' @return Data to add to the Round with added NAs.
#' @noRd
#'
#' @author Sergio Vignali
add_nas_rows <- function(data, experts) {

  n <- experts - nrow(data)
  nas <- matrix(nrow = n, ncol = ncol(data)) |>
    as.data.frame() |>
    stats::setNames(colnames(data))

  rbind(data, nas)
}

#' Omogenise datasets
#'
#' `omogenise_datasets()` is used to omogenise the data in Round 1 and Round 2.
#'
#' @param x [elic_cont][elicitr::elic_cont] object containing data from Round 1.
#' @param data tibble containing data from Round 2.
#'
#' @return A list with two elemnts, one containing the omogenised data for Round
#' 1, and the other containing the omogenised data for Round 2.
#' @noRd
#'
#' @author Sergio Vignali
omogenise_datasets <- function(x, data) {

  experts <- x[["experts"]]
  nrow_round_1 <- nrow(x[["data"]][["round_1"]])
  nrow_round_2 <- nrow(data)
  n_diff <- nrow_round_1 - nrow_round_2
  all_ids <- union(x[["data"]][["round_1"]][["id"]], data[["id"]])

  round_1_has_nas <- anyNA(x[["data"]][["round_1"]][["id"]])


  if (round_1_has_nas) {
    round_1_ids <- x[["data"]][["round_1"]][["id"]]
    round_2_ids <- data[["id"]]
    r1_diff_ids <- setdiff(round_2_ids, round_1_ids)
    r2_diff_ids <- setdiff(round_1_ids, round_2_ids) |>
      stats::na.omit()
    round_2_has_nas <- length(data[["id"]]) < x[["experts"]]

    # Check if all non NA id in round 1 are also in round 2
    if (all(stats::na.omit(round_1_ids) %in% round_2_ids)) {

      data_all <- dplyr::rows_upsert(x[["data"]][["round_1"]], data,
                                     by = "id")
      data <- data_all[rowSums(is.na(data_all)) != ncol(data_all), ]

      r1_na_idx <- which(is.na(round_1_ids))
      selection <- r1_na_idx[seq_along(r1_diff_ids)]
      x[["data"]][["round_1"]][["id"]][selection] <- r1_diff_ids

      if (round_2_has_nas) {
        data <- add_nas_rows(data,
                             x[["experts"]])
      }

      n <- length(r1_diff_ids)
      cli::cli_alert_info("The dataset in {.val Round 2} has {.val {n}} \\
                           {.cls id} not present in {.val Round 1}. \\
                           Th{?is/ese} {.cls id} ha{?s/ve} been added to \\
                           {.val Round 1} with {.val {NA}} values.")

      list(round_1 = x[["data"]][["round_1"]],
           round_2 = data)
    } else {
      r1_na_idx <- which(is.na(round_1_ids))
      n <- length(r1_diff_ids)
      r1_nas <- sum(is.na(round_1_ids))

      if (r1_nas >= length(r1_diff_ids)) {
        data_all <- dplyr::rows_upsert(x[["data"]][["round_1"]], data,
                                       by = "id")
        data <- data_all[rowSums(is.na(data_all)) != ncol(data_all), ]

        selection <- r1_na_idx[seq_along(r1_diff_ids)]
        x[["data"]][["round_1"]][["id"]][selection] <- r1_diff_ids
        r2_na_idx <- which(is.na(round_2_ids))
        data[data[["id"]] %in% r2_diff_ids, -1] <- NA
        data <- add_nas_rows(data,
                             x[["experts"]])

        warn <- "The dataset in {.val Round 2} has {.val {n}} {.cls id} not \\
                 present in {.val Round 1}. Th{?is/ese} {.cls id} ha{?s/ve} \\
                 been added to {.val Round 1} with {.val NA} values but \\
                 could be typo{?s} in the raw data."
        info <- "Check raw data and if you want to update the dataset in \\
                 {.val Round 2} use {.fn elicitr::cont_add_data} with \\
                 {.code overwrite = TRUE}."
        cli::cli_warn(c(warn, "i" = info))

        list(round_1 = x[["data"]][["round_1"]],
             round_2 = data)
      } else {
        total_experts <- length(round_1_ids) - r1_nas + n
        text <- "Impossible to combine {.val Round 1} and {.val Round 2} \\
                 datasets:"
        error <- "{.val Round 2} has {.val {n}} {.cls id} not present in \\
                  {.val Round 1}."
        error2 <- "This brings the total number of expert to \\
                   {.val {total_experts}} while {.arg experts} was set to \\
                   {.val {experts}} in {.fn elicitr::cont_start}."
        info <- "Check raw data. Either modify the metadata using \\
                 {.fn elicitr::cont_start} with {.arg experts} = \\
                 {.val {total_experts}} and reupload the data for both rounds,"
        info2 <- "or modify the dataset manually and upload it again with \\
                  {.fn elicitr::cont_add_data}."
        cli::cli_abort(c(text,
                         "x" = error,
                         " " = error2,
                         "i" = info,
                         "y" = info2),
                       call = rlang::caller_env())
      }
    }
  } else {
    # No NAs in Round 1 and Round 2 has one row for each expert
    n <- length(all_ids) - nrow_round_1

    # All id of Round 2 are also in Round 1 ==> reorder data in Round 2
    if (n == 0) {

      mis_in_round_2 <- setdiff(x[["data"]][["round_1"]][["id"]], data[["id"]])

      data <- dplyr::rows_upsert(x[["data"]][["round_1"]], data,
                                 by = "id")

      # Round 2 could have less entries than experts ==> Fill with NAs
      if (length(mis_in_round_2) > 0) {
        data[data[["id"]] %in% mis_in_round_2, -1] <- NA
      }

      list(round_1 = x[["data"]][["round_1"]],
           round_2 = data)

    } else if (n == 1) {
      # Same number of rows in Round 1 and Round 2 but one id is different ==>
      # Consider it as a typo and replace it with the one from Round 1. Also
      # raise a warning.
      mis_in_round_1 <- setdiff(data[["id"]], x[["data"]][["round_1"]][["id"]])
      mis_in_round_2 <- setdiff(x[["data"]][["round_1"]][["id"]], data[["id"]])

      # Round 2 could have less entries than experts ==> Impossible match, raise
      # an error
      if (length(mis_in_round_2) > 0 && n_diff > 0) {

        fn <- as.list(sys.call(-1))[[1]]

        missing <- setdiff(data[["id"]], x[["data"]][["round_1"]][["id"]])
        text <- "Automatic match between the dataset in {.val Round 1} and \\
                 {.val Round 2} is not possible:"
        error <- "{.val Round 2} has {.val {1}} {.cls id} not present in \\
                  {.val Round 1} and {.val {n_diff}} entries with NAs."
        error2 <- "The {.cls id} not present in {.val Round 1} {?is/are} \\
                  {.val {missing}}."
        cli::cli_abort(c(text,
                         "x" = error,
                         " " = error2,
                         "i" = "Check raw data. See {.fn elicitr::{fn}} for \\
                                more information."),
                       call = rlang::caller_env())
      }

      data[data[["id"]] == mis_in_round_1, "id"] <- mis_in_round_2
      data <- dplyr::rows_upsert(x[["data"]][["round_1"]], data,
                                 by = "id")

      warn <- "Dataset for {.val Round 2} has {.val {1}} {.cls id} not \\
               present in {.val Round 1}. This is considered a typo by \\
               expert {.val {mis_in_round_2}} in {.val Round 2} and the \\
               {.cls id} has been replaced."

      info <- "Check raw data and if you want to update the dataset in \\
               {.val Round 2} use {.fn elicitr::cont_add_data} with \\
               {.code overwrite = TRUE}."
      cli::cli_warn(c("!" = warn,
                      "i" = info))

      list(round_1 = x[["data"]][["round_1"]],
           round_2 = data)
    } else {
      fn <- as.list(sys.call(-1))[[1]]
      # More than 1 id present in Round 2 is not in Round 1 ==> Raise an error
      missing <- setdiff(data[["id"]], x[["data"]][["round_1"]][["id"]])
      text <- "Automatic match between the dataset in {.val Round 1} and \\
                 {.val Round 2} is not possible:"
      error <- "{.val Round 2} has {.val {n}} {.cls id} not present in \\
                  {.val Round 1}."
      error2 <- "The {.cls id} not present in {.val Round 1} are \\
                {.val {missing}}."
      cli::cli_abort(c(text,
                       "x" = error,
                       " " = error2,
                       "i" = "Check raw data. See {.fn elicitr::{fn}} for \\
                                more information."),
                     call = rlang::caller_env())
    }
  }
}
