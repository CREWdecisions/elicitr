# cat_add_data_checkers----

#' Check names and categories
#'
#' Checks if names and categories are as expected, i.e. if the number of unique
#' names is less than or equal to the expected number of experts, and if the
#' categories are those recorded in the object.
#'
#' @param x [elic_cat] object.
#' @param data [tibble][tibble::tibble] with the data to be checked.
#' @param type character string with the type of check to be performed.
#'
#' @returns An error if the number of unique names is greater than the number
#' of experts or if the categories are not those recorded in the object.
#' @noRd
#'
#' @author Sergio Vignali
check_names_categories_options <- function(x, data, type) {

  error <- ""

  if (type == "name") {
    names <- unique(data[["id"]])
    n <- x[["experts"]]

    if (length(names) > n) {

      text <- "The number of unique names is greater than the expected number \\
               of experts:"
      error <- "There are {.val {length(names)}} unique names but they should \\
                be no more than {.val {n}}."
    }

  } else if (type == "categories") {

    categories <- unique(data[["category"]])
    diff <- setdiff(categories, x[["categories"]])

    if (length(diff) > 0) {

      text <- "The column with the name of the categories contains unexpected \\
               values:"
      error <- "The value{?s} {.val {diff}} {?is/are} not valid."
    }

  } else if (type == "options") {

    options <- unique(data[["option"]])
    diff <- setdiff(options, x[["options"]])

    if (length(diff) > 0) {

      text <- "The column with the name of the options contains unexpected \\
               values:"
      error <- "The value{?s} {.val {diff}} {?is/are} not valid."
    }
  }

  if (nzchar(error, keepNA = TRUE)) {

    info <- "Check the metadata in the {.cls elic_cat} object."

    cli::cli_abort(c(text,
                     "x" = error,
                     "i" = info),
                   call = rlang::caller_env())
  }
}

#' Check column format
#'
#' Checks if the given column is formatted as expected.
#'
#' @param x data.frame with the data to be checked.
#'
#' @return An error if the given column containing is malformed.
#' @noRd
#'
#' @author Sergio Vignali, Maude Vernet
check_column_format <- function(x, col) {

  if (col == "confidence") {
    x_nona <- tidyr::replace_na(x[[col]], 1)
    n_categories <- length(unique(x[["category"]]))
    diff <- rle(x_nona)[["lengths"]] %% n_categories

    if (sum(diff) == 0) {
      expected_values <- x[[col]]
    } else {
      # Create dummy wrong values
      expected_values <- seq_len(nrow(x))
    }
  } else {
    col_values <- unique(x[[col]])
    diff_cols <- setdiff(c("id", "option", "category"), col)
    col_1 <- unique(x[[diff_cols[[1]]]]) |>
      length()
    col_2 <- unique(x[[diff_cols[[2]]]]) |>
      length()

    if (col == "id") {
      expected_values <- rep(col_values, each = col_1 * col_2)
    } else if (col == "category") {
      expected_values <- rep(col_values, col_1 * col_2)
    } else {
      expected_values <- rep(col_values, col_1, each = col_2)
    }
  }

  if (!identical(x[[col]], expected_values)) {

    what <- switch(col,
                   "id" = "expert names",
                   "option" = "options",
                   "category" = "categories",
                   "confidence" = "confidence values")

    error <- "The column containing the {what} is not formatted as \\
              expected."
    info <- "See Data format in {.fn elicitr::cat_add_data}."

    cli::cli_abort(c("Malformatted dataset:",
                     "x" = error,
                     "i" = info),
                   call = rlang::caller_env())
  }
}

#' Check estimates
#'
#' Check if estimates for each expert and option sum to 1 or 100 and normalise
#' to 100.
#'
#' @param x data.frame with the data to be checked.
#'
#' @return An error if estimates for each expert and option don't sum to 1.
#' @noRd
#'
#' @author Sergio Vignali, Maude Vernet
check_sum_1 <- function(x) {

  sums <- x |>
    # Convert to factor to avoid unwanted reorder of the table rows
    dplyr::mutate("id" = factor(.data[["id"]],
                                levels = unique(.data[["id"]]))) |>
    dplyr::group_by(.data[["id"]], .data[["option"]]) |>
    dplyr::summarise(sum = sum(.data[["estimate"]]),
                     sum_NA = sum(.data[["estimate"]], na.rm = TRUE),
                     all_missing = all(is.na(.data[["estimate"]])),
                     .groups = "drop")
  sums_vector <- dplyr::pull(sums, #pull is basically the same as $
                             "sum")
  #sum is the sum of the estimates for each expert & option
  sums_vector_na <- dplyr::pull(sums,
                                "sum_NA")

  na_options <- is.na(sums_vector)
  all_missing <- sums[["all_missing"]]
  partial_missing <- na_options & !all_missing

  if (any(partial_missing)) {

    error <- "Expert {.val {sums[partial_missing, ]$id[1]}} gave estimates \\
    for only part of an option."

    fn <- as.list(sys.call(-1))[[1]]

    cli::cli_abort(c("Invalid raw data:",
                     "x" = error,
                     "i" = "Check raw data. See {.fn elicitr::{fn}} for more \\
                     information."),
                   call = rlang::caller_env())
  }

  tol <- 1.5e-8
  #in case estimates were given in proportions
  good_1 <- abs(sums_vector_na - 1) <= tol
  #in case estimates were given in percents
  good_100 <- abs(sums_vector_na - 100) <= tol

  invalid_sum <- !all_missing & !good_1 & !good_100
  total <- sum(invalid_sum)
  #if both are bad, then the sum is wrong

  if (total > 0) {

    idx <- which(invalid_sum)
    wrong_data <- sums[idx, ]

    if (total == 1) {
      error <- "Estimates of one expert and one option don't sum to 1 or 100."
    } else {
      error <- "Estimates of one/some experts for one/some options don't sum \\
                to 1 or 100."
    }

    msg <- paste0("{cli::symbol$bullet} Check {.field id} {.val ",
                  wrong_data[[1]],
                  "} for {.field option} {.val ",
                  wrong_data[[2]], "}: sum {.val {",
                  wrong_data[[3]], "}}")

    cli::cli_abort(c("Invalid value for {.arg estimate}:",
                     "x" = error,
                     msg),
                   call = rlang::caller_env())
  } else {

    #in case estimates were given in proportions
    has_1 <- any(good_1)

    #in case estimates were given in percents
    has_100 <- any(good_100)

    if (has_1) {

      if (has_100) {
        cli::cli_inform(c("i" = "Estimates sum to 100 for some \\
                          experts/options, and to 1 for others. \\
                          Rescaling the 1-sums to 100."))
      } else {
        cli::cli_inform(c("i" = "Estimates sum to 1. Rescaling to 100."))
      }

      # Rescaling to 100
      sums_flag <- sums |>
        # add a column to sums which flags the ones equal to 1
        dplyr::mutate(flag = good_1) |>
        # Keep only the columns id, option and flag from sums (drop sum column)
        dplyr::select("id", "option", "flag")

      x <- x |>
        # Join the flags to the original data
        dplyr::left_join(sums_flag, by = c("id", "option")) |>
        # Rescale only the flagged rows (if flag is true -> *100)
        dplyr::mutate("estimate" = dplyr::if_else(.data[["flag"]],
                                                  .data[["estimate"]] * 100,
                                                  .data[["estimate"]])) |>
        # Remove the flag column
        dplyr::select(-"flag")
    }
  }

  x
}

#' Check NAs
#'
#' Check that NAs happen for all categories in an option and for both confidence
#' and estimate columns
#'
#' @param x data.frame with the data to be checked.
#'
#' @return An error if only some values are NA for an expert in an option
#' @noRd
#'
#' @author Sergio Vignali, Maude Vernet
check_na <- function(x) {

  mismatch <- xor(is.na(x$confidence), is.na(x$estimate))
  #checks if exactly one of them is NA (logical, length = nrow(x))

  if (sum(mismatch) > 0) {
    error <- "Expert {.val {unique(x$id[which(mismatch)])}} reported NA \\
              values for only part of an estimation."

    fn <- as.list(sys.call(-1))[[1]]

    cli::cli_abort(c("Invalid value for {.arg confidence} or {.arg estimate}:",
                     "x" = error,
                     "i" = "Check raw data. See {.fn elicitr::{fn}} for more \\
                     information."),
                   call = rlang::caller_env())
  }
}

#' Check positive estimates and confidence
#'
#' Check that all estimates are positive and not higher than 100, and check that
#' confidence levels are higher than 50 and max 100.
#'
#' @param data data.frame with the data to be checked.
#'
#' @return An error if some values are not conform.
#' @noRd
#'
#' @author Maude Vernet
check_cat_values <- function(data) {

  estimates <- data[["estimate"]]
  confidence <- data[["confidence"]]

  # Missing values are checked separately.
  estimates <- estimates[!is.na(estimates)]
  confidence <- confidence[!is.na(confidence)]

  fn <- as.list(sys.call(-1))[[1]]
  if (any(estimates < 0 |
          estimates > 100)) {
    cli::cli_abort(c("Invalid estimates",
                     "x" = "Estimates must be nonnegative probabilities or \\
                   percentages.",
                     "i" = "See {.fn elicitr::{fn}} for more information."),
                   call = rlang::caller_env())
  }

  if (any(!is.finite(confidence) |
          confidence <= 50 |
          confidence > 100)) {
    cli::cli_abort(c("Invalid confidence values",
                     "x" = "Confidence must be greater than 50 and at most \\
                     100.",
                     "i" = "See {.fn elicitr::{fn}} for more information."),
                   call = rlang::caller_env())
  }
}
