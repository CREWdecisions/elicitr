# Checkers----

#' Check round data
#'
#' Check if the number of rows on the dataset are the same as the number of
#' experts. If there are more data than experts, raises an error. If there are
#' more experts than data, raise a warn and eventually adds NAs only for
#' Round 1.
#'
#' @param data tibble with data belonging to the first round.
#' @param experts numeric providing the number of experts.
#' @param round integer indicating the elicitation round.
#'
#' @return An error or the data to add to the Round, eventually with added NAs.
#' @noRd
#'
#' @author Sergio Vignali
check_round_data <- function(data, experts, round) {

  if (nrow(data) != experts) {

    # Number of experts and rows in data are not the same
    if (nrow(data) > experts) {

      # More data than experts ==> Raise error
      text <- "The dataset for {.val Round {round}} contains \\
                {.val {nrow(data)}} rows but expects estimates from \\
                {.val {experts}} experts."

      if (round == 1) {
        info <- "Check raw data or modify the {.cls elic_cont} object by \\
                 setting the number of experts to {.val {nrow(data)}} in \\
                 {.fn elicitr::cont_start} and load the data again."
      } else {
        info <- "Check raw data or modify the {.cls elic_cont} object by \\
                 setting the number of experts to {.val {nrow(data)}} in \\
                 {.fn elicitr::cont_start} and load the data for \\
                 {.val Round 1} and {.val Round 2} again."
      }

      fn <- as.list(sys.call(-1))[[1]]

      info2 <- "See {.fn elicitr::{fn}} for more information."

      cli::cli_abort(c("Incorrect number of rows in dataset:",
                       "x" = text,
                       "i" = info,
                       " " = info2),
                     call = rlang::caller_env())
    } else {
      # More experts than data ==> raise a warn
      n_round <- nrow(data)
      n_diff <- experts - n_round

      # Add NAs is delegated to omogenise_datasets() when round == 2
      if (round == 1) {
        # Add NAs
        data <- add_nas_rows(data, experts)
      }

      warn <- "The dataset for {.val Round {round}} has {.val {n_round}} rows \\
               but expects {.val {experts}} experts. {.val {NA}}s added to \\
               missing {.cls id}."
      info <- "Check raw data and if you want to update the dataset use \\
               {.fn elicitr::cont_add_data} with {.code overwrite = TRUE}."
      cli::cli_warn(c("!" = warn,
                      "i" = info))
    }
  }

  data
}

#' Check data type
#'
#' Check if the data types are correct according to the metadata stored in the
#' [elic_cont][elicitr::elic_cont] object.
#'
#' @param x [elic_cont][elicitr::elic_cont] object containing the metadata.
#' @param data [`tibble`][tibble::tibble] with the data to be checked.
#'
#' @returns An error if the data types are not correct.
#' @noRd
#'
#' @author Sergio Vignali and Maude Vernet
check_data_types <- function(x, data) {

  var_names <- x[["var_names"]]
  var_types <- x[["var_types"]]
  elic_types <- x[["elic_types"]]

  error_all_vartype <- character()
  error_all_conf <- character()
  error_all_na <- character()
  warn_all_na <- character()
  for (i in seq_along(var_names)) {

    expected_cols <- paste0(var_names[[i]],
                            "_",
                            var_labels[[elic_types[[i]]]])

    idx <- names(data) %in% expected_cols

    error_na <- list(error = FALSE,
                     warn = FALSE,
                     message = NA)
    error_na <- check_na_blocks(data = data[, idx, drop = FALSE],
                                experts_idx = data[["id"]],
                                var_name = var_names[[i]])

    if (isTRUE(error_na[["error"]])) {
      error_all_na <- c(error_all_na, error_na[["message"]])
    }

    if (isTRUE(error_na[["warn"]])) {
      warn_all_na <- c(warn_all_na, error_na[["message"]])
    }

    df <- unlist(data[, idx, drop = FALSE])

    error_conf <- list(error = FALSE,
                       message = NA)

    error_type_message <- switch(var_types[[i]],
                                 Z = check_is_integer(df,
                                                      var_names[[i]]),
                                 N = check_is_positive_integer(df,
                                                               var_names[[i]]),
                                 z = check_is_negative_integer(df,
                                                               var_names[[i]]),
                                 s = check_is_positive_real(df,
                                                            var_names[[i]]),
                                 r = check_is_negative_real(df,
                                                            var_names[[i]]),
                                 p = check_is_probability(df,
                                                          var_names[[i]]),
                                 list(error = FALSE,
                                      message = NA))

    if (isTRUE(error_type_message[["error"]])) {
      error_all_vartype <- c(error_all_vartype, error_type_message[["message"]])
    }

    if (elic_types[i]  == "4p") {
      error_conf <- check_conf(df, var_names[i])
    }

    if (isTRUE(error_conf[["error"]])) {
      error_all_conf <- c(error_all_conf, error_conf[["message"]])
    }
  }

  #warn in na
  if (length(warn_all_na) > 0) {
    bullets_all_na_warn <- stats::setNames(warn_all_na,
                                           rep("!", length(warn_all_na)))
    info <- "If this is a mistake and you want to change and update the \\
             dataset use {.fn elicitr::cont_add_data} with \\
             {.code overwrite = TRUE}."
    cli::cli_warn(c(bullets_all_na_warn,
                    "i" = info),
                  .verbatim = TRUE)
  }

  #error in na
  if (length(error_all_na) > 0) {
    bullets_all_na_error <- stats::setNames(error_all_na,
                                            rep("x", length(error_all_na)))
    info <- "Experts cannot report estimates for only part of a variable."
    fn <- as.list(sys.call(-1))[[1]]
    cli::cli_abort(c("Invalid raw data:",
                     bullets_all_na_error,
                     "i" = info,
                     " " = "Fix raw data. For more information, see \\
                            {.fn {fn}}."),
                   .verbatim = TRUE,
                   call = rlang::caller_env(n = 1))
  }

  #error in variable type
  if (length(error_all_vartype) > 0) {
    bullets_all_vartype <- stats::setNames(error_all_vartype,
                                           rep("x", length(error_all_vartype)))
    info <- "Check the raw data. If this is a mistake, recreate the \\
             metadata  with {.fn elicitr::cont_start} (make sure to change \\
             the argument {.arg var_types} accordingly), and reload the data."

    cli::cli_abort(c("Incorrect {.arg var_types}:",
                     bullets_all_vartype,
                     "i" = info),
                   .verbatim = TRUE,
                   call = rlang::caller_env(n = 1))
  }

  #error in confidence
  if (length(error_all_conf) > 0) {
    bullets_all_conf <- stats::setNames(error_all_conf,
                                        rep("x", length(error_all_conf)))
    fn <- as.list(sys.call(-1))[[1]]
    info <- "Check the raw data. For more information, see {.fn {fn}}."

    cli::cli_abort(c("Incorrect confidence values:",
                     bullets_all_conf,
                     "i" = info),
                   .verbatim = TRUE,
                   call = rlang::caller_env(n = 1))
  }
}

#' Check if the argument `x` is a integer
#'
#' @param x numeric vector to be checked.
#' @param v character string with the name of the variable to be checked.
#'
#' @returns A list with error if `x` contains some non integer numbers.
#' @noRd
#'
#' @author Sergio Vignali and Maude Vernet
check_is_integer <- function(x, v) {

  idx <- grepl("_conf[0-9]+$", names(x))
  x <- x[!idx]
  x <- x[!is.na(x)]

  error <- list(error = FALSE,
                message = NA)
  if (!all(x %% 1 == 0)) {
    fn <- as.list(sys.call(-2))[[1]]

    error[["error"]] <- TRUE
    error[["message"]] <-
      cli::format_inline("The argument {.arg var_types} of {.val {v}} was set \\
                          to {.val Z} in the metadata but variable {.val {v}} \\
                          contains estimates that are not round numbers.")
  }
  error
}

#' Check if the argument `x` is a positive integer
#'
#' @param x numeric vector to be checked.
#' @param v character string with the name of the variable to be checked.
#'
#' @returns A list with error if `x` contains some non positive integer numbers.
#' @noRd
#'
#' @author Sergio Vignali and Maude Vernet
check_is_positive_integer <- function(x, v) {

  error <- list(error = FALSE,
                message = NA)
  idx <- grepl("_conf[0-9]+$", names(x))
  x <- x[!idx]
  x <- x[!is.na(x)]

  if (!all(x %% 1 == 0) || any(x < 0)) {

    error[["error"]] <- TRUE
    error[["message"]] <-
      cli::format_inline("The argument {.arg var_types} of {.val {v}} was set \\
                          to {.val N} in the metadata but variable {.val {v}} \\
                          contains estimates that are not positive round \\
                          numbers or 0.")
  }
  error
}

#' Check if the argument `x` is a negative integer
#'
#' @param x numeric vector to be checked.
#' @param v character string with the name of the variable to be checked.
#'
#' @returns A list with error if `x` contains some non negative integer numbers.
#' @noRd
#'
#' @author Sergio Vignali and Maude Vernet
check_is_negative_integer <- function(x, v) {

  idx <- grepl("_conf[0-9]+$", names(x))
  x <- x[!idx]
  x <- x[!is.na(x)]

  error <- list(error = FALSE,
                message = NA)
  if (!all(x %% 1 == 0) || any(x >= 0)) {

    error[["error"]] <- TRUE
    error[["message"]] <-
      cli::format_inline("The argument {.arg var_types} of {.val {v}} was set \\
                          to {.val z} in the metadata but variable {.val {v}} \\
                          contains estimates that are not negative round \\
                          numbers.")
  }
  error
}

#' Check if the argument `x` is a positive real number
#'
#' @param x numeric vector to be checked.
#' @param v character string with the name of the variable to be checked.
#'
#' @returns A list with error if `x` contains some non positive numbers.
#' @noRd
#'
#' @author Sergio Vignali and Maude Vernet
check_is_positive_real <- function(x, v) {

  idx <- grepl("_conf[0-9]+$", names(x))
  x <- x[!idx]
  x <- x[!is.na(x)]

  error <- list(error = FALSE,
                message = NA)
  if (any(x < 0)) {

    error[["error"]] <- TRUE
    error[["message"]] <-
      cli::format_inline("The argument {.arg var_types} of {.val {v}} was set \\
                          to {.val s} in the metadata but variable {.val {v}} \\
                          contains estimates that are not positive round \\
                          numbers or 0.")
  }
  error
}

#' Check if the argument `x` is a negative real number
#'
#' @param x numeric vector to be checked.
#' @param v character string with the name of the variable to be checked.
#'
#' @returns A list with error if `x` contains some non negative numbers.
#' @noRd
#'
#' @author Sergio Vignali and Maude Vernet
check_is_negative_real <- function(x, v) {

  idx <- grepl("_conf[0-9]+$", names(x))
  x <- x[!idx]
  x <- x[!is.na(x)]

  error <- list(error = FALSE,
                message = NA)
  if (any(x >= 0)) {

    error[["error"]] <- TRUE
    error[["message"]] <-
      cli::format_inline("The argument {.arg var_types} of {.val {v}} was set \\
                          to {.val r} in the metadata but variable {.val {v}} \\
                          contains estimates that are not not strictly \\
                          negative numbers.")
  }
  error
}

#' Check if the argument `x` is a probability
#'
#' @param x numeric vector to be checked.
#' @param v character string with the name of the variable to be checked.
#'
#' @returns A list with error if `x` contains some values not in the range 0, 1.
#' @noRd
#'
#' @author Sergio Vignali and Maude Vernet
check_is_probability <- function(x, v) {

  idx <- grepl("_conf[0-9]+$", names(x))
  x <- x[!idx]
  x <- x[!is.na(x)]

  error <- list(error = FALSE,
                message = NA)
  if (!all(x >= 0) || !all(x <= 1)) {

    error[["error"]] <- TRUE
    error[["message"]] <-
      cli::format_inline("The argument {.arg var_types} of {.val {v}} was set \\
                          to {.val p} in the metadata but variable {.val {v}} \\
                          contains estimates that are not in the range [0,1].")
  }
  error
}

#' Check if the argument `x` has the correct confidence levels
#'
#' @param x numeric vector to be checked.
#' @param v character string with the name of the variable to be checked.
#'
#' @returns An error if `x` contains some values not in the range 50, 100.
#' @noRd
#'
#' @author Sergio Vignali and Maude Vernet
check_conf <- function(x, v) {

  idx <- grepl("_conf[0-9]+$", names(x))
  x <- x[idx]
  x <- x[!is.na(x)]

  error <- list(error = FALSE,
                message = NA)
  if (!all(x > 50) || !all(x <= 100)) {
    error[["error"]] <- TRUE
    error[["message"]] <-
      cli::format_inline("The variable {.val {v}} contains confidence \\
                          estimates not in the range (50, 100].")
  }
  error
}

#' Check NA blocks
#'
#' @param data the data of the variable to be checked.
#' @param var_name character string with the name of the variable to be checked.
#' @param experts_idx character vector with the name of the experts.
#'
#' @returns a list with the relevant messages or errors.
#' @noRd
#'
#' @author Maude Vernet
check_na_blocks <- function(data, experts_idx, var_name) {

  na_data_all <- NULL
  na_data_partial <- NULL

  for (i in seq_len(nrow(data))) {
    if (anyNA(data[i, ])) {
      if (all(is.na(data[i, ]))) {
        na_data_all <- c(na_data_all, i)
      } else {
        na_data_partial <- c(na_data_partial, i)
      }
    }
  }

  error <- list(error = FALSE,
                warn = FALSE,
                message = NA)
  if (!is.null(na_data_partial)) {
    expert_na_partial <- experts_idx[na_data_partial]

    error[["error"]] <- TRUE
    error[["message"]] <-
      cli::format_inline("{cli::qty(length(na_data_partial))} Expert{?s} \\
                          {.val {expert_na_partial}} replied to only one part \\
                          of the variable {.val {var_name}}.")
  } else if (!is.null(na_data_all)) {
    expert_na_all <- experts_idx[na_data_all]

    error[["warn"]] <- TRUE
    error[["message"]] <-
      cli::format_inline("{cli::qty(length(na_data_all))} Expert{?s} \\
                          {.val {expert_na_all}} did not reply to variable \\
                          {.val {var_name}}.")
  }
  error
}
