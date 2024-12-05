# Checkers----

#' Check elicit
#'
#' Check if `x` is an `elicit` object.
#'
#' @param x the object to be checked.
#'
#' @return An error if `x` is not and `elicit` object.
#' @noRd
#'
#' @author Sergio Vignali
check_elicit <- function(x) {

  if (!inherits(x, "elicit")) {

    fn <- as.list(sys.call(-1))[[1]]

    cli::cli_abort(c("Argument {.arg x} must be an object of class \\
                      {.cls elicit}:",
                     "x" = "An object of class {.cls {class(x)}} is invalid.",
                     "y" = "See {.fn elicitr::{fn}}."),
                   call = rlang::caller_env())
  }
}

#' Check round
#'
#' Check if the value for the `round` argument. It can be only `1` or `2`.
#'
#' @param x the value to be checked.
#'
#' @return An error if `x` is neither `1` nor `2`.
#' @noRd
#'
#' @author Sergio Vignali
check_round <- function(x) {

  if (x > 2 || x <= 0) {

    fn <- as.list(sys.call(-1))[[1]]

    cli::cli_abort(c("Argument {.arg round} can be only {.val {1}} or \\
                      {.val {2}}:",
                     "x" = "The value {.val {x}} is invalid.",
                     "i" = "See {.fn elicitr::{fn}}."),
                   call = rlang::caller_env())
  }
}

#' Check argument length
#'
#' `check_arg_length()` throws an error if the length of the provided argument
#' is greater than 1.
#'
#' @param x value passed to `elic_cont_start()` for the variable or elicitation
#' type.
#' @param type character, either _var_ for `var_types` or _elic_ for
#' `elic_types`.
#'
#' @return An error if the length of `x` is greater than 1.
#' @noRd
#'
#' @author Sergio Vignali
check_arg_length <- function(x,
                             type) {

  n <- length(x)

  if (n > 1) {

    fn <- as.list(sys.call(-1))[[1]]

    sect <- switch(type,
                   var = "Variable Types",
                   elic = "Elicitation types")

    error <- "The value provided for {.arg {type}_types} is a character \\
              vector of length {.val {n}} but should be a single string with \\
              short codes."

    cli::cli_abort(c("Incorrect value for {.arg {type}_types}:",
                     "x" = error,
                     "i" = "See {.str {sect}} in {.fn elicitr::{fn}}."),
                   call = rlang::caller_env())
  }
}

#' Check argument types
#'
#' `check_arg_types()` throws an error if the short codes are not allowed.
#'
#' @param x character containing short codes for variable or elicitation types.
#' @param type character, either _var_ for `var_types` or _elic_ for èlic_types.
#'
#' @return An error if the short codes are not allowed.
#' @noRd
#'
#' @author Sergio Vignali
check_arg_types <- function(x,
                            type) {

  if (type == "elic") {
    # Check allowed elicitation types
    diff <- setdiff(x, names(var_labels)) |>
      unique()
  } else {
    # Check allowed variable types
    diff <- setdiff(x, c("Z", "N", "z", "R", "s", "r", "p")) |>
      unique()
  }

  if (length(diff) > 0) {
    diff <- gsub(pattern = "p",
                 replacement = "",
                 x = diff,
                 fixed = TRUE)

    sect <- switch(type,
                   var = "Variable Types",
                   elic = "Elicitation types")

    fn <- as.list(sys.call(-1))[[1]]

    error <- "The incorrect short code{?s} {?is/are} {.val {diff}}."

    cli::cli_abort(c("Incorrect value for {.arg {type}_types}:",
                     "x" = error,
                     "i" = "See {.str {sect}} in {.fn elicitr::{fn}}."),
                   call = rlang::caller_env())
  }
}

# Helpers----

#' Split short codes
#'
#' `split_short_codes()` converts the string containing short codes to a
#' character vector with elements corresponding to each short code.
#'
#' @param x character containing the short codes.
#' @param add_p logical, whether to add the "p" character to each short code.
#'
#' @return A character vector with the short codes.
#' @noRd
#'
#' @author Sergio Vignali
split_short_codes <- function(x,
                              add_p = FALSE) {

  output <- stringr::str_split_1(x,
                                 pattern = "")

  if (add_p) {
    output <- output |>
      paste0("p")
  }

  output
}
