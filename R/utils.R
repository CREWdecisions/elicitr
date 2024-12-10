# Checkers----

#' Check elicit
#'
#' Check if `x` is an [elic_cont] or an [elic_cat] object.
#'
#' @param x the object to be checked.
#' @param type character, either _cont_ or _cat_.
#'
#' @return An error if `x` is not and [elic_cont]or an [elic_cat] object.
#' @noRd
#'
#' @author Sergio Vignali
check_elic_obj <- function(x,
                           type) {

  cl <- paste0("elic_", type)

  if (!inherits(x, cl)) {

    fn <- as.list(sys.call(-1))[[1]]

    error <- "Argument {.arg x} must be an object of class {.cls {cl}} and \\
              not of class {.cls {class(x)}}."

    cli::cli_abort(c("Invalid value for {.arg x}:",
                     "x" = error,
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

    cli::cli_abort(c("Incorrect value for {.arg round}:",
                     "x" = "{.arg round} can only be {.val {1}} or {.val {2}}.",
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
    short_codes <- paste(x, collapse = "")
    wrong_values <- paste0("c(", paste0('"', x, '"', collapse = ", "), ")")

    error <- "The value provided for {.arg {type}_types} should be a \\
              character string of short codes, i.e. {.val {short_codes}} and \\
              not {.code {wrong_values}}."

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

    error <- "{.val {diff}} {?is/are} not in the list of available short \\
              codes."

    cli::cli_abort(c("Incorrect value for {.arg {type}_types}:",
                     "x" = error,
                     "i" = "See {.str {sect}} in {.fn elicitr::{fn}}."),
                   call = rlang::caller_env())
  }
}

#' Check experts argument
#'
#' Check if the value for the `experts` argument is a number.
#'
#' @param x the value to be checked.
#'
#' @return An error if `x` is not a single number and its value is neither 1 nor
#' 2.
#' @noRd
#'
#' @author Sergio Vignali
check_experts_arg <- function(x) {

  error <- ""

  if (!is.numeric(x)) {
    raise_error <- TRUE
    error <- "Argument {.arg experts} must be {.cls numeric} not \\
              {.cls {class(x)}}."
  } else if (length(x) > 1) {
    raise_error <- TRUE
    error <- "Argument {.arg experts} must be a single number not a \\
              vector of length {.val {length(x)}}."
  }

  if (nchar(error) > 0) {

    fn <- as.list(sys.call(-1))[[1]]

    cli::cli_abort(c("Incorrect value for {.arg experts}:",
                     "x" = error,
                     "y" = "See {.fn elicitr::{fn}}."),
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
