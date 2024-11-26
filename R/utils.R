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
check_elicit <- function(x, fun) {

  if (!inherits(x, "elicit")) {
    cli::cli_abort(c("Argument {.arg x} must be an object of class \\
                      {.cls elicit}:",
                     "x" = "An object of class {.cls {class(x)}} is invalid.",
                     "y" = "See {.fn elicitr::{fun}}."),
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
check_round <- function(x, fun) {

  if (x > 2 || x <= 0) {
    cli::cli_abort(c("Argument {.arg round} can be only {.val {1}} or \\
                      {.val {2}}:",
                     "x" = "The value {.val {x}} is invalid.",
                     "i" = "See {.fn elicitr::{fun}}."),
                   call = rlang::caller_env())
  }
}
