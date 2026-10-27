# Checkers----

#' Check if the argument `x` is a character vector
#'
#' @param x the object to be checked.
#' @param arg_name character string with the name of the argument to be checked.
#'
#' @return An error if `x` is not a character vector.
#' @noRd
#'
#' @author Sergio Vignali
check_is_character <- function(x, arg_name) {

  if (!is.character(x)) {
    error <- "Argument {.arg {arg_name}} must be {.cls character} not \\
              {.cls {class(x)}}."
    cli::cli_abort(c("Invalid value for {.arg {arg_name}}:",
                     "x" = error,
                     "i" = "See {.fn elicitr::cat_start}."),
                   call = rlang::caller_env())
  }
}
