#' Start elicitation
#'
#' `r lifecycle::badge("experimental")` `elic_start()` initialises an `elicit`
#' object which stores important metadata for the data collected during the
#' elicitation process.
#'
#' @param var_names character vector with the name of the estimated variables.
#' @param var_types character with short codes indicating the variable type. If
#' only one `var_type` is provided, its value is recycled for all variables. See
#' Variable Types for more.
#' @param elic_types character with short codes indicating the elicitation type.
#' If only one `elic_type` is provided, its value is recycled for all variables.
#' See Elicitation Types for more.
#' @param ... Unused arguments, included only for future extensions of the
#' function.
#' @param title character, used to bind a name to the object.
#'
#' @section Variable Types:
#'
#' Variable types must be provided as a single string containing short codes,
#' e.g. "pPN".
#'
#' Valid short codes are:
#'
#' * `Z`: _integers_, when the estimate must be an integer number in the
#' interval (-Inf, Inf).
#'
#' * `N`: _positive integers_, when the estimate must be an integer number in
#' the interval (0, Inf).
#'
#' * `z`: _negative integers_, when the estimate must be an integer number in
#' the interval (-Inf, 0].
#'
#' * `R`: _reals_, when the estimate must be a real number in the interval
#' (-Inf, Inf).
#'
#' * `s`: _positive reals_, when the estimate must be a real number in the
#' interval (0, Inf).
#'
#' * `r`: _negative reals_, when the estimate must be a real number in the
#' interval (-Inf, ].
#'
#' * `p`: _probability_, when the estimate must be a real number in the interval
#' (0, 1).
#'
#' @section Elicitation types:
#'
#' Elicitation types must be provided as a single string containing short codes,
#' e.g. "134".
#'
#' Valid short codes are:
#'
#' * `1`: _one point elicitation_, when only the best estimate is provided.
#' * `3`: _three points elicitation_, when the minimum, maximum, and best
#' estimates are provided.
#' * `4`: _four points elicitation_, when the minimum, maximum, best, and
#' confidence estimates are provided.
#'
#'
#' @return An object of class `elicit`.
#' @export
#'
#' @author Sergio Vignali
#'
#' @examples
elic_start <- function(var_names,
                       var_types,
                       elic_types,
                       ...,
                       title = "Elicitation") {

  # Check that variable and elicitation types are a single string
  check_arg_length(var_types,
                   "var")
  check_arg_length(elic_types,
                   "elic")

  n_vars <- length(var_names)

  # Split variable types
  var_types <- split_short_codes(var_types)

  # Split elicitation types and add a "p" character
  elic_types <- split_short_codes(elic_types,
                                  add_p = TRUE)

  # Check variable types
  check_arg_types(var_types,
                  type = "var")

  # Check elicitation types
  check_arg_types(elic_types,
                  type = "elic")

  # Recycle variable and elicitation types if necessary
  if (n_vars > 1 & length(var_types) == 1) {
    var_types <- rep(var_types, n_vars)
  }

  if (n_vars > 1 & length(elic_types) == 1) {
    elic_types <- rep(elic_types, n_vars)
  }

  # Check arguments for compatibility
  check_arg_mism(var_names,
                 var_types,
                 elic_types)

  obj <- list(var_names = var_names,
              var_types = var_types,
              elic_types = elic_types,
              data = list(round_1 = NULL,
                          round_2 = NULL))

  structure(obj,
            class = "elicit",
            title = title)
}

#' @export
print.elicit <- function(x, ...) {

  rounds <- lengths(x$data) |>
    sum()

  title <- attr(x, "title")

  cli::cli_h2(title)
  cli::cli_li("Variable{?s}: {.field {x$var_names}}")
  cli::cli_li("Variable type{?s}: {.field {x$var_types}}")
  cli::cli_li("Elicitation type{?s}: {.field {x$elic_types}}")
  cli::cli_li("Number of rounds: {.val {rounds}}")
  cli::cli_text()

  if (rounds > 0) {
    cli::style_underline("Round 1") |>
      cli::col_magenta() |>
      cli::cli_text()
    print(x$data$round_1)
  }

  if (rounds == 2) {
    cli::style_underline("Round 2") |>
      cli::col_magenta() |>
      cli::cli_text()
    print(x$data$round_2)
  }

  invisible(x)
}
