#' Get data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Get data from an `elicit` object.
#'
#' @inheritParams elic_add_data
#' @param var character string with the name of the variable or character vector
#' with more variable names that you want to extract from the data. Use `all`
#' for all variables.
#'
#' @return A [`tibble`][tibble::tibble] with the extracted data.
#' @export
#'
#' @family data helpers
#'
#' @author Sergio Vignali
#'
#' @examples
#' # Create the elict object and add data for the first and second round from a
#' # data.frame.
#' my_elicit <- elic_start(var_names = c("var1", "var2", "var3"),
#'                         var_types = "ZNp",
#'                         elic_types = "134",
#'                         experts = 6) |>
#'   elic_add_data(x, data_source = round_1, round = 1) |>
#'   elic_add_data(data_source = round_2, round = 2)
#'
#' # Get all data from round 1
#' elic_get_data(my_elicit, round = 1)
#'
#' # Get data for var3 from round 2
#' elic_get_data(my_elicit, round = 2, var = "var3")
#'
#' # Get data for var1 and var2 from round 1
#' elic_get_data(my_elicit, round = 1, var = c("var1", "var2"))

elic_get_data <- function(x,
                          round,
                          ...,
                          var = "all") {

  check_elicit(x, fun = "elic_get_data")
  check_round(round, fun = "elic_get_data")
  check_var(x, var)

  if (length(var) == 1) {

    if (var == "all") {
      return(x$data[[round]])
    }

    pattern <- var

  } else {
    pattern <- paste(var, collapse = "|")
  }

  idx <- c(TRUE, grepl(pattern, colnames(x$data[[round]][, -1])))

  x$data[[round]][, idx]
}

#' Check variable
#'
#' Check the argument `var` for allowed values.
#'
#' @param x `elicit` object.
#' @param var character with the name of the variable, or the word `all`.
#'
#' @return An error if `var` is not one of the variables of the `elicit` object,
#' or `all`.
#' @noRd
#'
#' @author Sergio Vignali
check_var <- function(x, var) {

  diff <- setdiff(var, c(x$var_names, "all"))

  if (length(diff) > 0) {
    cli::cli_abort(c("Argument {.arg var} can be only a vector with a \\
                      combination of {.val {x$var_names}} or {.val all}:",
                     "x" = "The value{?s} {.val {diff}} {?is/are} invalid.",
                     "i" = "See {.fn elicitr::elic_get_data}."),
                   call = rlang::caller_env())
  }
}
