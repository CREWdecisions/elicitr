# Checkers----

#' Check variable
#'
#' Check the argument `var` for allowed values.
#'
#' @param x [elic_cont] object.
#' @param var character with the name of the variable, or the word `all`.
#'
#' @return An error if `var` is not one of the variables of the [elic_cont]
#' object, or `all`.
#'
#' @noRd
#'
#' @author Sergio Vignali
check_var <- function(x, var) {

  vars <- x[["var_names"]]
  diff <- setdiff(var, c(vars, "all"))
  fn <- as.list(sys.call(-1))[[1]]

  if (length(diff) > 0) {
    cli::cli_abort(c("Invalid value for {.arg var}:",
                     "x" = "Variable{?s} {.val {diff}} not present in the \\
                            {.cls elic_cont} object.",
                     "i" = "Available {.arg var_names} {?is/are} \\
                            {.val {vars}}.",
                     " " = "See {.fn elicitr::{fn}} for more information."),
                   call = rlang::caller_env())
  }
}

#' Check optional arguments
#'
#' Check that only one optional argument is passed, if not, return the first one
#' and rise a warning.
#'
#' @inheritParams cont_get_data
#'
#' @return Character string with one of `var`, `var_types`, `elic_types`. Warns
#' if more than one optional argument is provided.
#' @noRd
#'
#' @author Sergio Vignali
check_optional_args <- function(var, var_types, elic_types) {

  if (length(var) > 1) {
    var <- "selected_vars"
  }

  idx <- c(var, var_types, elic_types) != "all"
  args <- c("var", "var_types", "elic_types")

  if (sum(idx) == 0) {
    arg <- "all"
  } else if (sum(idx) > 1) {
    arg <- args[idx][[1]]
    text <- "Only one optional argument can be specified, used the first one \\
             provided: {.arg {arg}}"
    cli::cli_warn(c(text,
                    "i" = "See Details in {.fn elicitr::cont_get_data}."))
  } else {
    arg <- args[idx]
  }

  arg
}
