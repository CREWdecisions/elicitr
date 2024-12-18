#' Get data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `cont_get_data()` gets data from an [elic_cont] object.
#'
#' @inheritParams cont_add_data
#' @param var character string with the name of the variable or character vector
#' with more variable names that you want to extract from the data. Use `all`
#' for all variables.
#' @param var_types character string with short codes indicating the variable
#' type. See Variable types for more.
#' @param elic_types character string with the short codes codes indicating the
#' elicitation type. Use `all` for all elicitation types. See Elicitation Types
#' for more.
#'
#' @details
#' One one optional argument can be specified. If more than one is provided, the
#' first of the following will be used: `var`, `var_types`, or `elic_types`.
#'
#'
#' @inheritSection cont_start Variable types
#' @inheritSection cont_start Elicitation types
#'
#' @return A [`tibble`][tibble::tibble] with the extracted data.
#' @export
#'
#' @family cont data helpers
#'
#' @author Sergio Vignali
#'
#' @examples
#' # Create the elict object and add data for the first and second round from a
#' # data.frame.
#' my_elicit <- cont_start(var_names = c("var1", "var2", "var3"),
#'                         var_types = "ZNp",
#'                         elic_types = "134",
#'                         experts = 6) |>
#'   cont_add_data(data_source = round_1, round = 1) |>
#'   cont_add_data(data_source = round_2, round = 2)
#'
#' # Get all data from round 1
#' elic_cont_get_data(my_elicit, round = 1)
#'
#' # Get data by variable name----
#' # Get data for var3 from round 2
#' elic_cont_get_data(my_elicit, round = 2, var = "var3")
#'
#' # Get data for var1 and var2 from round 1
#' elic_cont_get_data(my_elicit, round = 1, var = c("var1", "var2"))
#'
#' # Get data by variable type----
#' # Get data for variables containing integer numbers
#' elic_cont_get_data(my_elicit, round = 2, var_types = "Z")
#' # Get data for variables containing positive integers and probabilities
#' elic_cont_get_data(my_elicit, round = 2, var_types = "Np")
#'
#' # Get data by elicitation type----
#' # Get data for three points estimates
#' elic_cont_get_data(my_elicit, round = 2, elic_types = "3")
#' # Get data for one and four points estimates
#' elic_cont_get_data(my_elicit, round = 2, elic_types = "14")
elic_cont_get_data <- function(x,
                               round,
                               ...,
                               var = "all",
                               var_types = "all",
                               elic_types = "all") {

  check_elic_obj(x, type = "cont")
  check_round(round)
  check_var(x, var)

  # Check that variable and elicitation types are a single string
  check_arg_length(var_types, type = "var")
  check_arg_length(elic_types, type = "elic")

  arg <- check_optional_args(var, var_types, elic_types)

  if (arg == "all") {

    return(x[["data"]][[round]])

  } else if (arg == "var_types") {
    # Split and check variable types
    var_types <- split_short_codes(var_types)
    check_arg_types(var_types, type = "var")
    check_value_in_element(x, element = "var_types", value = var_types)

    idx <- match(var_types, x[["var_types"]])
    var <- x[["var_names"]][idx]
  } else if (arg == "elic_types") {
    # Split and check elicitation types
    elic_types <- split_short_codes(elic_types, add_p = TRUE)
    check_arg_types(elic_types, type = "elic")
    check_value_in_element(x, element = "elic_types", value = elic_types)

    idx <- match(elic_types, x[["elic_types"]])
    var <- x[["var_names"]][idx]
  }

  if (length(var) == 1) {

    pattern <- var

  } else {
    pattern <- paste(var, collapse = "|")
  }

  idx <- c(TRUE, grepl(pattern, colnames(x[["data"]][[round]][, -1])))

  x[["data"]][[round]][, idx]
}

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

  if (length(diff) > 0) {
    cli::cli_abort(c("Invalid value for {.arg var}:",
                     "x" = "Variable{?s} {.val {diff}} not present in the \\
                            {.cls elic_cont} object.",
                     "i" = "Available variable{?s} {?is/are} {.val {vars}}."),
                   call = rlang::caller_env())
  }
}

#' Check optional arguments
#'
#' Check that only one optional argument is passed, if not, return the first one
#' and rise a warning.
#'
#' @inheritParams elic_cont_get_data
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
                    "i" = "See Details in {.fn elicitr::elic_cont_get_data}."))
  } else {
    arg <- args[idx]
  }

  arg
}
