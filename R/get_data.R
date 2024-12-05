#' Get data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Get data from an `elicit` object.
#'
#' @inheritParams elic_cont_add_data
#' @param var character string with the name of the variable or character vector
#' with more variable names that you want to extract from the data. Use `all`
#' for all variables. Use `all` for all variable types. See Elicitation Types
#' for more.
#' @param var_types character string with short codes indicating the variable
#' type. See Variable Types for more.
#' @param elic_types character string with the short codes codes indicating the
#' elicitation type. Use `all` for all elicitation types. See Elicitation Types
#' for more.
#'
#' @details
#' One one optional argument can be specified. If more than one is provided, the
#' first of the following will be used: `var`, `var_types`, or `elic_types`.
#'
#'
#' @inheritSection elic_cont_start Variable Types
#' @inheritSection elic_cont_start Elicitation types
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
#' my_elicit <- elic_cont_start(var_names = c("var1", "var2", "var3"),
#'                              var_types = "ZNp",
#'                              elic_types = "134",
#'                              experts = 6) |>
#'   elic_cont_add_data(data_source = round_1, round = 1) |>
#'   elic_cont_add_data(data_source = round_2, round = 2)
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

  check_elicit(x)
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
    check_type_in_obj(x, var_types, type = "var_types")

    idx <- match(var_types, x[["var_types"]])
    var <- x[["var_names"]][idx]
  } else if (arg == "elic_types") {
    # Split and check elicitation types
    elic_types <- split_short_codes(elic_types, add_p = TRUE)
    check_arg_types(elic_types, type = "elic")
    check_type_in_obj(x, elic_types, type = "elic_types")

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
#' @param x `elicit` object.
#' @param var character with the name of the variable, or the word `all`.
#'
#' @return An error if `var` is not one of the variables of the `elicit` object,
#' or `all`.
#' @noRd
#'
#' @author Sergio Vignali
check_var <- function(x, var) {

  diff <- setdiff(var, c(x[["var_names"]], "all"))

  if (length(diff) > 0) {
    cli::cli_abort(c("Argument {.arg var} can be only a vector with a \\
                      combination of {.val {x$var_names}} or {.val all}:",
                     "x" = "The value{?s} {.val {diff}} {?is/are} invalid.",
                     "i" = "See {.fn elicitr::elic_cont_get_data}."),
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
    text <- "Only one optional argument can be specified, used the first one: \\
             {.arg {arg}}"
    cli::cli_warn(c(text,
                    "i" = "See Details in {.fn elicitr::elic_cont_get_data}."))
  } else {
    arg <- args[idx]
  }

  arg
}

#' Check type in object
#'
#' Check that the given variable or elicitation type/s is/are available in the
#' `elicit` object.
#'
#' @param obj an object of class `elicit`.
#' @param x character string with the value to be checked.
#' @param type character string, either `var_types` or `elic:types`.
#'
#' @return An error if the variable or elicitation type/s is/are not present in
#' the `elicit` object.
#' @noRd
#'
#' @author Sergio Vignali
check_type_in_obj <- function(obj,
                              x,
                              type) {
  obj_types <- obj[[type]]
  diff <- setdiff(x, obj_types)

  if (length(diff) > 0) {

    if (type == "var_types") {
      error <- "Variable type{?s} {.val {diff}} not present in the \\
                {.cls elicit} object."
      info <- "Available variable type{?s} {?is/are} {.val {obj_types}}"
    } else {
      error <- "Elicitation type{?s} {.val {diff}} not present in the \\
                {.cls elicit} object."
      info <- "Available elicitation type{?s} {?is/are} {.val {obj_types}}"
    }

    cli::cli_abort(c("Invalid value for {.arg {type}}:",
                     "x" = error,
                     "i" = info),
                   call = rlang::caller_env())
  }
}
