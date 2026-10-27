#' Get data
#'
#' @description
#' `r lifecycle::badge("stable")`
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
#' @author Sergio Vignali and Maude Vernet
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
#' cont_get_data(my_elicit, round = 1)
#'
#' # Get data by variable name----
#' # Get data for var3 from round 2
#' cont_get_data(my_elicit, round = 2, var = "var3")
#'
#' # Get data for var1 and var2 from round 1
#' cont_get_data(my_elicit, round = 1, var = c("var1", "var2"))
#'
#' # Get data by variable type----
#' # Get data for variables containing integer numbers
#' cont_get_data(my_elicit, round = 2, var_types = "Z")
#' # Get data for variables containing positive integers and probabilities
#' cont_get_data(my_elicit, round = 2, var_types = "Np")
#'
#' # Get data by elicitation type----
#' # Get data for three points estimates
#' cont_get_data(my_elicit, round = 2, elic_types = "3")
#' # Get data for one and four points estimates
#' cont_get_data(my_elicit, round = 2, elic_types = "14")
cont_get_data <- function(x,
                          round,
                          ...,
                          var = "all",
                          var_types = "all",
                          elic_types = "all") {

  check_elic_obj(x, type = "cont", metadata = FALSE)
  check_round(round)
  check_empty_obj(x, type = "cont", round_topic = round)
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

    idx <- which(x[["var_types"]] %in% var_types)
    var <- x[["var_names"]][idx]
  } else if (arg == "elic_types") {
    # Split and check elicitation types
    elic_types <- split_short_codes(elic_types, add_p = TRUE)
    check_arg_types(elic_types, type = "elic")
    check_value_in_element(x, element = "elic_types", value = elic_types)

    idx <- which(x[["elic_types"]] %in% elic_types)
    var <- x[["var_names"]][idx]
  }

  # Identify all requested variables in their original order
  selected <- x[["var_names"]] %in% var

  # Construct their exact column names, including "id"
  expected_cols <- get_col_names(
    var_names = x[["var_names"]][selected],
    elic_types = x[["elic_types"]][selected]
  )

  data <- x[["data"]][[round]]
  idx <- names(data) %in% expected_cols

  data[, idx, drop = FALSE]
}
