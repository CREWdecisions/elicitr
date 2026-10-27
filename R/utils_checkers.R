# Checkers----

#' Check elicit
#'
#' Check if `x` is an [elic_cont] or an [elic_cat] object.
#'
#' @param x the object to be checked.
#' @param type character, either _cont_ or _cat_.
#' @param metadata logical, whether x should be a metadata or a loaded metadata.
#' @param round_topic the round or topic being investigated.

#'
#' @return An error if `x` is not and [elic_cont]or an [elic_cat] object.
#' @noRd
#'
#' @author Sergio Vignali
check_elic_obj <- function(x,
                           type,
                           metadata) {

  cl <- paste0("elic_", type)

  if (!inherits(x, cl)) {
    fn <- as.list(sys.call(-1))[[1]]

    fn_start <- paste0(type, "_start")
    fn_add_data <- paste0(type, "_add_data")

    if (metadata) {
      error <- "{.help elicitr::{cl}} object created with
                {.fn elicitr::{fn_start}}."

    } else {
      error <- "{.help elicitr::{cl}} object created with
                {.fn elicitr::{fn_start}} and with data added with \\
                {.fn elicitr::{fn_add_data}}."
    }

    error <- paste0("Argument {.arg x} must be a ", error)
    error2 <- "{.arg x} should be of class {.cls {cl}} and not of class \\
               {.cls {class(x)}}."

    cli::cli_abort(c("Invalid value for {.arg x}:",
                     "x" = error,
                     " " = error2,
                     "i" = "See {.fn elicitr::{fn}}."),
                   call = rlang::caller_env())
  }
}

#' Check elicit empty
#'
#' Check if `x` is an empty [elic_cont] or an [elic_cat] object.
#'
#' @param x the object to be checked.
#' @param type character, either _cont_ or _cat_.
#' @param round_topic the round or topic being investigated
#'
#' @return An error if `x` is an empty [elic_cont]or an [elic_cat] object.
#' @noRd
#'
#' @author Maude Vernet
check_empty_obj <- function(x,
                            type,
                            round_topic) {

  if (is.null(x[["data"]][[round_topic]])) {

    fn <- as.list(sys.call(-1))[[1]]
    fn_start <- paste0(type, "_start")
    fn_add_data <- paste0(type, "_add_data")
    cl <- paste0("elic_", type)

    error <- "{.help elicitr::{cl}} object created with
              {.fn elicitr::{fn_start}} and with data added with \\
              {.fn elicitr::{fn_add_data}}."

    error <- paste0("Argument {.arg x} must be a ", error)

    cli::cli_abort(c("Invalid value for {.arg x}:",
                     "x" = error,
                     "i" = "See {.fn elicitr::{fn}}."),
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
#' @author Sergio Vignali and Maude Vernet
check_round <- function(x) {
  if (length(x) > 1 || !x %in% c(1, 2)) {

    fn <- as.list(sys.call(-1))[[1]]

    cli::cli_abort(c("Incorrect value for {.arg round}:",
                     "x" = "Argument {.arg round} can only be equal to \\
                            {.val {1}} (for {.val Round 1}) or to {.val {2}} \\
                            (for {.val Round 2}).",
                     "i" = "See {.fn elicitr::{fn}}."),
                   call = rlang::caller_env())

  }
}

#' Check argument length
#'
#' `check_arg_length()` throws an error if the length of the provided argument
#' is greater than 1.
#'
#' @param x value passed to [cont_start] for the variable or elicitation type.
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
                   var = "Variable types",
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
                   var = "Variable types",
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

  if (nzchar(error, keepNA = TRUE)) {

    fn <- as.list(sys.call(-1))[[1]]

    cli::cli_abort(c("Incorrect value for {.arg experts}:",
                     "x" = error,
                     "i" = "See {.fn elicitr::{fn}}."),
                   call = rlang::caller_env())
  }
}

#' Check length
#'
#' @param x The object to be checked.
#' @param arg The name of the function argument.
#' @param length The expected length of the object.
#'
#' @return An error if the length of `x` is not equal to `length`.
#' @noRd
#'
#' @author Sergio Vignali
check_length <- function(x,
                         arg,
                         length) {

  if (length(x) != length) {

    fn <- as.list(sys.call(-1))[[1]]

    error <- "Argument {.arg {arg}} must have length {.val {length}} not \\
              {.val {length(x)}}."

    cli::cli_abort(c("Incorrect value for {.arg {arg}}:",
                     "x" = error,
                     "i" = "See {.fn elicitr::{fn}}."),
                   call = rlang::caller_env())
  }
}

#' Check columns
#'
#' Check whether the number of columns correspond to those expected.
#'
#' @param x data.frame or tibble with the imported data.
#' @param col_names character vector with the expected column names.
#' @param type the type of elicitation (continuous or categorical).
#'
#' @return An error if the number of columns is not as expected.
#' @noRd
#'
#' @author Sergio Vignali
check_columns <- function(x, y, type) {
  # Check number of columns
  if (ncol(x) != y) {

    fn <- as.list(sys.call(-1))[[1]]

    error <- "The imported dataset has {.val {ncol(x)}} column{?s} but \\
              {.val {y}} are expected."
    if (type == "cont") {
      info <- "The first column in the data should be the expert name, \\
      followed by the elicited variables as:"
      info2 <- "One-point elicitation variables should each have 1 column, \\
      three-point elicitation variables should each have 2 columns and \\
      four-point elicitation variables should each have 4 columns."
    } else if (type == "cat") {
      info <-  "The columns in the data should be:"
      info2 <- "(1) the expert names, (2) the options, (3) the categories, \\
      (4) the confidence and (5) the estimates."
    }

    info3 <- "See Data format in {.fn elicitr::{fn}}."

    cli::cli_abort(c("Unexpected number of columns:",
                     "x" = error,
                     " " = info,
                     "y" = info2,
                     "i" = info3),
                   call = rlang::caller_env())
  }
}

#' Check columns type
#'
#' Check if the columns are of the expected type.
#'
#' @param x data.frame or tibble with the imported data.
#' @param y character with the expected types (maximum 2 types).
#'
#' @return An error if the columns are not of the expected type.
#' @noRd
#'
#' @author Sergio Vignali
check_columns_type <- function(x, y) {

  col_types <- sapply(x, class)
  idx <- which(!col_types %in% y)

  if (any(idx)) {

    fn <- as.list(sys.call(-1))[[1]]
    cols <- colnames(x)[idx]

    error <- "The {cli::qty(cols)} column{?s} {.val {cols}} {?is/are} not of \\
              type {.val {y[[1]]}}"

    if (length(y) > 1) {
      error <- paste(error, "or {.val {y[[2]]}} but of type \\
                             {.val {unique(col_types[idx])}}.")
    } else {
      error <- paste(error, "but of type {.val {unique(col_types[idx])}}.")
    }

    info <- "See Data format in {.fn elicitr::{fn}}."

    cli::cli_abort(c("Unexpected column {cli::qty(cols)} type{?s}:",
                     "x" = error,
                     "i" = info),
                   call = rlang::caller_env())

  }
}

#' Check if value is in the list element.
#'
#' @param x [elic_cont] object.
#' @param element character string with the name of the element to be checked.
#' @param value character with the value/s to be checked.
#'
#' @return An error if `value` is not among the available values of list
#' element.
#' @noRd
#'
#' @author Sergio Vignali
check_value_in_element <- function(x,
                                   element,
                                   value) {

  if (element == "topic") {
    values <- names(x[["data"]])
  } else {
    values <- x[[element]]
  }

  info_element <- gsub("_", " ", element, fixed = TRUE)

  if (!endsWith(info_element, "s")) {
    info_element <- paste0(info_element, "s")
  }

  diff <- setdiff(value, values)

  if (length(diff) > 0) {

    fn <- as.list(sys.call(-1))[[1]]

    error <- "{.val {diff}} not present in the {.cls {class(x)}} object."
    info <- "Available {info_element}: {.val {values}}."
    cli::cli_abort(c("Invalid value for {.arg {element}}:",
                     "x" = error,
                     "i" = info,
                     " " = "See {.fn elicitr::{fn}} for more information."),
                   call = rlang::caller_env())
  }
}

#' Check method
#'
#' Check if the aggregation method is available for the data type.
#'
#' @param x [elic_cont] or [elic_cat] object.
#' @param method character with the aggregation method.
#'
#' @return An error if the method is not available for the data type.
#' @noRd
#'
#' @author Sergio Vignali
check_method <- function(x, method) {

  fn <- as.list(sys.call(-1))[[1]]


  if (inherits(x, "elic_cat")) {
    methods <- c("unweighted", "weighted")
    data_type <- "categorical"
  } else {
    methods <- "PERT"
    data_type <- "continuous"
  }

  if (!method %in% methods) {
    cli::cli_abort(c("Invalid value for {.arg method}:",
                     "x" = "The method {.val {method}} is not available for \\
                            {data_type} data.",
                     "i" = "See Methods in {.fn elicitr::{fn}}."),
                   call = rlang::caller_env())
  }
}

#' Check option
#'
#' Check if the option is available in the sampled data.
#'
#' @param x [tibble][tibble::tibble] with the sampled data.
#' @param option character string with the option to be checked.
#'
#' @returns An error if the option is not available in the sampled data.
#' @noRd
#'
#' @author Sergio Vignali
check_option <- function(x, option) {

  available_options <- c(unique(x[["option"]]), "all")
  diff <- setdiff(option, available_options)

  if (length(diff) > 0) {
    fn <- as.list(sys.call(-1))[[1]]
    error <- "{cli::qty(diff)} Option{?s} {.val {diff}} not available in \\
               the sampled data."
    info <- "See {.fn elicitr::{fn}} for more information."
    cli::cli_abort(c("Invalid value for argument {.arg option}:",
                     "x" = error,
                     " " = "{cli::qty(available_options)} Available \\
                     option{?s}: {.val {available_options}}.",
                     "i" = info),
                   call = rlang::caller_env())
  }
}

#' Check weights
#'
#' Check if the weights are either 1 or a vector of length `n`.
#'
#' @param x numeric with the weights.
#' @param n numeric with the number of experts.
#'
#' @returns An error if the weights are not 1 or a vector of length `n`.
#' @noRd
#'
#' @author Sergio Vignali and Maude Vernet
check_weights <- function(x, n) {


  if ((length(x) != n)) {

    fn <- as.list(sys.call(-1))[[1]]

    error <- "Argument {.arg weights} must be {.val {1}} or a vector of \\
              length {.val {n}}, same as the number of experts."

    cli::cli_abort(c("Invalid value for argument {.arg weights:}",
                     "x" = error,
                     "i" = "See {.fn elicitr::{fn}} for more information."),
                   call = rlang::caller_env())
  }

  if (anyNA(x)) {
    fn <- as.list(sys.call(-1))[[1]]

    error <- "Weights cannot be {.val NA}. Set weights to default (NULL) to \\
              avoid using this argument."

    cli::cli_abort(c("Invalid value for argument {.arg weights:}",
                     "x" = error,
                     "i" = "See {.fn elicitr::{fn}} for more information."),
                   call = rlang::caller_env())
  }

  if (all(x == 0)) {
    fn <- as.list(sys.call(-1))[[1]]

    error <- "Argument {.arg weights} cannot be 0 for all experts."

    cli::cli_abort(c("Invalid value for argument {.arg weights:}",
                     "x" = error,
                     "i" = "See {.fn elicitr::{fn}} for more information."),
                   call = rlang::caller_env())
  }

  if (any(x < 0)) {
    fn <- as.list(sys.call(-1))[[1]]

    error <- "Weights cannot be negative."

    cli::cli_abort(c("Invalid value for argument {.arg weights:}",
                     "x" = error,
                     "i" = "See {.fn elicitr::{fn}} for more information."),
                   call = rlang::caller_env())
  }
}

#' Check if the variable is available in the sampled data
#'
#' @param x [`tibble`][tibble::tibble] with the sampled data.
#' @param var character vector with the name of the variable to be checked.
#'
#' @returns An error if the variable is not available in the sampled data.
#' @noRd
#'
#' @author Sergio Vignali, Maude Vernet
check_var_in_sample <- function(x, var) {

  vars <- unique(x[["var"]])

  if (!all(var %in% vars)) {

    diff <- setdiff(var, vars)

    fn <- as.list(sys.call(-1))[[1]]
    error <- "Variable{?s} {.val {diff}} {?is/are} not available in the \\
              sampled data."
    cli::cli_abort(c("Invalid value for argument {.arg var}:",
                     "x" = error,
                     "i" = "Available variable{?s} {?is/are} {.val {vars}}.",
                     " " = "See {.fn elicitr::{fn}} for more information."),
                   call = rlang::caller_env())
  }
}

# check_n_votes----

#' check_n_votes
#'
#' Verify that n_voets is correctly formed.
#'
#' @return an error with n_votes is wrong.
#' @noRd
#'
#' @author Maude Vernet
check_n_votes <- function(n_votes) {
  if (anyNA(n_votes) || length(n_votes) != 1 || any(n_votes < 0)) {

    fn <- as.list(sys.call(-1))[[1]]

    error <- "Argument {.arg n_votes} must be a single positive integer."

    cli::cli_abort(c("Invalid value for {.arg n_votes}:",
                     "x" = error,
                     "y" = "See {.fn elicitr::{fn}}."),
                   call = rlang::caller_env())
  }
}

#' Check plot type
#'
#' Check the imputed type and change it if needed.
#' @param type a character string with the type of plot
#' @param elic a character string with the elicitaton class
#' @noRd
#'
#' @author Maude Vernet
check_plot_type <- function(type, elic) {
  if (elic == "cont") {
    allowed_types <- c("violin", "beeswarm", "density")
  } else if (elic == "cat") {
    allowed_types <- c("violin", "beeswarm")
  }

  if (!type %in% allowed_types) {
    info <- "Available types are {.val {allowed_types}}."
    fn <- as.list(sys.call(-1))[[1]]
    cli::cli_abort(c("Invalid value for argument {.arg type}:",
                     "x" = "Type {.val {type}} is not implemented.",
                     "i" = info,
                     " " = "See {.fn elicitr::{fn}} for more information."),
                   call = rlang::caller_env())
  }
}
