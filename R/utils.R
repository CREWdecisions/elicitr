# Checkers----

#' Check argument length
#'
#' `check_arg_length()` throws an error if the length of the provided argument
#' is greater than 1.
#'
#' @param x value passed to `elic_start()` for the variable or elicitation type.
#' @param type character, either _var_ for `var_types` or _elic_ for èlic_types.
#' @noRd
#'
#' @author Sergio Vignali
check_arg_length <- function(x,
                             type) {

  n <- length(x)

  if (n > 1) {

    sect <- switch(type,
                   var = "Variable Types",
                   elic = "Elicitation types")

    error <- "The value provided for {.arg {type}_types} is a character \\
              vector of length {.val {n}} but should be a single string with \\
              short codes."

    cli::cli_abort(c("Incorrect value for {.arg {type}_types}:",
                     "x" = error,
                     "i" = "See {.str {sect}} in {.fn elicitr::elic_start}."),
                   call = rlang::caller_env())
  }
}

#' Check argument types
#'
#' `check_arg_types()` throws an error if the short codes are not allowed.
#'
#' @param x character containing short codes for variable or elicitation types.
#' @param type character, either _var_ for `var_types` or _elic_ for èlic_types.
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
                 x = diff)

    sect <- switch(type,
                   var = "Variable Types",
                   elic = "Elicitation types")

    error <- "The incorrect short code{?s} {?is/are} {.val {diff}}."

    cli::cli_abort(c("Incorrect value for {.arg {type}_types}:",
                     "x" = error,
                     "i" = "See {.str {sect}} in {.fn elicitr::elic_start}."),
                   call = rlang::caller_env())
  }
}

#' Check arguments compatibility
#'
#' Check whether `var_names`, `var_types`, and `elic_types` have compatible
#' values, accounting that values for `var_types` and `elic_type` can be
#' recycled when have length 1.
#'
#' @param var_names character vector with the name of the estimated variables.
#' @param var_types character with short codes indicating the variable type.
#' @param elic_types character with short codes indicating the elicitation type.
#' @noRd
#'
#' @author Sergio Vignali
check_arg_mism <- function(var_names,
                           var_types,
                           elic_types) {

  n_vars <- length(var_names)
  n_var_types <- length(var_types)
  n_elic_types <- length(elic_types)

  head_error <- "You provided {.val {n_vars}} value{?s} for {.var var_names}"
  var_error <- ""
  est_error <- ""
  raise_error <- TRUE

  if (n_var_types != n_vars) {
    var_error <- "{.val {n_var_types}} short code{?s} for {.var var_types}"
  }

  if (n_elic_types != n_vars) {
    est_error <- "{.val {n_elic_types}} short code{?s} for {.var elic_types}"
  }

  if (nzchar(var_error) && nzchar(est_error)) {
    error <- paste0(head_error, ", ", var_error, ", and ", est_error, ".")
  } else if (nzchar(var_error)) {
    error <- paste0(head_error, " and ", var_error, ".")
  } else if (nzchar(est_error)) {
    error <- paste0(head_error, " and ", est_error, ".")
  } else {
    raise_error <- FALSE
  }

  if (raise_error) {
    cli::cli_abort(c("Mismatch between function arguments:",
                     "x" = error,
                     "i" = "See {.fun elicitr::read_data}."),
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
