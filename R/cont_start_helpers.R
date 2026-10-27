# Checkers----

#' Check arguments compatibility
#'
#' Check whether `var_names`, `var_types`, and `elic_types` have compatible
#' values, accounting that values for `var_types` and `elic_type` can be
#' recycled when have length 1.
#'
#' @param var_names character vector with the name of the estimated variables.
#' @param var_types character with short codes indicating the variable type.
#' @param elic_types character with short codes indicating the elicitation type.
#'
#' @return An error if `var_names`, `var_types` and `elic_types` have
#' incompatible values or length.
#'
#' @noRd
#'
#' @author Sergio Vignali
check_arg_mism <- function(var_names,
                           var_types,
                           elic_types) {

  n_vars <- length(var_names)
  n_var_types <- length(var_types)
  n_elic_types <- length(elic_types)

  head_error <- "The number of short codes in"
  tail_error <- "should be either {.val {1}} or equal to the number of \\
                 elements in {.arg var_names}."
  var_error <- ""
  est_error <- ""
  raise_error <- TRUE

  if (n_var_types != n_vars) {
    var_error <- "{.arg var_types}"
  }

  if (n_elic_types != n_vars) {
    est_error <- "{.arg elic_types}"
  }

  if (nzchar(var_error) && nzchar(est_error)) {
    error <- paste(head_error, var_error, " and", est_error, tail_error)
  } else if (nzchar(var_error)) {
    error <- paste(head_error, var_error, tail_error)
  } else if (nzchar(est_error)) {
    error <- paste0(head_error, est_error, tail_error)
  } else {
    raise_error <- FALSE
  }

  if (raise_error) {
    cli::cli_abort(c("Mismatch between function arguments:",
                     "x" = error,
                     "i" = "See {.fn elicitr::cont_start}."),
                   call = rlang::caller_env())
  }
}
