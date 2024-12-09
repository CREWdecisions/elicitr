#' Start elicitation
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `elic_cont_start()` initialises an `elic_cont` object which stores important
#' metadata for the data collected during the elicitation process of continuous
#' variables.
#'
#' @param var_names character vector with the name of the estimated variables.
#' @param var_types character string with short codes indicating the variable
#' type. If only one `var_type` is provided, its value is recycled for all
#' variables. See Variable Types for more.
#' @param elic_types character string with short codes indicating the
#' elicitation type. If only one `elic_type` is provided, its value is recycled
#' for all variables. See Elicitation Types for more.
#' @param experts numeric indicating the number of experts participating in the
#' elicitation process.
#' @param ... Unused arguments, included only for future extensions of the
#' function.
#' @param title character, used to bind a name to the object.
#' @param verbose logical, if `TRUE` prints informative messages.
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
#' @return An object of class `elic_cont` binding metadata related to the
#' elicitation process. These metadata are used by other functions to validate
#' the correctness of the provided data.
#' @export
#'
#' @author Sergio Vignali
#'
#' @references Hemming, V., Burgman, M. A., Hanea, A. M., McBride, M. F., &
#' Wintle, B. C. (2018). A practical guide to structured expert elicitation
#' using the IDEA protocol. Methods in Ecology and Evolution, 9(1), 169–180.
#' <https://doi.org/10.1111/2041-210X.12857>
#'
#' @examples
#' # Create the elic_cont object for an elicitation process that estimates 3
#' # variables, the first for a one point estimation of a positive integer, the
#' # second for three points estimation of a negative real, and the last for a
#' # four point estimation of a probability
#' x <- elic_cont_start(var_names = c("var1", "var2", "var3"),
#'                      var_types = "Nrp",
#'                      elic_types = "134",
#'                      experts = 4)
#' x
#'
#' # A title can be added to bind a name to the object:
#' x <- elic_cont_start(var_names = c("var1", "var2", "var3"),
#'                      var_types = "Nrp",
#'                      elic_types = "134",
#'                      experts = 4,
#'                      title = "My elicitation")
#' x
#' # Notice that if var_types and elic_types are provided as single character,
#' # their value is recycled and applied to all variables. In the following
#' # example all three variables will be considered for a four point estimation
#' # to estimate a probability:
#' x <- elic_cont_start(var_names = c("var1", "var2", "var3"),
#'                      var_types = "p",
#'                      elic_types = "4",
#'                      experts = 4)
#' x
elic_cont_start <- function(var_names,
                            var_types,
                            elic_types,
                            experts,
                            ...,
                            title = "Elicitation",
                            verbose = TRUE) {

  # Check that variable and elicitation types are a single string
  check_arg_length(var_types, type = "var")
  check_arg_length(elic_types, type = "elic")

  # Check that the argument `experts` is a number
  check_experts_arg(experts)

  n_vars <- length(var_names)

  # Split variable and elicitation types
  var_types <- split_short_codes(var_types)
  elic_types <- split_short_codes(elic_types, add_p = TRUE)

  # Check variable and elicitation types
  check_arg_types(var_types, type = "var")
  check_arg_types(elic_types, type = "elic")

  # Recycle variable and elicitation types if necessary
  if (n_vars > 1 && length(var_types) == 1) {
    var_types <- rep(var_types, n_vars)
  }

  if (n_vars > 1 && length(elic_types) == 1) {
    elic_types <- rep(elic_types, n_vars)
  }

  # Check arguments for compatibility
  check_arg_mism(var_names,
                 var_types,
                 elic_types)

  obj <- new_elic_cont(var_names,
                       var_types,
                       elic_types,
                       experts = experts,
                       title)

  if (verbose) {
    cli::cli_alert_success("{.cls elic_cont} object for {.val {title}} \\
                            correctly initialised")
  }

  obj
}

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
                     "i" = "See {.fn elicitr::elic_cont_start}."),
                   call = rlang::caller_env())
  }
}
