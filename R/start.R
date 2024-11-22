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
#' @return An object of class `elicit` binding metadata related to the
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
#' # Create the elict object for a elicitation process that estimates 3
#' # variables, the first for a one point estimation of a positive integer, the
#' # second for three points estimation of a negative real, and the last for a
#' # four point estimation of a probability
#' x <- elic_start(var_names = c("var1", "var2", "var3"),
#'                 var_types = "Nrp",
#'                 elic_types = "134")
#' x
#'
#' # A title can be added to bind a name to the object:
#' x <- elic_start(var_names = c("var1", "var2", "var3"),
#'                 var_types = "Nrp",
#'                 elic_types = "134",
#'                 title = "My elicitation")
#' x
#' # Notice that if var_types and elic_types are provided as single character,
#' # their value is recycled and applyed to all variables. In the following
#' # example all thre variables will be considered for a four point estimation
#' # to estimate a probability:
#' x <- elic_start(var_names = c("var1", "var2", "var3"),
#'                 var_types = "p",
#'                 elic_types = "4")
#' x
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

  cli::cli_alert_success("{.code elicit} object correctly initialised")

  new_elicit(var_names,
             var_types,
             elic_types,
             title)
}

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

