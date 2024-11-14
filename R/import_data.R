#' Import data
#'
#' `r lifecycle::badge("experimental")` `load_data()` imports data from
#' different sources and format them for further analysis.
#'
#' @param x Either a [`data.frame`][base::data.frame], a string with the path to
#' a `csv` or `xlmx` file, or anything accepted by the
#' [read_sheet()][googlesheets4::read_sheet] function.
#' @param var_names character vector with the name of the estimated variables,
#' used only when `x` is a Google Sheets file.
#' @param var_types character vector with short codes indicating the variable
#' type, used only when `x` is a Google Sheets file. If only one `var_type` is
#' provided, its value is recycled for all variables. See Variable Types for
#' more.
#' @param elic_types character vector with short codes indicating the
#' elicitation type, used only when `x` is a Google Sheets file. If only one
#' `elic_type` is provided, its value is recycled for all variables. See Data
#' Format and Elicitation Types for more.
#' @param ... Unused arguments, included only for future extensions of the
#' function.
#' @param sep Character used as field separator, used only when `x` is a `csv`
#' file.
#'
#' @section Data Format:
#'
#' Data are expected to have a unique identifier always as first column and
#' named `id` lower case. After the unique identifier it is expected one or more
#' blocks which follow the specifications below:
#'
#' One point elicitation:
#'
#' * `var_best`: best estimate for the variable
#'
#' Three points elicitation:
#'
#' * `var_min`: minimum estimate for the variable
#' * `var_max`: maximum estimate for the variable
#' * `var_best`: best estimate for the variable
#'
#' Four points elicitation:
#'
#' * `var_min`: minimum estimate for the variable
#' * `var_max`: maximum estimate for the variable
#' * `var_best`: best estimate for the variable
#' * `var_conf`: confidence for the estimate
#'
#' Column `id` is unique, the other columns are a block and can be repeated for
#' each variable.
#'
#' @section Variable Types:
#'
#' Variable types must be provided in a single string containing short codes,
#' e.g. "pPN".
#'
#' Valid short codes are:
#'
#' * `Z`: _integers_, when the estimate must be an integer number in the
#' interval (-Inf, Inf).
#'
#' * `N` for _positive integers_, when the estimate must be an integer number in
#' the interval (0, Inf).
#'
#' * `n` for _negative integers_, when the estimate must be an integer number in
#' the interval (-Inf, 0].
#'
#' * `R` for _reals_, when the estimate must be a real number in the interval
#' (-Inf, Inf).
#'
#' * `s` for _positive reals_, when the estimate must be a real number in the
#' interval (0, Inf).
#'
#' * `r` for _negative reals_, when the estimate must be a real number in the
#' interval (-Inf, ].
#'
#' * `p`: _probability_, when the estimate must be a real number in the interval
#' (0, 1).
#'
#' @section Elicitation types:
#'
#' Elicitation types must be provided in a single string containing short codes,
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
#' @return A [`tibble`][tibble::tibble] containing...
#' @export
#'
#' @author Sergio Vignali
#'
#' @examples
import_data <- function(x,
                        var_names,
                        var_types,
                        elic_types,
                        ...,
                        sep = ",") {

  n_vars <- length(var_names)

  # Split variable types
  var_types <- stringr::str_split_1(var_types,
                                    pattern = "")

  # Split elicitation types and add a "p" character
  elic_types <- stringr::str_split_1(elic_types,
                                     pattern = "") |>
    paste0("p")

  # Check variable types
  check_arg_types(var_types,
                  type = "var_types")

  # Check elicitation types
  check_arg_types(elic_types,
                  type = "elic_types")

  # Recycle variable and elicitation types if necessary
  if (n_vars > 1 & length(var_types) == 1) {
    var_types <- rep(var_types, n_vars)
  }

  if (n_vars > 1 & length(elic_types) == 1) {
    elic_types <- rep(elic_types, n_vars)
  }

  # Check arguments for compatibility
  check_arg_comp(var_names,
                 var_types,
                 elic_types)

  # Get column names
  col_names <- get_col_names(var_names,
                             elic_types)

  if (inherits(x, "data.frame")) {
    check_columns(x,
                  col_names)
    data <- x |>
      tibble::as_tibble()
  } else if (inherits(x, "character")) {
    # When `x` contains the file extension at the end of the string, it is
    # assumed to be a file
    ext <- tools::file_ext(x)
    if (nzchar(ext)) {

      # Check if file exists
      if (!file.exists(x)) {
        cli::cli_abort(c("x" = "File {.file {x}} doesn't exist!"))
      }

      # Check file extension
      if (ext == "csv") {
        cli::cli_alert_success("Function arguments are correct")
        data <- utils::read.csv(x, sep = sep) |>
          tibble::as_tibble()
        cli::cli_alert_success("Data imported from {.field csv} file")
      } else if (ext == "xlsx") {
        cli::cli_alert_success("Function arguments are correct")
        data <- openxlsx::read.xlsx(x, sheet = 1)
        cli::cli_alert_success("Data imported from {.field xlsx} file")
      } else {
        cli::cli_abort(c("File extension must be {.field .csv} or {.field .xlsx}",
                         "i" = "See {.fun elicitr::import_data}.",
                         "x" = "Your file extension is {.field .{ext}} instead"))
      }

      # Check number of columns and their name
      check_columns(data,
                    col_names)
    } else {
      # Assume that `x` is a valid Google Sheets file. Otherwise, the error is
      # handled by the read_sheet() function (this doesn't need to be tested).
      # Download data
      cli::cli_alert_success("Function arguments are correct")

      data <- googlesheets4::read_sheet(x) |>
        suppressMessages() |>
        # Remove timestamp
        dplyr::select(-1)

      # Check number of columns
      check_columns(data,
                    col_names,
                    full = FALSE)

      names(data) <- col_names

      cli::cli_alert_success("Data imported from {.field Google Sheets}")
      # # Remove capital letters
      #
      # # Order names
      # dplyr::arrange(2) |>
      # # Remove columns with timestamp and name (expected to be the first 2 ones)
      # dplyr::select(-c(1, 2)) |>
      # # Create a unique identifier
      # dplyr::mutate(id = dplyr::row_number(), .before = 1)
    }
  }



  #
  # # There can be only 4 columns per variable plus a unique identifier
  # nc <- ncol(data)
  # if ((nc - 1) %% 4 != 0) {
  #   cli::cli_abort(c("The dataset contains a wrong number of columns:",
  #                    "i" = "See {.emph Details} in {.fun elicitr::import_data}",
  #                    "x" = "Your dataset has {.strong {nc}} columns."))
  # }
  #
  return(data)
}

#' Check arguments compatibility
#'
#' Check whether `var_names`, `var_types`, and `est_type` have compatible
#' values, accounting that values for `var_types` and `est_type` can be recycled
#' when have length 1.
#'
#' @inheritParams import_data
#'
#' @keywords Internal
#' @noRd
#'
#' @author Sergio Vignali
check_arg_comp <- function(var_names,
                           var_types,
                           elic_types) {

  n_vars <- length(var_names)
  n_var_types <- length(var_types)
  n_elic_types <- length(elic_types)

  head_error <- "You provided {.field {n_vars}} value{?s} for {.var var_names}"
  var_error <- ""
  est_error <- ""
  raise_error <- TRUE

  if (n_var_types != n_vars) {
    var_error <- "{.field {n_var_types}} short code{?s} for {.var var_types}"
  }

  if (n_elic_types != n_vars) {
    est_error <- "{.field {n_elic_types}} short code{?s} for {.var elic_types}"
  }

  if (nzchar(var_error) && nzchar(est_error)) {
    error <- paste0(head_error, ", ", var_error, ", and ", est_error)
  } else if (nzchar(var_error)) {
    error <- paste0(head_error, " and ", var_error)
  } else if (nzchar(est_error)) {
    error <- paste0(head_error, " and ", est_error)
  } else {
    raise_error <- FALSE
  }

  if (raise_error) {
    cli::cli_abort(c("Mismatch between function arguments:",
                     "i" = "See {.fun elicitr::import_data}.",
                     "x" = error),
                   call = rlang::caller_env())
  }
}

#' Check argument types
#'
#' Check whether variable or elicitation types are allowed.
#'
#' @param type character, either `var_types` or `elic_types`.
#' @inheritParams import_data
#'
#' @keywords Internal
#' @noRd
#'
#' @author Sergio Vignali
check_arg_types <- function(x, type) {

  if (type == "elic_types") {
    # Check allowed elicitation types
    diff <- setdiff(x, names(var_labels))
  } else {
    # Check allowed variable types
    diff <- setdiff(x, c("Z", "N", "n", "R", "s", "r", "p"))
  }

  if (length(diff) > 0) {
    diff <- gsub(pattern = "p",
                 replacement = "",
                 x = diff)
    error <- "The incorrect short code{?s} {?is/are} {.field {diff}}."
    cli::cli_abort(c("Incorrect value for {.arg {type}}:",
                     "i" = "See {.fun elicitr::import_data}.",
                     "x" = error),
                   call = rlang::caller_env())
  }
}

check_columns <- function(x,
                          col_names,
                          full = TRUE) {
  # Check number of columns
  if (ncol(x) != length(col_names)) {
    text <- "Unexpected number of columns:"
    error <- "The imported dataset has {.field {ncol(x)}} columns but are \\
              expected to be {.field {length(col_names)}}."
  # Check column names
  } else if (!all(names(x) == col_names) && full) {
    text <- "Incorrect column names:"
    error <- "The imported dataset has {.field {names(x)}} but it is expected \\
              {.field {col_names}}."
  } else {
    cli::cli_alert_success("The number and name of columns are correct")
  }

  if (exists("error")) {
    cli::cli_abort(c(text,
                     "i" = "See Data Format in {.fun elicitr::import_data}.",
                     "x" = error),
                   call = rlang::caller_env())
  }
}

#' Get labels
#'
#' Get label to build column names.
#'
#' @param n integer, number of variables.
#' @inheritParams import_data
#'
#' @return character vector with the labels
#' @keywords Internal
#' @noRd
#'
#' @author Sergio Vignali
get_labels <- function(n,
                       elic_types) {

  if (n > 1L & length(elic_types) == 1L) {
    # Recycle variable type
    elic_types <- rep(elic_types, n)
  }

  # Labels are stored on the `var_labels` data object
  raw_labels <- var_labels[elic_types] |>
    unlist(use.names = FALSE)

  return(raw_labels)
}

#' Get column names
#'
#' `get_col_names()` combines the information provided with `var_names` and
#' `elic_types` to construct the column names.
#'
#' @inheritParams import_data
#'
#' @return character vector with the column names.
#' @keywords Internal
#' @noRd
#'
#' @author Sergio Vignali
get_col_names <- function(var_names,
                          elic_types) {
  n <- length(var_names)
  r <- gsub("p", "", elic_types) |>
    as.integer()
  labels <- get_labels(n = n,
                       elic_types = elic_types)

  var_columns <- rep(var_names, r) |>
    paste0("_", labels)

  c("id", var_columns)
}
