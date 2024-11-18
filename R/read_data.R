#' Read data
#'
#' `r lifecycle::badge("experimental")` `load_data()` reads data from
#' different sources and format them for further analysis.
#'
#' @param x either a [`data.frame`][base::data.frame], a string with the path to
#' a `csv` or `xlsx` file, or anything accepted by the
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
#' @param name character, used to bind a name to the dataset.
#' @param sep character used as field separator, used only when `x` is a `csv`
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
#' @return An object of class `elicitr`.
#' @export
#'
#' @author Sergio Vignali
#'
#' @examples
read_data <- function(x,
                      var_names,
                      var_types,
                      elic_types,
                      ...,
                      name = "Elicitation Dataset",
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

    cli::cli_alert_success("Function arguments are correct")
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
      check_file_extension(ext)
      cli::cli_alert_success("Function arguments are correct")

      # Load data
      if (ext == "csv") {
        data <- utils::read.csv(x, sep = sep) |>
          tibble::as_tibble()
        cli::cli_alert_success("Data imported from {.field csv} file")
      } else {
        data <- openxlsx::read.xlsx(x, sheet = 1)
        cli::cli_alert_success("Data imported from {.field xlsx} file")
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
        dplyr::select(-1) |>
        # Columns with mixed integer and real numbers are imported as list
        dplyr::mutate(dplyr::across(dplyr::where(is.list), as.character))

      # Check number of columns
      check_columns(data,
                    col_names,
                    full = FALSE)

      names(data) <- col_names

      to_hash <- Vectorize(digest::digest,
                           USE.NAMES = FALSE)

      # Replace internal whitespaces with a single space, leading and trailing
      # ones are removed by googlesheets4::read_sheet()
      data <- data |>
        dplyr::mutate("id" = tolower(.data$id),
                      "id" = stringr::str_squish(.data$id)) |>
        # Order by name
        dplyr::arrange("id") |>
        dplyr::mutate("id" = to_hash(.data$id,
                                     algo = "sha1",
                                     serialize = FALSE),
                      "id" = substr(.data$id,
                                    start = 1,
                                    stop = 7))

      cli::cli_alert_success("Data imported from {.field Google Sheets}")
    }
  }
  out <- list(var_names = var_names,
              var_types = var_types,
              elic_types = elic_types,
              name = name,
              data = list(round_1 = data,
                          round_2 = NULL))

  structure(out,
            class = "elicitr")
}

#' @export
print.elicitr <- function(x, ...) {

  rounds <- ifelse(is.null(x$data$round_2), 1, 2)

  cli::cli_h3(x$name)
  cli::cli_text()
  cli::cli_li("Variable{?s}: {.field {x$var_names}}")
  cli::cli_li("Variable type{?s}: {.field {x$var_types}}")
  cli::cli_li("Elicitation type{?s}: {.field {x$elic_types}}")
  cli::cli_li("Number of rounds: {.val {rounds}}")
  cli::cli_li("Data:")
  cli::cli_text()

  cli::style_underline("Round 1") |>
    cli::col_magenta() |>
    cli::cli_text()
  print(x$data$round_1)

  if (rounds == 2) {
    cli::style_underline("Round 2") |>
      cli::col_magenta() |>
      cli::cli_text()
    print(x$data$round_2)
  }

  invisible(x)
}

# Helper functions----

#' Get labels
#'
#' Get label to build column names.
#'
#' @param n integer, number of variables.
#' @inheritParams read_data
#'
#' @return character vector with the labels
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
#' @inheritParams read_data
#'
#' @return character vector with the column names.
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

# Functions for checking function arguments----

#' Check arguments compatibility
#'
#' Check whether `var_names`, `var_types`, and `est_type` have compatible
#' values, accounting that values for `var_types` and `est_type` can be recycled
#' when have length 1.
#'
#' @inheritParams read_data
#'
#' @noRd
#'
#' @author Sergio Vignali
check_arg_comp <- function(var_names,
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
                     "i" = "See {.fun elicitr::read_data}.",
                     "x" = error),
                   call = rlang::caller_env())
  }
}

#' Check argument types
#'
#' Check whether variable or elicitation types are allowed.
#'
#' @param type character, either `var_types` or `elic_types`.
#' @inheritParams read_data
#'
#' @noRd
#'
#' @author Sergio Vignali
check_arg_types <- function(x, type) {

  if (type == "elic_types") {
    # Check allowed elicitation types
    diff <- setdiff(x, names(var_labels))
  } else {
    # Check allowed variable types
    diff <- setdiff(x, c("Z", "N", "z", "R", "s", "r", "p"))
  }

  if (length(diff) > 0) {
    diff <- gsub(pattern = "p",
                 replacement = "",
                 x = diff)
    error <- "The incorrect short code{?s} {?is/are} {.field {diff}}."
    cli::cli_abort(c("Incorrect value for {.arg {type}}:",
                     "i" = "See {.fun elicitr::read_data}.",
                     "x" = error),
                   call = rlang::caller_env())
  }
}

#' Check file extension
#'
#' Check if the file extension is supported, i.e. _csv_ or _xlsx_.
#'
#' @param x character containing the file extension
#'
#' @noRd
#'
#' @author Sergio Vignali
check_file_extension <- function(x) {

  if (!x %in% c("csv", "xlsx")) {
    error <- "The file extension is {.field .{x}}, supported are \\
              {.field .csv} or {.field .xlsx}."

    cli::cli_abort(c("Unsupported file extension:",
                     "i" = "See {.fun elicitr::read_data}.",
                     "x" = error),
                   call = rlang::caller_env())
  }
}

# Function for checking data----

#' Check columns
#'
#' Check whether the number of columns and their names correspond to those
#' expected.
#'
#' @param x data.frame or tibble with the imported data.
#' @param col_names character vector with the expected column names.
#' @param full logical indicating whether to check both, number of columns and
#' their name, or only the number of columns.
#'
#' @noRd
#'
#' @author Sergio Vignali
check_columns <- function(x,
                          col_names,
                          full = TRUE) {
  # Check number of columns
  if (ncol(x) != length(col_names)) {
    text <- "Unexpected number of columns:"
    error <- "The imported dataset has {.val {ncol(x)}} column{?s} but are \\
              expected to be {.val {length(col_names)}}."
    # Check column names
  } else if (!all(names(x) == col_names) && full) {
    text <- "Incorrect column names:"
    error <- "The imported dataset has {.field {names(x)}} but it is expected \\
              {.field {col_names}}."
  } else {
    cli::cli_alert_success("The number and name of the columns are correct")
  }

  if (exists("error")) {
    cli::cli_abort(c(text,
                     "i" = "See Data Format in {.fun elicitr::read_data}.",
                     "x" = error),
                   call = rlang::caller_env())
  }
}
