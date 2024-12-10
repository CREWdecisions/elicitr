# Checkers----

#' Check elicit
#'
#' Check if `x` is an [elic_cont] or an [elic_cat] object.
#'
#' @param x the object to be checked.
#' @param type character, either _cont_ or _cat_.
#'
#' @return An error if `x` is not and [elic_cont]or an [elic_cat] object.
#' @noRd
#'
#' @author Sergio Vignali
check_elic_obj <- function(x,
                           type) {

  cl <- paste0("elic_", type)

  if (!inherits(x, cl)) {

    fn <- as.list(sys.call(-1))[[1]]

    error <- "Argument {.arg x} must be an object of class {.cls {cl}} and \\
              not of class {.cls {class(x)}}."

    cli::cli_abort(c("Invalid value for {.arg x}:",
                     "x" = error,
                     "y" = "See {.fn elicitr::{fn}}."),
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
#' @author Sergio Vignali
check_round <- function(x) {

  if (x > 2 || x <= 0) {

    fn <- as.list(sys.call(-1))[[1]]

    cli::cli_abort(c("Incorrect value for {.arg round}:",
                     "x" = "{.arg round} can only be {.val {1}} or {.val {2}}.",
                     "i" = "See {.fn elicitr::{fn}}."),
                   call = rlang::caller_env())
  }
}

#' Check argument length
#'
#' `check_arg_length()` throws an error if the length of the provided argument
#' is greater than 1.
#'
#' @param x value passed to `elic_cont_start()` for the variable or elicitation
#' type.
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
                   var = "Variable Types",
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
                   var = "Variable Types",
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

  if (nchar(error) > 0) {

    fn <- as.list(sys.call(-1))[[1]]

    cli::cli_abort(c("Incorrect value for {.arg experts}:",
                     "x" = error,
                     "y" = "See {.fn elicitr::{fn}}."),
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

# Helpers----

#' Read data
#'
#' `read_data()` reads data from a data frame, a file or a Google Sheets file
#' and returns a tibble.
#'
#' @param data_source data frame, character string with the path to the file or
#' Google Sheets file.
#' @param sep character used as field separator.
#' @param sheet integer or character to select the sheet.
#'
#' @return A tibble with the data.
#' @noRd
#'
#' @author Sergio Vignali
read_data <- function(data_source,
                      sep,
                      sheet) {

  if (inherits(data_source, "data.frame")) {

    assign("src", "data.frame", envir = rlang::caller_env())

    # Make sure is a tibble
    data <- data_source |>
      tibble::as_tibble()

  } else if (inherits(data_source, "character")) {
    # When `data_source` contains the file extension at the end of the string,
    # it is assumed to be a file
    ext <- tools::file_ext(data_source)

    if (nzchar(ext)) {

      data <- read_file(data_source,
                        ext = ext,
                        sheet = sheet,
                        sep = sep)

    } else {
      # Assume that `data_source` is a valid Google Sheets file. Otherwise, the
      # error is handled by the read_sheet() function (this doesn't need to be
      # tested).
      assign("src", "Google Sheets", envir = rlang::caller_env())
      data <- googlesheets4::read_sheet(data_source, sheet = sheet) |>
        suppressMessages() |>
        clean_gs_data()
    }
  }

  data
}

#' Read file
#'
#' `read_file()` reads data from a file and returns a tibble.
#'
#' @param data_source character string with the path to the file.
#' @param ext character string with the file extension.
#' @param sheet integer or character to select the sheet.
#' @param sep character used as field separator.
#'
#' @return A tibble with the data or an error if the file doesn't exist or the
#' extension is not supported.
#' @noRd
#'
#' @author Sergio Vignali
read_file <- function(data_source,
                      ext,
                      sheet,
                      sep) {

  # Check if file exists
  if (!file.exists(data_source)) {
    cli::cli_abort(c("x" = "File {.file {data_source}} doesn't exist!"),
                   call = rlang::caller_env(n = 2))
  }

  # Load data
  if (ext == "csv") {

    assign("src", "csv file", envir = rlang::caller_env(n = 2))
    data <- utils::read.csv(data_source, sep = sep) |>
      tibble::as_tibble()

  } else if (ext == "xlsx") {

    assign("src", "xlsx file", envir = rlang::caller_env(n = 2))
    data <- openxlsx::read.xlsx(data_source, sheet = sheet)

  } else {
    error <- "The extension of the provided file is {.val .{ext}}, supported \\
              are {.val .csv} or {.val .xlsx}."

    cli::cli_abort(c("Unsupported file extension:",
                     "x" = error,
                     "i" = "See {.fn elicitr::elic_cont_add_data}."),
                   call = rlang::caller_env(n = 2))
  }

  data
}

#' Split short codes
#'
#' `split_short_codes()` converts the string containing short codes to a
#' character vector with elements corresponding to each short code.
#'
#' @param x character containing the short codes.
#' @param add_p logical, whether to add the "p" character to each short code.
#'
#' @return A character vector with the short codes.
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
