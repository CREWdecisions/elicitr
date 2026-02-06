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
  if (length(x) > 1) {

    fn <- as.list(sys.call(-1))[[1]]

    cli::cli_abort(c("Incorrect value for {.arg round}:",
                     "x" = "{.arg round} can only be {.val {1}} or {.val {2}}.",
                     "i" = "See {.fn elicitr::{fn}}."),
                   call = rlang::caller_env())

  } else if (x > 2 || x <= 0) {

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
#'
#' @return An error if the number of columns is not as expected.
#' @noRd
#'
#' @author Sergio Vignali
check_columns <- function(x, y) {
  # Check number of columns
  if (ncol(x) != y) {

    fn <- as.list(sys.call(-1))[[1]]

    error <- "The imported dataset has {.val {ncol(x)}} column{?s} but \\
              {.val {y}} are expected."
    info <- "See Data format in {.fn elicitr::{fn}}."

    cli::cli_abort(c("Unexpected number of columns:",
                     "x" = error,
                     "i" = info),
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

    error <- "{.val {diff}} not present in the {.cls {class(x)}} object."
    info <- "Available {info_element}: {.val {values}}."
    cli::cli_abort(c("Invalid value for {.arg {element}}:",
                     "x" = error,
                     "i" = info),
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
    methods <- "basic"
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

  available_options <- unique(x[["option"]])
  diff <- setdiff(option, available_options)

  if (length(diff) > 0) {
    error <- "{cli::qty(diff)} Option{?s} {.val {diff}} not available in \\
               the sampled data."
    info <- "Available option{?s}: {.val {available_options}}."
    cli::cli_abort(c("Invalid value for argument {.arg option}:",
                     "x" = error,
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
#' @author Sergio Vignali
check_weights <- function(x, n) {


  if ((length(x) == 1 && x != 1) || (length(x) != 1 && length(x) != n)) {

    fn <- as.list(sys.call(-1))[[1]]

    error <- "Argument {.arg weights} must be {.val {1}} or a vector of \\
              length {.val {n}}, same as the number of experts."

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
#' @author Sergio Vignali
check_var_in_sample <- function(x, var) {

  vars <- unique(x[["var"]])

  if (!all(var %in% vars)) {

    diff <- setdiff(var, vars)

    error <- "Variable{?s} {.val {diff}} {?is/are} not available in the \\
              sampled data."
    cli::cli_abort(c("Invalid value for argument {.arg var}:",
                     "x" = error,
                     "i" = "Available variable{?s} {?is/are} {.val {vars}}."))
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
    data <- tibble::as_tibble(data_source)

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
    data <- openxlsx::read.xlsx(data_source, sheet = sheet) |>
      tibble::as_tibble()

  } else {

    fn <- as.list(sys.call(-2))[[1]]

    error <- "The extension of the provided file is {.val .{ext}}, supported \\
              are {.val .csv} or {.val .xlsx}."

    cli::cli_abort(c("Unsupported file extension:",
                     "x" = error,
                     "i" = "See {.fn elicitr::{fn}}."),
                   call = rlang::caller_env(n = 2))
  }

  data
}

#' Clean Google Sheets data
#'
#' `clean_gs_data()` performs some data cleaning for data coming from
#' _Google Sheets_.
#'
#' @param x data imported form _Google Sheets_
#'
#' @details
#' 1. Remove column with timestamp, if present
#' 2. Converts columns with lists to character
#' 3. Standardise decimal separators by replacing commas to periods
#' 4. Forces all column but the first to be numeric
#'
#' @return The cleaned data
#' @noRd
#'
#' @author Sergio Vignali
clean_gs_data <- function(x) {

  clean <- \(x) gsub(pattern = ",", replacement = "\\.", x = x) # nolint
  n_cols <- ncol(x)

  if (inherits(x[[1]], "POSIXct")) {
    col_2 <- colnames(x)[[2]]
    x <- x |>
      # Keep only last submission from each expert
      dplyr::slice_tail(n = 1, by = dplyr::all_of(col_2)) |>
      # Remove column with timestamp (it should be the first column)
      dplyr::select(-1)
  }

  cols <- 1

  # This is to avoid errors if the function is called directly within a test
  if (as.list(sys.call())[[2]][[1]] != "dplyr::mutate" &&
        as.list(sys.call(-2))[[1]] == "cat_add_data") {
    cols <- 1:3
  }

  x |>
    # Columns with mixed integer and real numbers are imported as list
    dplyr::mutate(dplyr::across(dplyr::where(is.list), as.character),
                  # Some experts use a comma as decimal separator
                  dplyr::across(dplyr::everything(), clean),
                  # If there is a mix of integer and doubles,or if there are
                  # different decimal separators, or if someone omit the leading
                  # zero on a decimal number, these columns are imported as
                  # character
                  dplyr::across(!dplyr::all_of(cols), as.numeric))
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
    output <- paste0(output,
                     "p")
  }

  output
}

#' Anonymise names
#'
#' Converts names to anonymous ids from the names of the experts.
#'
#' @param x [tibble][tibble::tibble] with the data collected during the
#' elicitation process.
#'
#' @returns A [tibble][tibble::tibble] with names converted to anonymous ids.
#' @noRd
#'
#' @author Sergio Vignali
anonimise_names <- function(x) {

  col_1 <- colnames(x)[[1]]

  x |>
    dplyr::rename("id" = dplyr::all_of(col_1)) |>
    # Standardise names: remove capital letters, whitespaces, and punctuation
    dplyr::mutate("id" = stand_names(.data[["id"]]),
                  "id" = hash_names(.data[["id"]]))
}

#' Standardise names
#'
#' `stand_names()` converts strings to lower case, removes all whitespaces, and
#' removes punctuation.
#'
#' @param x character vector with strings to be normalised.
#'
#' @return Character vector with normalised strings.
#' @noRd
#'
#' @author Sergio Vignali
stand_names <- function(x) {
  tolower(x) |>
    gsub(pattern = "(\\s|[[:punct:]])",
         replacement = "",
         x = _)
}

#' Hash names
#'
#' `hash_names()` converts names to short sha1 codes (7 characters), used to
#' create anonymous ids.
#'
#' @param x character vector with names.
#'
#' @return a vector with encoded names
#' @noRd
#'
#' @author Sergio Vignali
hash_names <- function(x) {

  to_hash <- Vectorize(digest::digest,
                       USE.NAMES = FALSE)

  to_hash(x,
          algo = "sha1",
          serialize = FALSE) |>
    substr(start = 1,
           stop = 7)
}

#' Get type
#'
#' Get variable or elicitation type for the given variable.
#'
#' @param x an object of class `elic_cont`.
#' @param var character string with the variable name.
#' @param type character string, either `var` or `elic`.
#'
#' @return A character string with the variable or elicitation type.
#' @noRd
#'
#' @author Sergio Vignali
get_type <- function(x, var, type) {
  x[[paste0(type, "_types")]][x[["var_names"]] == var]
}

#' Rescale data
#'
#' Rescale the min and max values of the data to the confidence value.
#'
#' @param x a data.frame with the elicitation data.
#' @param s numeric, the scale factor for the confidence interval.
#'
#' @return A data.frame with the rescaled min and max values.
#' @noRd
#'
#' @author Sergio Vignali and Stefano Canessa
rescale_data <- function(x, s = 100) {

  x[["min"]] <- x[["best"]] - (x[["best"]] - x[["min"]]) * s / x[["conf"]]
  x[["max"]] <- x[["best"]] + (x[["max"]] - x[["best"]]) * s / x[["conf"]]

  x
}

#' Get bootstrap number of samples
#'
#' @param experts character vector with the expert ids.
#' @param n_votes numeric indicating the number of votes to consider for each
#' expert.
#' @param conf numeric vector with the confidence values.
#'
#' @returns A vector with the number of samples to take for each expert.
#' @noRd
#'
#' @author Sergio Vignali
get_boostrap_n_sample <- function(experts, n_votes, conf) {

  if (anyNA(conf)) {
    position <- which(is.na(conf))
    conf[is.na(conf)] <- 1
  }

  n_samp <- (length(experts) * n_votes * conf / sum(conf)) |>
    miceadds::sumpreserving.rounding(digits = 0, preserve = TRUE)

  if (exists("position")) {
    n_samp[position] <- 1
  }

  n_samp
}

# Theme----

#' Elic theme
#'
#' Custom theme for elicitation plots.
#'
#' @return A [`theme`][`ggplot2::theme`] function.
#' @noRd
#'
#' @author Sergio Vignali and Maude Vernet
elic_theme <- function(family = "sans") {
  ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 16,
                                                      face = "bold",
                                                      hjust = 0.5,
                                                      family = family),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_text(size = 16,
                                                        colour = "black",
                                                        family = family),
                   axis.title.x = ggplot2::element_text(vjust = -1.2,
                                                        size = 16,
                                                        colour = "black",
                                                        family = family),
                   axis.text = ggplot2::element_text(size = 14,
                                                     family = family),
                   plot.margin = ggplot2::unit(c(5, 10, 5, 5), units = "mm"))
}
