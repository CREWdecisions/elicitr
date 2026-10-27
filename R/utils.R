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

    fn <- as.list(sys.call(-2))[[1]]
    cli::cli_abort(c("x" = "File {.file {data_source}} doesn't exist!",
                     "i" = "See {.fn elicitr::{fn}} for more information."),
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
#' @param weights numeric vector with the weight values.
#' @param elic_type the type of elicitation.
#' @param data the data to be sampled.
#'
#' @returns A vector with the number of samples to take for each expert.
#' @noRd
#'
#' @author Sergio Vignali and Maude Vernet
get_boostrap_n_sample <- function(experts,
                                  n_votes,
                                  weights,
                                  elic_type = "other",
                                  data = NULL) {
  if (elic_type == "4p") {
    position <- which(is.na(data[, 5, drop = TRUE]))
    if (is.null(weights)) {
      weights_conf <- data[, 5, drop = TRUE] / 100
    } else {
      weights[position] <- NA
      weights_conf <- weights
    }
  } else if (elic_type %in% c("1p", "3p")) {
    position <- which(is.na(data[, 2]))
    if (!is.null(weights)) {
      weights[position] <- NA
      weights_conf <- weights
    } else {
      weights_fill <- rep(1, length(experts))
      weights_fill[position] <- NA
      weights_conf <- weights_fill
    }
  } else if (elic_type == "weighted") {
    weights_conf <- weights
  } else if (elic_type %in% c("unweighted", "other")) {
    weights_conf <- weights
  }

  # if some or all experts don't answer an option
  position <- NULL
  if (anyNA(weights_conf)) {
    position <- which(is.na(weights_conf))
    weights_conf[is.na(weights_conf)] <- 0
  }

  if (sum(weights_conf, na.rm = TRUE) != 0) {
    n_samp_pre <- (length(experts) * n_votes * weights_conf / sum(weights_conf))
    n_samp <- floor(n_samp_pre)
    n_extra <- as.integer(round(sum(n_samp_pre)) - sum(n_samp))

    if (n_extra > 0) {
      #find places that lost the largest fractional part by rounding down
      indices <- order(n_samp_pre - n_samp, decreasing = TRUE)[seq_len(n_extra)]
      #add 1 to those
      n_samp[indices] <- n_samp[indices] + 1
    }
  } else {
    n_samp <- rep(1, length(experts))
  }

  if (elic_type == "unweighted") {
    n_samp <- rep(n_votes, length(experts))
  }

  if (!is.null(position)) {
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
