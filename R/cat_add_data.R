#' Add data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `elic_cat_add_data()` adds data to an [elic_cat] object from different
#' sources.
#'
#' @param x an object of class [elic_cat].
#' @param mechanism character string that indicates the machanism to which the
#' data belongs.
#' @inheritParams elic_cont_add_data
#'
#' @section Data format:
#'
#' For each mechanism, data are expected to have five columns, built as follows:
#' * The first column of the data should hold the names of the experts. The name
#' of each expert should be repeated as many times as many times as the number
#' of impact levels and sites.
#' (i.e. each expert should appear \eqn{number\ of\ levels
#' \cdot number\ of\ sites} times).
#' * The second column should be the names of the levels of impact considered in
#' the elicitation. Each block of levels of impact should be repeated as many
#' times as the number of sites investigated/considered.
#' * The third column should hold the names of the sites considered in the
#' study. The name of each site should be repeated as many times as the number
#' of levels of impact considered.
#' * The fourth column should be the experts confidence in their own estimates
#' (given in percent). Experts should estimate how confident they are in their
#' estimates for each block of levels of impact and for each site. Therefore,
#' expert confidence estimates should be repeated as many times as the number of
#' levels of impact considered.
#' * The final column should be the estimates of each expert for each site and
#' impact level.
#'
#' The name of the columns is not important, `elic_cat_add_data()` will
#' overwrite them according to the following convention:
#' * The first column will be renamed `id`, the second column `level`, the third
#' column `site`, the fourth column `confidence`, and the fifth column
#' `estimate`.
#'
#' Here is an example of data correctly formatted for an elicitation with two
#' levels of impact and two sites (only one expert is shown):
#'
#' ```
#' name       level       site      confidence      estimate
#' ---------------------------------------------------------
#' expert 1   level 1     site 1            15          0.08
#' expert 1   level 2     site 1            15             0
#' expert 1   level 3     site 1            15          0.84
#' expert 1   level 4     site 1            15          0.02
#' expert 1   level 5     site 1            15          0.06
#' expert 1   level 1     site 2            35          0.02
#' expert 1   level 2     site 2            35          0.11
#' expert 1   level 3     site 2            35          0.19
#' expert 1   level 4     site 2            35          0.02
#' expert 1   level 5     site 2            35          0.66
#' ```
#'
#' @section Data cleaning:
#'
#' When data are added to the [elic_cat] object, first names are standardised
#' by converting capital letters to lower case, and by removing any whitespaces
#' and punctuation. Then, data are anonymised by converting names to short sha1
#' hashes. In this way, sensible information collected during the elicitation
#' process never reaches the [elic_cat] object.
#'
#' @return The provided object of class [elic_cat] updated with the data.
#' @export
#'
#' @family cat data helpers
#'
#' @examples
#' # Create the elic_cat object for an elicitation process with three
#' # mechanisms, four sites, five levels and a maximum of six experts per
#' # mechanism
#' my_levels <- c("level_1", "level_2", "level_3", "level_4", "level_5")
#' my_sites <- c("site_1", "site_2", "site_3", "site_4")
#' my_mechanisms <- c("mechanism_1", "mechanism_2", "mechanism_3")
#' x <- elic_cat_start(levels = my_levels,
#'                     sites = my_sites,
#'                     experts = 6,
#'                     mechanisms = my_mechanisms)
#'
#' # Add data for the three mechanisms from a data.frame. Notice that the
#' # three commands can be piped
#' my_elicit <- elic_cat_add_data(x,
#'                                data_source = mechanism_1,
#'                                mechanism = "mechanism_1") |>
#'   elic_cat_add_data(data_source = mechanism_2, mechanism = "mechanism_2") |>
#'   elic_cat_add_data(data_source = mechanism_3, mechanism = "mechanism_3")
#' my_elicit
#'
#' # Add data for the first and second round from a csv file
#' files <- list.files(path = system.file("extdata", package = "elicitr"),
#'                     pattern = "mechanism_",
#'                     full.names = TRUE)
#' my_elicit <- elic_cat_add_data(x,
#'                                data_source = files[1],
#'                                mechanism = "mechanism_1") |>
#'   elic_cat_add_data(data_source = files[2], mechanism = "mechanism_2") |>
#'   elic_cat_add_data(data_source = files[3], mechanism = "mechanism_3")
#' my_elicit
#'
#' # Add data for the first and second round from a xlsx file with three sheets
#' file <- list.files(path = system.file("extdata", package = "elicitr"),
#'                    pattern = "mechanisms",
#'                    full.names = TRUE)
#' # Using the sheet index
#' my_elicit <- elic_cat_add_data(x,
#'                                data_source = file,
#'                                sheet = 1,
#'                                mechanism = "mechanism_1") |>
#'   elic_cat_add_data(data_source = file,
#'                     sheet = 2,
#'                     mechanism = "mechanism_2") |>
#'   elic_cat_add_data(data_source = file,
#'                     sheet = 3,
#'                     mechanism = "mechanism_3")
#' my_elicit
#' # Using the sheet name
#' my_elicit <- elic_cat_add_data(x, data_source = file,
#'                                sheet = "Mechanism 1",
#'                                mechanism = "mechanism_1") |>
#'   elic_cat_add_data(data_source = file,
#'                     sheet = "Mechanism 2",
#'                     mechanism = "mechanism_2") |>
#'   elic_cat_add_data(data_source = file,
#'                     sheet = "Mechanism 3",
#'                     mechanism = "mechanism_3")
#' my_elicit
#'
#' @examplesIf interactive()
#' # Add data for the first and second round from Google Sheets
#' googlesheets4::gs4_deauth()
#' gs <- "18VHeHB89P1s-6banaVoqOP-ggFmQZYx-z_31nMffAb8"
#' # Using the sheet index
#' my_elicit <- elic_cat_add_data(x,
#'                                data_source = gs,
#'                                sheet = 1,
#'                                mechanism = "mechanism_1") |>
#'   elic_cat_add_data(data_source = gs,
#'                     sheet = 2,
#'                     mechanism = "mechanism_2") |>
#'   elic_cat_add_data(data_source = gs,
#'                     sheet = 3,
#'                     mechanism = "mechanism_3")
#' my_elicit
#' # Using the sheet name
#' my_elicit <- elic_cat_add_data(x, data_source = gs,
#'                                sheet = "Mechanism 1",
#'                                mechanism = "mechanism_1") |>
#'   elic_cat_add_data(data_source = gs,
#'                     sheet = "Mechanism 2",
#'                     mechanism = "mechanism_2") |>
#'   elic_cat_add_data(data_source = gs,
#'                     sheet = "Mechanism 3",
#'                     mechanism = "mechanism_3")
#' my_elicit
elic_cat_add_data <- function(x,
                              data_source,
                              mechanism,
                              ...,
                              sep = ",",
                              sheet = 1,
                              overwrite = FALSE,
                              verbose = TRUE) {

  # Check if the object is of class elic_cat
  check_elic_obj(x, type = "cat")

  # Check if mechanism is a character string of length 1
  check_is_character(mechanism, "mechanism")
  check_length(mechanism, "mechanism", 1)
  # Check if mechanism is available in the object
  check_value_in_element(x,
                         element = "mechanisms",
                         value = mechanism)

  # Read data
  data <- read_data(data_source,
                    sep = sep,
                    sheet = sheet)

  # Check if data has the correct number of columns
  check_columns(data, 5)
  colnames(data) <- c("id", "level", "site", "confidence", "estimate")

  # Check columns type
  check_columns_type(data[1:3], "character")
  check_columns_type(data[4:5], c("numeric", "integer"))

  # First check that names, levels and sites are as expected
  # Check that unique names are <= expected experts
  check_names_levels_sites(x, data, type = "name")

  # Check that levels are those recorded in the object
  check_names_levels_sites(x, data, type = "levels")

  # Check that sites are those recorded in the object
  check_names_levels_sites(x, data, type = "sites")

  # Then check that the data is formatted as expected
  # Check that each name is repeated as many times as the number of levels and
  # sites
  check_column_format(data, col = "id")

  # Check that each level block is repeated as many times as the number of
  # experts and sites
  check_column_format(data, col = "level")

  # Check that each site is repeated as many times as the number of experts and
  # levels
  check_column_format(data, col = "site")

  # Check that each confidence value is repeated as many times as the number of
  # experts
  check_column_format(data, col = "confidence")

  # Anonymise names
  data <- anonimise_names(data)

  # Check if estimates for each expert and site sum to 1. This is done after
  # anonymising the names to avoid exposing the names in the error message.
  check_sum_1(data)

  x[["data"]][[mechanism]] <- data

  if (verbose) {
    cli::cli_alert_success("Data added to Mechanism {.val {mechanism}} from \\
                            {.val {src}}")
  }

  x
}

# Checkers----

#' Check if value is in the list element.
#'
#' @param x [elic_cont] object.
#' @param element character string with the name of the element to be checked.
#' @param value character string with the value to be checked.
#'
#' @return An error if `value` is not among the available values of list
#' element.
#' @noRd
#'
#' @author Sergio Vignali
check_value_in_element <- function(x,
                                   element,
                                   value) {

  if (element == "mechanisms") {
    values <- names(x[["data"]])
  } else {
    values <- x[[element]]
  }

  if (!value %in% values) {

    error <- "{.val {value}} not present in the {.cls {class(x)}} object."
    info <- "Available {element} {cli::qty(values)} {?is/are} {.val {values}}."
    cli::cli_abort(c("Invalid value for {.arg {element}}:",
                     "x" = error,
                     "i" = info),
                   call = rlang::caller_env())
  }
}

check_names_levels_sites <- function(x, data, type) {

  error <- ""

  if (type == "name") {
    names <- unique(data[["id"]])
    n <- x[["experts"]]

    if (length(names) > n) {

      text <- "The number of unique names is greater than the expected number \\
               of experts:"
      error <- "There are {.val {length(names)}} unique names but they should \\
                be no more than {.val {n}}."
    }

  } else if (type == "levels") {

    levels <- unique(data[["level"]])
    diff <- setdiff(levels, x[["levels"]])

    if (length(diff) > 0) {

      text <- "The column with the name of the levels contains unexpected \\
               values:"
      error <- "The value{?s} {.val {diff}} {?is/are} not valid."
    }

  } else if (type == "sites") {

    sites <- unique(data[["site"]])
    diff <- setdiff(sites, x[["sites"]])

    if (length(diff) > 0) {

      text <- "The column with the name of the sites contains unexpected \\
               values:"
      error <- "The value{?s} {.val {diff}} {?is/are} not valid."
    }
  }

  if (nchar(error) > 0) {

    info <- "Check the metadata in the {.cls elic_cat} object."

    cli::cli_abort(c(text,
                     "x" = error,
                     "i" = info),
                   call = rlang::caller_env())
  }
}

#' Check column format
#'
#' Checks if the given column is formatted as expected.
#'
#' @param x data.frame with the data to be checked.
#'
#' @return An error if the given column containing is malformed.
#' @noRd
#'
#' @author Sergio Vignali
check_column_format <- function(x, col) {

  if (col == "confidence") {
    n_levels <- length(unique(x[["level"]]))
    diff <- rle(x[[col]])[["lengths"]] %% n_levels

    if (sum(diff) == 0) {
      expected_values <- x[[col]]
    } else {
      # Create dummy wrong values
      expected_values <- seq_len(nrow(x))
    }
  } else {
    col_values <- unique(x[[col]])
    diff_cols <- setdiff(c("id", "level", "site"), col)
    col_1 <- unique(x[[diff_cols[[1]]]]) |>
      length()
    col_2 <- unique(x[[diff_cols[[2]]]]) |>
      length()

    if (col == "id") {
      expected_values <- rep(col_values, each = col_1 * col_2)
    } else if (col == "level") {
      expected_values <- rep(col_values, col_1 * col_2)
    } else {
      expected_values <- rep(col_values, col_1, each = col_2)
    }
  }

  if (!identical(x[[col]], expected_values)) {

    what <- switch(col,
                   "id" = "expert names",
                   "level" = "levels",
                   "site" = "sites",
                   "confidence" = "confidence values")

    error <- "The column containing the {what} is not formatted as \\
              expected."
    info <- "See Data format in {.fn elicitr::elic_cat_add_data}."

    cli::cli_abort(c("Malformatted dataset:",
                     "x" = error,
                     "i" = info),
                   call = rlang::caller_env())
  }
}

#' Check estimates
#'
#' Check if estimates for each expert and site sum to 1.
#'
#' @param x data.frame with the data to be checked.
#'
#' @return An error if estimates for each expert and site don't sum to 1.
#' @noRd
#'
#' @author Sergio Vignali
check_sum_1 <- function(x) {

  sums <- x |>
    # Convert to facto to avoid unwanted reorder of the table rows
    dplyr::mutate("id" = factor(.data[["id"]],
                                levels = unique(.data[["id"]]))) |>
    dplyr::group_by(.data[["id"]], .data[["site"]]) |>
    dplyr::summarise(sum = sum(.data[["estimate"]]))
  sums_vector <- sums |>
    dplyr::pull("sum")

  total <- sum(sums_vector > 1 + 1.5e-8 | sums_vector < 1 - 1.5e-8)

  if (total > 0) {

    idx <- which(sums_vector != 1)
    wrong_data <- sums[idx, ]

    if (total == 1) {
      error <- "Estimates of one expert and one site don't sum to 1."
    } else {
      error <- "Estimates of one/some experts for one/some sites don't sum \\
                to 1."
    }

    msg <- paste0("{cli::symbol$bullet} Check {.field id} {.val ",
                  wrong_data[[1]],
                  "} for {.field site} {.val ",
                  wrong_data[[2]], "}: sum {.val {",
                  wrong_data[[3]], "}}")

    cli::cli_abort(c("Invalid value for {.arg estimate}:",
                     "x" = error,
                     msg),
                   call = rlang::caller_env())
  }
}
