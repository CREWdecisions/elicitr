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
#' * The first column will be renamed `name`, the second column `level`, the
#' third column `site`, the fourth column `confidence`, and the fifth column
#' `estimate`.
#'
#' Here is an example of how data correctly formatted should look like, for an
#' elicitation with two experts, two levels of impact,two sites,  and one
#' mechanism:
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
#' x <- elic_cat_start(levels = c("levels_1", "levels_2", "levels_3",
#'                                "levels_4", "levels_5"),
#'                     sites = c("site_1", "site_2", "site_3", "site_4"),
#'                     experts = 6,
#'                     mechanisms = c("mechanism_1", "mechanism_2",
#'                                    "mechanism_3"))
#'
#' # Add data for the three mechanisms from a data.frame. Notice that the
#' # three commands can be piped
#' my_elicit <- elic_cat_add_data(x, data_source = mechanism_1,
#'                                mechanism = "mechanism_1") |>
#'   elic_cat_add_data(data_source = mechanism_2, mechanism = "mechanism_2") |>
#'   elic_cat_add_data(data_source = mechanism_3, mechanism = "mechanism_3")
#' my_elicit
#'
#' # Add data for the first and second round from a csv file
#' files <- list.files(path = system.file("extdata", package = "elicitr"),
#'                     pattern = "mechanism_",
#'                     full.names = TRUE)
#' my_elicit <- elic_cat_add_data(x, data_source = files[1],
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
#' my_elicit <- elic_cat_add_data(x, data_source = file,
#'                                sheet = 1, mechanism = "mechanism_1") |>
#'   elic_cat_add_data(data_source = file, sheet = 2,
#'                     mechanism = "mechanism_2") |>
#'   elic_cat_add_data(data_source = file, sheet = 3, mechanism = "mechanism_3")
#' my_elicit
#' # Using the sheet name
#' my_elicit <- elic_cat_add_data(x, data_source = file,
#'                                sheet = "Mechanism 1",
#'                                mechanism = "mechanism_1") |>
#'   elic_cat_add_data(data_source = file, sheet = "Mechanism 2",
#'                     mechanism = "mechanism_2") |>
#'   elic_cat_add_data(data_source = file, sheet = "Mechanism 3",
#'                     mechanism = "mechanism_3")
#' my_elicit
#'
#' @examplesIf interactive()
#' # Add data for the first and second round from Google Sheets
#' googlesheets4::gs4_deauth()
#' gs <- "18VHeHB89P1s-6banaVoqOP-ggFmQZYx-z_31nMffAb8"
#' # Using the sheet index
#' my_elicit <- elic_cat_add_data(x, data_source = gs,
#'                                sheet = 1, mechanism = "mechanism_1") |>
#'   elic_cat_add_data(data_source = gs, sheet = 2,
#'                     mechanism = "mechanism_2") |>
#'   elic_cat_add_data(data_source = gs, sheet = 3, mechanism = "mechanism_3")
#' my_elicit
#' # Using the sheet name
#' my_elicit <- elic_cat_add_data(x, data_source = gs,
#'                                sheet = "Mechanism 1",
#'                                mechanism = "mechanism_1") |>
#'   elic_cat_add_data(data_source = gs, sheet = "Mechanism 2",
#'                     mechanism = "mechanism_2") |>
#'   elic_cat_add_data(data_source = gs, sheet = "Mechanism 3",
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

  # Check that each name is repeated as many times as the number of levels and
  # sites
  # check_column_name(x, data)

  # Check that each level block is repeated as many times as the number of
  #experts and sites
  # check_column_level(x, data)

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

#' Check column name
#'
#' First it checks if the unique names are <= the expected experts. Then it
#' checks if the column containing the name is formatted as expected. The name
#' should be repeated as many times as the number of levels and sites.
#'
#' @param x [elic_cat] object.
#' @param data data.frame with the data to be checked.
#'
#' @return An error if the unique names are > than the expected experts or if
#' the column containing the name is not formatted as expected.
#' @noRd
#'
#' @author Sergio Vignali
check_column_name <- function(x, data) {

  n_levels <- length(x[["levels"]])
  n_sites <- length(x[["sites"]])

  expected_names <- rep(names, each = n_levels * n_sites)

  if (!identical(data[["id"]], expected_names)) {

    error <- "The column containing the expert name is not formatted as \\
              expected."
    info <- "See Data format in {.fn elicitr::elic_cat_add_data}."

    cli::cli_abort(c("Malformatted dataset:",
                     "x" = error,
                     "i" = info),
                   call = rlang::caller_env())
  }
}

#' Check column level
#'
#' First it checks if the unique names levels are those reordered in the
#' object. Then checks if the column containing the levels is formatted as
#' expected. Each level block should be repeated as many times as the number of
#' experts and sites.
#'
#' @param x [elic_cat] object.
#' @param data data.frame with the data to be checked.
#'
#' @return An error if the levels are not those recorded in the object or if the
#' column containing the levels is not formatted as expected.
#' @noRd
#'
#' @author Sergio Vignali
check_column_level <- function(x, data) {

  n_experts <- length(data[["level"]])
  n_sites <- length(data[["site"]])

  expected_names <- rep(names, n_levels * n_sites)

  if (!identical(data[["id"]], expected_names)) {

    error <- "The column containing the expert name is not formatted as \\
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
    dplyr::group_by(id, site) |>
    dplyr::summarise(sum = sum(estimate))
  sums_vector <- sums |>
    dplyr::pull(sum)

  total <- sum(sums_vector != 1)

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
