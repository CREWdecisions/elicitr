#' Add data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `elic_cat_add_data()` adds data to an [elic_cat] object from different
#' sources.
#'
#' @param x an object of class [elic_cat].
#' @param data_source either a [`data.frame`][base::data.frame] or
#' [`tibble`][tibble::tibble], a string with the path to a _csv_ or _xlsx_ file,
#' or anything accepted by the [read_sheet()][googlesheets4::read_sheet]
#' function.
#' @param mechanism integer indicating what mechanism the data belongs to.
#' @inheritParams elic_cont_add_data
#'
#' @section Data Format:
#'
#' For each mechanism, data are expected to have five columns, built as follows:
#' * The first column of the data should hold the names of the experts. The name
#' of each expert should be repeated as many times as there are impact levels
#' and sites considered in the elicitation
#' (i.e. each expert should appear \eqn{number\ of\ levels
#' \cdot number\ of\ sites} times).
#' * The second column should be the names of the levels considered in the
#' eliciation. Each level should be repeated as many times as there are sites
#' considered.
#' * The third column should hold the names of the sites considered in the
#' study. The name of each site should be repeated as many times as the number
#' of levels.
#' * The fourth column should be the experts confidence levels (given in
#' percents). Experts should each give one confidence level per site, so each
#' confidence level should be repeated for all levels at each site.
#' * The final column should be the estimates of each expert.
#'
#' @section Data cleaning:
#'
#' When data are added to the [elic_cat] object, first names are standardised
#' by converting capital letters to lower case, and by removing any whitespaces
#' and punctuation. Then, data are anonymised by converting names to short sha1
#' hashes. In this way, sensible information collected during the elicitation
#' process never reaches the [elic_cat] object.
#'
#' If the data are imported from _Google Sheets_, `elic_cat_add_data()`
#' performs additional data cleaning operations. This is relevant when data are
#' collected with Google Forms because, for example, there could be multiple
#' submission by the same expert or a different decimal separator could be used.
#' When data are collected with Google Form, a column with the date and time is
#' recorded. First, the function checks for multiple submissions and if present,
#' only the last submission is retained. Second, the function removes the column
#' with the timestamp. Then it checks for consistency of the decimal separator,
#' i.e. commas _,_ are replaced with periods _._. Finally, columns four and five
#' (which contain the confidence levels and estimates) are forced to numeric.
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
#' mechanism = 1) |>
#'   elic_cat_add_data(data_source = mechanism_2, mechanism = 2)
#' my_elicit
#'
#' # Add data for the first and second round from a csv file
#' files <- list.files(path = system.file("extdata", package = "elicitr"),
#'                     pattern = "round_",
#'                     full.names = TRUE)
#' my_elicit <- elic_cat_add_data(x, data_source = files[1], mechanism = 1) |>
#'   elic_cat_add_data(data_source = files[2], mechanism = 2)
#' my_elicit
#'
#' # Add data for the first and second round from a xlsx file with two sheets
#' file <- list.files(path = system.file("extdata", package = "elicitr"),
#'                    pattern = "rounds",
#'                    full.names = TRUE)
#' # Using the sheet index
#' my_elicit <- elic_cat_add_data(x, data_source = file,
#'                                sheet = 1, mechanism = 1) |>
#'   elic_cat_add_data(data_source = file, sheet = 2, mechanism = 2)
#' my_elicit
#' # Using the sheet name
#' my_elicit <- elic_cat_add_data(x, data_source = file,
#'                                sheet = "Mechanism 1", mechanism = 1) |>
#'   elic_cat_add_data(data_source = file, sheet = "Mechanism 2",
#'                     mechanism = 2)
#' my_elicit
#'
#' @examplesIf interactive()
#' # Add data for the first and second round from Google Sheets
#' googlesheets4::gs4_deauth()
#' gs <- "18VHeHB89P1s-6banaVoqOP-ggFmQZYx-z_31nMffAb8"
#' # Using the sheet index
#' my_elicit <- elic_cat_add_data(x, data_source = gs,
#'                                sheet = 1, mechanism = 1) |>
#'   elic_cat_add_data(data_source = gs, sheet = 2, mechanism = 2)
#' my_elicit
#' # Using the sheet name
#' my_elicit <- elic_cat_add_data(x, data_source = gs,
#'                                sheet = "Mechanism 1", mechanism = 1) |>
#'   elic_cat_add_data(data_source = gs, sheet = "Mechanism 2",
#'                     mechanism = 2)
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

  # Check if estimates for each expert and site sum to 1
  check_sum_1(data)

  # Anonymise names
  data <- anonimise_names(data)

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
#'
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

check_sum_1 <- function(x) {

  sums <- x |>
    dplyr::group_by(id, site) |>
    dplyr::summarise(sum = sum(estimate)) |>
    dplyr::pull(sum)

  total <- sum(sums != 1)

  if (total > 0) {

    idx <- which(sums != 1)
    wrong_data <- x[idx, c(1, 3)]

    error <- "Estimates of {cli::qty(total)} {?one/some} expert{?s} don't sum \\
              to 1."
    msg <- "Check {.arg id}: {.val {wrong_data[[1]]}} at \\
            {.val {wrong_data[[2]]}}"
    info <- cli::cli_ul(msg)

    cli::cli_abort(c("Invalid value for {.arg estimate}:",
                     "x" = error,
                     "i" = info),
                   call = rlang::caller_env())
  }
}
