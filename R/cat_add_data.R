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
  check_is_character(mechanisms, "mechanisms")
  check_length(mechanisms, "mechanism", 1)

  data <- read_data(data_source,
                    sep = sep,
                    sheet = sheet)
}
