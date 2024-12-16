#' #' Get data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Get data from an [elic_cat] object.
#'
#' @inheritParams elic_cat_add_data
#' @param site character string with the name of the site or character vector
#' with more sitess that you want to extract from the data. Use `all` for all
#' variables.
#'
#' @return A [`tibble`][tibble::tibble] with the extracted data.
#' @export
#'
#' @family cat data helpers
#'
#' @author Sergio Vignali
#'
#' @examples
#' # Create the elic_cat object for an elicitation process with three
#' # mechanisms, four sites, five levels and a maximum of six experts per
#' # mechanism
#' my_levels <- c("level_1", "level_2", "level_3", "level_4", "level_5")
#' my_sites <- c("site_1", "site_2", "site_3", "site_4")
#' my_mechanisms <- c("mechanism_1", "mechanism_2", "mechanism_3")
#' my_elicit <- elic_cat_start(levels = my_levels,
#'                             sites = my_sites,
#'                             experts = 6,
#'                             mechanisms = my_mechanisms) |>
#'   elic_cat_add_data(data_source = mechanism_1, mechanism = "mechanism_1") |>
#'   elic_cat_add_data(data_source = mechanism_2, mechanism = "mechanism_2") |>
#'   elic_cat_add_data(data_source = mechanism_3, mechanism = "mechanism_3")
#'
#' # Get all data from Mechanism 1
#' elic_cat_get_data(my_elicit, mechanism = "mechanism_1")
#'
#' # Get data by site name----
#' # Get data for site_1 from Mechanism 2
#' elic_cat_get_data(my_elicit, mechanism = "mechanism_2", site = "site_1")
#'
#' # Get data for site_1 and site_3 from Mechanism 3
#' elic_cat_get_data(my_elicit,
#'                   mechanism = "mechanism_3",
#'                   site = c("site_1", "site_3"))
elic_cat_get_data <- function(x,
                              mechanism,
                              ...,
                              site = "all") {

  # Check if the object is of class elic_cat
  check_elic_obj(x, type = "cat")

  # Check if mechanism is a character string of length 1
  check_is_character(mechanism, "mechanism")
  check_length(mechanism, "mechanism", 1)

  # Check if mechanism is available in the object
  check_value_in_element(x,
                         element = "mechanism",
                         value = mechanism)

  if (length(site) == 1 && site == "all") {
    out <- x[["data"]][[mechanism]]
  } else {
    # Check if site is available in the object
    check_value_in_element(x,
                           element = "sites",
                           value = site)

    # Check if site is not among the available sites in the data
    available_sites <- unique(x[["data"]][[mechanism]][["site"]])
    diff <- setdiff(site, available_sites)

    if (length(diff) > 0) {
      error <- "{cli::qty(diff)} Site{?s} {.val {diff}} not available in \\
                mechanism {.val {mechanism}}."
      info <- "Available site{?s}: {.val {available_sites}}."
      cli::cli_abort(c("Invalid value for {.arg {site}}:",
                       "x" = error,
                       "i" = info))
    }
    # Avoid overwrite dplyr variable
    vals <- site
    out <- x[["data"]][[mechanism]] |>
      dplyr::filter(.data[["site"]] %in% vals)

  }

  out
}
