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
