#' #' Get data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Get data from an [elic_cont] object.
#'
#' @inheritParams elic_cat_add_data
#' @param sites character vector with the names of the sites from which the data
#' will be extracted. If `sites = "all"`, the data from all sites will be
#' extracted.
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
                              sites = "all") {

  x[["data"]][[mechanism]]
}
