#' Add data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `elic_cat_add_data()` adds data to an `elic_cat` object from different
#' sources.
#'
#' @param x
#' @param mechanism
#' @inheritParams elic_cont_add_data
#'
#' @return The provided object of class `elic_cat` updated with the data.
#' @export
#'
#' @examples
elic_cat_add_data <- function(x,
                              data_source,
                              mechanism,
                              ...,
                              sep = ",",
                              sheet = 1,
                              overwrite = FALSE,
                              verbose = TRUE) {

}
