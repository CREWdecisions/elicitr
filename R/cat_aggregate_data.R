#' Aggregate categorical data
#'
#' Aggregate the data from a [`elic_cat`] object.
#'
#' @param x [elic_cat] object.
#' @param method character string with the name of the method to aggregate the
#' data. The available methods are: _basic_ and _bootstrap_.
#'
#' @return A [`tibble`][tibble::tibble] with the aggregated data.
#' @export
#'
#' @family cat data helpers
#'
#' @author Sergio Vignali and Maude Vernet
elic_cat_aggregate_data <- function(x,
                                    method) {

  # Check if the object is of class elic_cat
  check_elic_obj(x, type = "cat")

}
