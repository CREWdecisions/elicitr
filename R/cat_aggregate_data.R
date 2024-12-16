#' Aggregate categorical data
#'
#' Aggregate the data from a [`elic_cat`] object.
#'
#' @inheritParams elic_cat_get_data
#' @param method character string with the name of the method to aggregate the
#' data. The available methods are: _basic_ and _bootstrap_, see Methods below.
#' @param votes numeric indicating the number of votes to consider, used only in
#' the _basic_ aggregation method.
#'
#' @section Methods:
#' Add explanation for the methods.
#'
#' @return A [`tibble`][tibble::tibble] with the aggregated data.
#' @export
#'
#' @family cat data helpers
#'
#' @author Sergio Vignali and Maude Vernet
#'
#' @references Vernet, M., Trask, A.E., Andrews, C.E., Ewen, J.E., Medina, S.,
#' Moehrenschlager, A. & Canessa, S. (2024) Assessing invasion risks using
#' EICAT‐based expert elicitation: application to a conservation translocation.
#' Biological Invasions, 26(8), 2707–2721.
#' <https://doi.org/10.1007/s10530-024-03341-2>
cat_aggregate_data <- function(x,
                               method,
                               mechanism,
                               ...,
                               votes = 100,
                               site = "all") {

  # Check if the object is of class elic_cat
  check_elic_obj(x, type = "cat")

  # Check if method is a character string of length 1
  check_length(method, "method", 1)

  # Check if method is available
  check_method(x, method)

  # Get data
  data <- elic_cat_get_data(x, mechanism = mechanism, site = site)

  data
}
