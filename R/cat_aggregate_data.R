#' Aggregate categorical data
#'
#' Aggregate the data from a [`elic_cat`] object.
#'
#' @inheritParams elic_cat_add_data
#' @param method character string with the name of the method to aggregate the
#' data. The available methods are: _basic_ and _bootstrap_.
#' @param votes numeric indicating the number of votes to consider used in the
#' _basic_ aggregation method.
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
                               ...,
                               votes = 100) {

  # Check if the object is of class elic_cat
  check_elic_obj(x, type = "cat")

  # Check if method is available
  check_method(x, method)
}
