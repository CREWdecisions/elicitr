#' Summarise samples of continuous data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `summary()` summarises the sampled data and provides the minimum, first
#' quartile, median, mean, third quartile, and maximum values for each variable.
#'
#' @param object an object of class `cont_sample` created by the function
#' [cont_sample_data].
#' @param ... Unused arguments, included only for future extensions of the
#' function.
#' @param var character vector with the names of the variables to summarise. If
#' `var = "all"`, all variables are summarised.
#'
#' @returns A [`tibble`][tibble::tibble] with the summary statistics.
#' @export
#'
#' @family cont data helpers
#'
#' @author Sergio Vignali
#'
#' @examples
#' # Create the elict object and add data for the first and second round from a
#' # data.frame.
#' my_elicit <- cont_start(var_names = c("var1", "var2", "var3"),
#'                         var_types = "ZNp",
#'                         elic_types = "134",
#'                         experts = 6) |>
#'   cont_add_data(x, data_source = round_1, round = 1) |>
#'   cont_add_data(data_source = round_2, round = 2)
#'
#' # Sample data for the second round for all variables
#' samp <- cont_sample_data(my_elicit, round = 2)
#'
#' # Summarise the sampled data for all variables
#' summary(samp)
#'
#' # Summarise the sampled data for the variable "var1"
#' summary(samp, var = "var1")
#'
#' # Summarise the sampled data for the variables "var1" and "var3"
#' summary(samp, var = c("var1", "var3"))
summary.cont_sample <- function(object,
                                ...,
                                var = "all") {

  if (length(var) == 1 && var == "all") {
    vars <- unique(object[["var"]])
  } else {
    # Check if variable is available in the object
    check_var_in_sample(object, var)
    vars <- var
  }

  object |>
    dplyr::filter(.data[["var"]] %in% vars) |>
    dplyr::select(!"id") |>
    dplyr::group_by(.data[["var"]]) |>
    dplyr::summarise("Min" = min(.data[["value"]], na.rm = TRUE),
                     "Q1" = stats::quantile(.data[["value"]], probs = 0.25,
                                            na.rm = TRUE),
                     "Median" = median(.data[["value"]], na.rm = TRUE),
                     "Mean" = mean(.data[["value"]], na.rm = TRUE),
                     "Q3" = stats::quantile(.data[["value"]], probs = 0.75,
                                            na.rm = TRUE),
                     "Max" = max(.data[["value"]], na.rm = TRUE)) |>
    dplyr::rename("Var" = "var")
}
