# Helpers----

#' Add group data
#'
#' Add data for the group mean to the data.frame.
#'
#' @param data tibble with the elicitation data.
#' @param elic_type character string with the elicitation type.
#'
#' @return A tibble with the group data added.
#' @noRd
#'
#' @author Sergio Vignali and Maude Vernet
add_group_data <- function(data, elic_type) {

  if (elic_type == "1p") {
    data <- data |>
      dplyr::add_row("id" = "Group",
                     "best" = mean(data[["best"]], na.rm = TRUE),
                     "col" = "group")
  } else if (elic_type == "3p") {
    data <- data |>
      dplyr::add_row("id" = "Group",
                     "best" = mean(data[["best"]], na.rm = TRUE),
                     "min" = NA,
                     "max" = NA,
                     "col" = "group")
  } else {
    data <- data |>
      dplyr::add_row("id" = "Group",
                     "best" = mean(data[["best"]], na.rm = TRUE),
                     "min" = NA,
                     "max" = NA,
                     "conf" = 100,
                     "col" = "group")
  }

  data
}

#' Add 2.5th–97.5th percentile interval for 1-point elicitation
#'
#' Add summary statistics (mean and 95\% CI) around the group mean value in a
#' 1-point elicitation plot.
#' @param data tibble with the elicitation data.
#' @param elic_type character string with the elicitation type.
#'
#' @return A tible with the 2.5th–97.5th percentile interval of estimates.
#' @noRd
#' @author Maude Vernet

add_ci <- function(data, elic_type) {
  data_ci <- data.frame("id" = "Group",
                        "group95ciPOS" = quantile(data[["best"]], 0.975,
                                                  na.rm = TRUE),
                        "group95ciNEG" = quantile(data[["best"]], 0.025,
                                                  na.rm = TRUE),
                        "col" = "group",
                        stringsAsFactors = FALSE)

  data_ci
}

#' Add truth data
#'
#' Add data for the true values to the data.frame.
#'
#' @param data tibble with the elicitation data.
#' @param truth list with the true values.
#' @param elic_type character string with the elicitation type.
#'
#' @return A tibble with the true data added.
#' @noRd
#'
#' @author Sergio Vignali and Maude Vernet
add_truth_data <- function(data, truth, elic_type) {

  if (elic_type == "1p") {
    data <- dplyr::add_row(data,
                           "id" = "Truth",
                           "best" = truth[["best"]],
                           "col" = "truth")
  } else if (elic_type == "3p") {
    data <- dplyr::add_row(data,
                           "id" = "Truth",
                           "best" = truth[["best"]],
                           "min" = truth[["min"]],
                           "max" = truth[["max"]],
                           "col" = "truth")
  } else if (elic_type == "4p") {
    data <- dplyr::add_row(data,
                           "id" = "Truth",
                           "best" = truth[["best"]],
                           "min" = truth[["min"]],
                           "max" = truth[["max"]],
                           "conf" = truth[["conf"]],
                           "col" = "truth")
  }

  data
}
