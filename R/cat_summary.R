#' Summarise samples of categorical data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `summary()` summarises the sampled data and provides the minimum, first
#' quartile, median, mean, third quartile, and maximum values for each category.
#'
#' @param object an object of class `cat_sample` created by the function
#' [cat_sample_data].
#' @param option character string with the name of the option.
#' @param ... Unused arguments, included only for future extensions of the
#' function.
#'
#' @returns A [`tibble`][tibble::tibble] with the summary statistics.
#' @export
#'
#' @family cat data helpers
#'
#' @author Sergio Vignali
#'
#' @examples
#' # Create the elic_cat object for an elicitation process with three topics,
#' # four options, five categories and a maximum of six experts per topic
#' my_categories <- c("category_1", "category_2", "category_3",
#'                    "category_4", "category_5")
#' my_options <- c("option_1", "option_2", "option_3", "option_4")
#' my_topics <- c("topic_1", "topic_2", "topic_3")
#' my_elicit <- cat_start(categories = my_categories,
#'                        options = my_options,
#'                        experts = 6,
#'                        topics = my_topics) |>
#'   cat_add_data(data_source = topic_1, topic = "topic_1") |>
#'   cat_add_data(data_source = topic_2, topic = "topic_2") |>
#'   cat_add_data(data_source = topic_3, topic = "topic_3")
#'
#' # Sample data from Topic 1 for all options using the basic method
#' samp <- cat_sample_data(my_elicit,
#'                         method = "basic",
#'                         topic = "topic_1")
#'
#' # Summarise the sampled data
#' summary(samp, option = "option_1")
summary.cat_sample <- function(object,
                               option,
                               ...) {

  # Check if option is available
  check_option(object, option)

  # Avoid overwriting dplyr variable
  opt <- option

  object <- object |>
    dplyr::filter(.data[["option"]] == opt) |>
    dplyr::select(-c("id", "option"))

  out <- matrix(NA, nrow = ncol(object), ncol = 6)

  out[, 1] <- sapply(object, min)
  out[, 2] <- sapply(object, stats::quantile, probs = 0.25)
  out[, 3] <- sapply(object, median)
  out[, 4] <- sapply(object, mean)
  out[, 5] <- sapply(object, stats::quantile, probs = 0.75)
  out[, 6] <- sapply(object, max)

  colnames(out) <- c("Min", "Q1", "Median", "Mean", "Q3", "Max")

  out |>
    tibble::as_tibble() |>
    dplyr::mutate("Category" = colnames(object), .before = 1)
}
