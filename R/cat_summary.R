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
#' @param option character string with the name of the option(s). If
#' `option = "all"`, all options are summarised.
#' @param ... Unused arguments, included only for future extensions of the
#' function.
#'
#' @returns A [table] with the summary statistics.
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
#' # Sample data from Topic 1 for all options using the unweighted method
#' samp <- cat_sample_data(my_elicit,
#'                         method = "unweighted",
#'                         topic = "topic_1")
#'
#' # Summarise the sampled data
#' summary(samp, option = "option_1")
summary.cat_sample <- function(object,
                               option = "all",
                               ...) {

  # Check if option is available
  check_option(object, option)

  # Avoid overwriting dplyr variable
  opt <- option
  if (option == "all") {
    opt <- unique(object[["option"]])
  }

  object <- object |>
    dplyr::filter(.data[["option"]] %in% opt) |>
    dplyr::select(-c("id")) |>
    dplyr::group_by(.data[["option"]]) |>
    dplyr::mutate(observation = dplyr::row_number()) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(cols = -c(option, observation),
                        names_to = "category",
                        values_to = "value") |>
    tidyr::pivot_wider(names_from = option,
                       values_from = value) |>
    dplyr::select(category, everything(), -observation)

  na_option <- c()
  for (i in colnames(object)[-1]){
    na_opt <- ifelse(all(is.na(object[[i]])), i, NA)
    na_option <- c(na_option, na_opt)
  }

  object <- object[, which(!colnames(object) %in% na_option)]
  na_option <- stats::na.omit(na_option)

  if (length(na_option) != 0) {
    if (ncol(object) == 1) {
      cli::cli_abort(c("No estimate provided",
                       "x" = "the provided data only holds NAs",
                       "i" = "No data provided in {.val {na_option}}."))
    } else {
      cli::cli_alert("Results were dropped for {.val {na_option}} as no \\
                          estimate was provided.")
    }
  }

  out <- list()
  for (i in colnames(object)[-1]) {
    out[[i]] <- object |>
      dplyr::group_by(.data[["category"]]) |>
      dplyr::summarise("Min" = min(.data[[i]],
                                   na.rm = TRUE),
                       "Q1" = stats::quantile(.data[[i]], probs = 0.25,
                                              na.rm = TRUE),
                       "Median" = median(.data[[i]],
                                         na.rm = TRUE),
                       "Mean" = mean(.data[[i]],
                                     na.rm = TRUE),
                       "Q3" = stats::quantile(.data[[i]], probs = 0.75,
                                              na.rm = TRUE),
                       "Max" = max(.data[[i]],
                                   na.rm = TRUE))
  }

  out
}
