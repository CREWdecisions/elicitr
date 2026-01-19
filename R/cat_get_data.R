#' #' Get data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Get data from an [elic_cat] object.
#'
#' @inheritParams cat_add_data
#' @param option character string with the name of the option or character
#' vector with the options that you want to extract from the data. Use `all` for
#' all options.
#'
#' @return A [`tibble`][tibble::tibble] with the extracted data.
#' @export
#'
#' @family cat data helpers
#'
#' @author Sergio Vignali
#'
#' @examples
#' # Create the elic_cat object for an elicitation process with three topics,
#' # four options, five categories and a maximum of six experts per topic
#' my_topics <- c("topic_1", "topic_2", "topic_3")
#' my_options <- c("option_1", "option_2", "option_3", "option_4")
#' my_categories <- c("category_1", "category_2", "category_3",
#'                    "category_4", "category_5")
#' my_elicit <- cat_start(topics = my_topics,
#'                        options = my_options,
#'                        categories = my_categories,
#'                        experts = 6) |>
#'   cat_add_data(data_source = topic_1, topic = "topic_1") |>
#'   cat_add_data(data_source = topic_2, topic = "topic_2") |>
#'   cat_add_data(data_source = topic_3, topic = "topic_3")
#'
#' # Get all data from Topic 1
#' cat_get_data(my_elicit, topic = "topic_1")
#'
#' # Get data by option name----
#' # Get data for option_1 from Topic 2
#' cat_get_data(my_elicit, topic = "topic_2", option = "option_1")
#'
#' # Get data for option_1 and option_3 from Topic 3
#' cat_get_data(my_elicit,
#'              topic = "topic_3",
#'              option = c("option_1", "option_3"))
cat_get_data <- function(x,
                         topic,
                         ...,
                         option = "all") {

  # Check if the object is of class elic_cat
  check_elic_obj(x, type = "cat")

  # Check if topic is a character string of length 1
  check_is_character(topic, "topic")
  check_length(topic, "topic", 1)

  # Check if topic is available in the object
  check_value_in_element(x,
                         element = "topic",
                         value = topic)

  if (length(option) == 1 && option == "all") {
    out <- x[["data"]][[topic]]
  } else {
    # Check if option is available in the object
    check_value_in_element(x,
                           element = "options",
                           value = option)

    # Check if option is not among the available options in the data
    available_options <- unique(x[["data"]][[topic]][["option"]])
    diff <- setdiff(option, available_options)

    if (length(diff) > 0) {
      error <- "{cli::qty(diff)} Option{?s} {.val {diff}} not available in \\
                topic {.val {topic}}."
      info <- "Available option{?s}: {.val {available_options}}."
      cli::cli_abort(c("Invalid value for argument {.arg option}:",
                       "x" = error,
                       "i" = info))
    }
    # Avoid overwrite dplyr variable
    vals <- option
    out <- x[["data"]][[topic]] |>
      dplyr::filter(.data[["option"]] %in% vals)

  }

  out
}
