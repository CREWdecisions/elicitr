#' #' Get data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Get data from an [elic_cat] object.
#'
#' @inheritParams cat_add_data
#' @param site character string with the name of the site or character vector
#' with the sites that you want to extract from the data. Use `all` for all
#' sites.
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
#' # four sites, five categories and a maximum of six experts per topic
#' my_categories <- c("category_1", "category_2", "category_3",
#'                    "category_4", "category_5")
#' my_sites <- c("site_1", "site_2", "site_3", "site_4")
#' my_topics <- c("topic_1", "topic_2", "topic_3")
#' my_elicit <- cat_start(categories = my_categories,
#'                        sites = my_sites,
#'                        experts = 6,
#'                        topics = my_topics) |>
#'   cat_add_data(data_source = topic_1, topic = "topic_1") |>
#'   cat_add_data(data_source = topic_2, topic = "topic_2") |>
#'   cat_add_data(data_source = topic_3, topic = "topic_3")
#'
#' # Get all data from Topic 1
#' cat_get_data(my_elicit, topic = "topic_1")
#'
#' # Get data by site name----
#' # Get data for site_1 from Topic 2
#' cat_get_data(my_elicit, topic = "topic_2", site = "site_1")
#'
#' # Get data for site_1 and site_3 from Topic 3
#' cat_get_data(my_elicit,
#'              topic = "topic_3",
#'              site = c("site_1", "site_3"))
cat_get_data <- function(x,
                         topic,
                         ...,
                         site = "all") {

  # Check if the object is of class elic_cat
  check_elic_obj(x, type = "cat")

  # Check if topic is a character string of length 1
  check_is_character(topic, "topic")
  check_length(topic, "topic", 1)

  # Check if topic is available in the object
  check_value_in_element(x,
                         element = "topic",
                         value = topic)

  if (length(site) == 1 && site == "all") {
    out <- x[["data"]][[topic]]
  } else {
    # Check if site is available in the object
    check_value_in_element(x,
                           element = "sites",
                           value = site)

    # Check if site is not among the available sites in the data
    available_sites <- unique(x[["data"]][[topic]][["site"]])
    diff <- setdiff(site, available_sites)

    if (length(diff) > 0) {
      error <- "{cli::qty(diff)} Site{?s} {.val {diff}} not available in \\
                topic {.val {topic}}."
      info <- "Available site{?s}: {.val {available_sites}}."
      cli::cli_abort(c("Invalid value for argument {.arg site}:",
                       "x" = error,
                       "i" = info))
    }
    # Avoid overwrite dplyr variable
    vals <- site
    out <- x[["data"]][[topic]] |>
      dplyr::filter(.data[["site"]] %in% vals)

  }

  out
}
