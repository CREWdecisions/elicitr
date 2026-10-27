#' Add data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' `cat_add_data()` adds data to an [elic_cat] object from different sources.
#'
#' @param x an object of class [elic_cat].
#' @param data_source either a [`data.frame`][base::data.frame] or
#' [`tibble`][tibble::tibble], a string with the path to a _csv_ or _xlsx_ file,
#' or anything accepted by the [read_sheet][googlesheets4::read_sheet] function.
#' @param topic character string that indicates the mechanism to which the data
#' belongs.
#' @param ... Unused arguments, included only for future extensions of the
#' function.
#' @param sep character used as field separator, used only when data_source is a
#' path to a csv file.
#' @param sheet integer or character to select the sheet. The sheet can be
#' referenced by its position with a number or by its name with a string. Used
#' only when data_source is a path to a xlsx file or when data are imported from
#' Google Sheets.
#' @param verbose logical, if TRUE it prints informative messages.
#' @param anonymise logical, if TRUE expert names are anonymised before adding
#' the data to the [elic_cat] object.
#'
#' @section Data format:
#'
#' For each topic, data are expected to have five columns, built as follows:
#' * The first column of the data should hold the names of the experts. The name
#' of each expert should be repeated as many times as the number of categories
#' and options. (i.e. each expert should appear \eqn{number\ of\ categories
#' \cdot number\ of\ options} times).
#' * The second column should hold the names of the options considered in the
#' study. The name of each option should be repeated as many times as the number
#' of categories considered. If you do not use multiple options in your study,
#' please input 1 option for all elements.
#' * The third column should be the names of the categories considered in the
#' elicitation. Each block of categories should be repeated as many times as the
#' number of options considered.
#' * The fourth column should be the experts confidence in their own estimates
#' (given in percent). Experts should estimate how confident they are in their
#' estimates for each block of categories and for each option. Therefore, expert
#' confidence estimates should be repeated as many times as the number of
#' categories of impact considered for each option.
#' * The final column should be the estimates of each expert for each option and
#' category. These estimates should sum up to 1 (probabilities) (or 100
#' (percentages)) for each expert and option.
#'
#' The name of the columns is not important, `cat_add_data()` will overwrite
#' them according to the following convention:
#'
#' The first column will be renamed `id`, the second column `option`, the
#' third column `category`, the fourth column `confidence`, and the fifth column
#' `estimate`.
#'
#' Here is an example of data correctly formatted for an elicitation with five
#' categories and two options (only one expert is shown):
#'
#' ```
#' name         option       category      confidence      estimate
#' ----------------------------------------------------------------
#' expert 1     option 1     category 1            55          0.08
#' expert 1     option 1     category 2            55          0
#' expert 1     option 1     category 3            55          0.84
#' expert 1     option 1     category 4            55          0.02
#' expert 1     option 1     category 5            55          0.06
#' expert 1     option 2     category 1            75          0.02
#' expert 1     option 2     category 2            75          0.11
#' expert 1     option 2     category 3            75          0.19
#' expert 1     option 2     category 4            75          0.02
#' expert 1     option 2     category 5            75          0.66
#' ```
#'
#' @section Data cleaning:
#'
#' When data are added to the [elic_cat] object, first names are standardised
#' by converting capital letters to lower case, and by removing any whitespaces
#' and punctuation. Then, data are anonymised by converting names to short sha1
#' hashes. In this way, sensible information collected during the elicitation
#' process never reaches the [elic_cat] object.
#'
#' @return The provided object of class [elic_cat] updated with the data.
#' @export
#'
#' @family cat data helpers
#'
#' @author Sergio Vignali and Maude Vernet
#'
#' @examples
#' # Create the elic_cat object for an elicitation process with three topics,
#' # four options, five categories and a maximum of six experts per topic
#' my_topics <- c("topic_1", "topic_2", "topic_3")
#' my_categories <- c("category_1", "category_2", "category_3",
#'                    "category_4", "category_5")
#' my_options <- c("option_1", "option_2", "option_3", "option_4")
#' x <- cat_start(topics = my_topics,
#'                options = my_options,
#'                categories = my_categories,
#'                experts = 6)
#'
#' # Add data for the three topics from a data.frame. Notice that the three
#' # commands can be piped
#' my_elicit <- cat_add_data(x,
#'                           data_source = topic_1,
#'                           topic = "topic_1") |>
#'   cat_add_data(data_source = topic_2, topic = "topic_2") |>
#'   cat_add_data(data_source = topic_3, topic = "topic_3")
#' my_elicit
#'
#' # Add data for the first and second round from a csv file
#' files <- list.files(path = system.file("extdata", package = "elicitr"),
#'                     pattern = "topic_",
#'                     full.names = TRUE)
#' my_elicit <- cat_add_data(x,
#'                           data_source = files[1],
#'                           topic = "topic_1") |>
#'   cat_add_data(data_source = files[2], topic = "topic_2") |>
#'   cat_add_data(data_source = files[3], topic = "topic_3")
#' my_elicit
#'
#' # Add data for the first and second round from a xlsx file with three sheets
#' file <- list.files(path = system.file("extdata", package = "elicitr"),
#'                    pattern = "topics",
#'                    full.names = TRUE)
#' # Using the sheet index
#' my_elicit <- cat_add_data(x,
#'                           data_source = file,
#'                           sheet = 1,
#'                           topic = "topic_1") |>
#'   cat_add_data(data_source = file,
#'                sheet = 2,
#'                topic = "topic_2") |>
#'   cat_add_data(data_source = file,
#'                sheet = 3,
#'                topic = "topic_3")
#' my_elicit
#' # Using the sheet name
#' my_elicit <- cat_add_data(x,
#'                           data_source = file,
#'                           sheet = "Topic 1",
#'                           topic = "topic_1") |>
#'   cat_add_data(data_source = file,
#'                sheet = "Topic 2",
#'                topic = "topic_2") |>
#'   cat_add_data(data_source = file,
#'                sheet = "Topic 3",
#'                topic = "topic_3")
#' my_elicit
#'
#' @examplesIf interactive()
#' # Add data for the first and second round from Google Sheets
#' googlesheets4::gs4_deauth()
#' gs <- "18VHeHB89P1s-6banaVoqOP-ggFmQZYx-z_31nMffAb8"
#'
#' # Using the sheet index
#' my_elicit <- cat_add_data(x,
#'                           data_source = gs,
#'                           sheet = 1,
#'                           topic = "topic_1") |>
#'   cat_add_data(data_source = gs,
#'                sheet = 2,
#'                topic = "topic_2") |>
#'   cat_add_data(data_source = gs,
#'                sheet = 3,
#'                topic = "topic_3")
#' my_elicit
#'
#' # (You can also do this using the sheet name)
#' \dontrun{
#' my_elicit <- cat_add_data(x, data_source = gs,
#'                           sheet = "Topic 1",
#'                           topic = "topic_1") |>
#'   cat_add_data(data_source = gs,
#'                sheet = "Topic 2",
#'                topic = "topic_2") |>
#'   cat_add_data(data_source = gs,
#'                sheet = "Topic 3",
#'                topic = "topic_3")
#' my_elicit
#' }
cat_add_data <- function(x,
                         data_source,
                         topic,
                         ...,
                         sep = ",",
                         sheet = 1,
                         verbose = TRUE,
                         anonymise = TRUE) {

  # Check if the object is of class elic_cat
  check_elic_obj(x, type = "cat", metadata = TRUE)

  # Check if topic is a character string of length 1
  check_is_character(topic, "topic")
  check_length(topic, "topic", 1)
  # Check if topic is available in the object
  check_value_in_element(x,
                         element = "topic",
                         value = topic)

  # Read data
  data <- read_data(data_source,
                    sep = sep,
                    sheet = sheet)

  if (nrow(data) == 0) {
    cli::cli_abort(c("Data is empty.",
                     "x" = "Please add data",
                     "i" = "To test whether the data is correctly formed, add \\
                     a test response."))
  }

  # Check if data has the correct number of columns
  check_columns(data, 5, type = "cat")
  colnames(data) <- c("id", "option", "category", "confidence", "estimate")

  # Check columns type
  check_columns_type(data[1:3], "character")
  check_columns_type(data[4:5], c("numeric", "integer"))
  check_cat_values(data)

  # First check that names, categories and options are as expected
  # Check that unique names are <= expected experts
  check_names_categories_options(x, data, type = "name")

  # Check that categories are those recorded in the object
  check_names_categories_options(x, data, type = "categories")

  # Check that options are those recorded in the object
  check_names_categories_options(x, data, type = "options")

  # Then check that the data is formatted as expected
  # Check that each name is repeated as many times as the number of categories
  # and options
  check_column_format(data, col = "id")

  # Check that each category block is repeated as many times as the number of
  # experts and options
  check_column_format(data, col = "category")

  # Check that each option is repeated as many times as the number of experts
  # and categories
  check_column_format(data, col = "option")

  # Check that each confidence value is repeated as many times as the number of
  # experts
  check_column_format(data, col = "confidence")

  if (anonymise) {
    # Anonymise names
    data <- anonimise_names(data)
  }

  # Check if estimates for each expert and option sum to 1. This is done after
  # anonymising the names to avoid exposing the names in the error message.
  data <- check_sum_1(data)

  # Check that NAs happen for all categories in an option and for both
  # confidence and estimate columns
  check_na(data)

  x[["data"]][[topic]] <- data

  if (verbose) {
    cli::cli_alert_success("Data added to Topic {.val {topic}} from \\
                            {.val {src}}")
  }

  x
}
