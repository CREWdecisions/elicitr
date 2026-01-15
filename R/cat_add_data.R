#' Add data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `cat_add_data()` adds data to an [elic_cat] object from different sources.
#'
#' @param x an object of class [elic_cat].
#' @param topic character string that indicates the mechanism to which the data
#' belongs.
#' @inheritParams cont_add_data
#'
#' @section Data format:
#'
#' For each topic, data are expected to have five columns, built as follows:
#' * The first column of the data should hold the names of the experts. The name
#' of each expert should be repeated as many times as the number of categories
#' and options. (i.e. each expert should appear \eqn{number\ of\ categories
#' \cdot number\ of\ options} times).
#' * The second column should be the names of the categories considered in the
#' elicitation. Each block of categories should be repeated as many times as the
#' number of options considered.
#' * The third column should hold the names of the options considered in the
#' study. The name of each option should be repeated as many times as the number
#' of categories considered.
#' * The fourth column should be the experts confidence in their own estimates
#' (given in percent). Experts should estimate how confident they are in their
#' estimates for each block of categories and for each option. Therefore, expert
#' confidence estimates should be repeated as many times as the number of
#' categories of impact considered.
#' * The final column should be the estimates of each expert for each option and
#' category. These estimates should sum up to 1 (or 100) for each expert and
#' option.
#'
#' The name of the columns is not important, `cat_add_data()` will overwrite
#' them according to the following convention:
#'
#' The first column will be renamed `id`, the second column `category`, the
#' third column `option`, the fourth column `confidence`, and the fifth column
#' `estimate`.
#'
#' Here is an example of data correctly formatted for an elicitation with five
#' categories and two options (only one expert is shown):
#'
#' ```
#' name         category       option      confidence      estimate
#' ----------------------------------------------------------------
#' expert 1     category 1     option 1            15          0.08
#' expert 1     category 2     option 1            15          0
#' expert 1     category 3     option 1            15          0.84
#' expert 1     category 4     option 1            15          0.02
#' expert 1     category 5     option 1            15          0.06
#' expert 1     category 1     option 2            35          0.02
#' expert 1     category 2     option 2            35          0.11
#' expert 1     category 3     option 2            35          0.19
#' expert 1     category 4     option 2            35          0.02
#' expert 1     category 5     option 2            35          0.66
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
#' my_categories <- c("category_1", "category_2", "category_3",
#'                    "category_4", "category_5")
#' my_options <- c("option_1", "option_2", "option_3", "option_4")
#' my_topics <- c("topic_1", "topic_2", "topic_3")
#' x <- cat_start(categories = my_categories,
#'                options = my_options,
#'                experts = 6,
#'                topics = my_topics)
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
#' # Using the sheet name
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
cat_add_data <- function(x,
                         data_source,
                         topic,
                         ...,
                         sep = ",",
                         sheet = 1,
                         overwrite = FALSE,
                         verbose = TRUE) {

  # Check if the object is of class elic_cat
  check_elic_obj(x, type = "cat")

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

  # Check if data has the correct number of columns
  check_columns(data, 5)
  colnames(data) <- c("id", "category", "option", "confidence", "estimate")

  # Check columns type
  check_columns_type(data[1:3], "character")
  check_columns_type(data[4:5], c("numeric", "integer"))

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

  # Anonymise names
  data <- anonimise_names(data)

  # Check if estimates for each expert and option sum to 1. This is done after
  # anonymising the names to avoid exposing the names in the error message.
  check_sum_1(data)

  x[["data"]][[topic]] <- data

  if (verbose) {
    cli::cli_alert_success("Data added to Topic {.val {topic}} from \\
                            {.val {src}}")
  }

  x
}

# Checkers----

#' Check names and categories
#'
#' Checks if names and categories are as expected, i.e. if the number of unique
#' names is less than or equal to the expected number of experts, and if the
#' categories are those recorded in the object.
#'
#' @param x [elic_cat] object.
#' @param data [tibble][tibble::tibble] with the data to be checked.
#' @param type character string with the type of check to be performed.
#'
#' @returns An error if the number of unique names is greater than the number
#' of experts or if the categories are not those recorded in the object.
#' @noRd
#'
#' @author Sergio Vignali
check_names_categories_options <- function(x, data, type) {

  error <- ""

  if (type == "name") {
    names <- unique(data[["id"]])
    n <- x[["experts"]]

    if (length(names) > n) {

      text <- "The number of unique names is greater than the expected number \\
               of experts:"
      error <- "There are {.val {length(names)}} unique names but they should \\
                be no more than {.val {n}}."
    }

  } else if (type == "categories") {

    categories <- unique(data[["category"]])
    diff <- setdiff(categories, x[["categories"]])

    if (length(diff) > 0) {

      text <- "The column with the name of the categories contains unexpected \\
               values:"
      error <- "The value{?s} {.val {diff}} {?is/are} not valid."
    }

  } else if (type == "options") {

    options <- unique(data[["option"]])
    diff <- setdiff(options, x[["options"]])

    if (length(diff) > 0) {

      text <- "The column with the name of the options contains unexpected \\
               values:"
      error <- "The value{?s} {.val {diff}} {?is/are} not valid."
    }
  }

  if (nchar(error) > 0) {

    info <- "Check the metadata in the {.cls elic_cat} object."

    cli::cli_abort(c(text,
                     "x" = error,
                     "i" = info),
                   call = rlang::caller_env())
  }
}

#' Check column format
#'
#' Checks if the given column is formatted as expected.
#'
#' @param x data.frame with the data to be checked.
#'
#' @return An error if the given column containing is malformed.
#' @noRd
#'
#' @author Sergio Vignali
check_column_format <- function(x, col) {

  if (col == "confidence") {
    n_categories <- length(unique(x[["category"]]))
    diff <- rle(x[[col]])[["lengths"]] %% n_categories

    if (sum(diff) == 0) {
      expected_values <- x[[col]]
    } else {
      # Create dummy wrong values
      expected_values <- seq_len(nrow(x))
    }
  } else {
    col_values <- unique(x[[col]])
    diff_cols <- setdiff(c("id", "category", "option"), col)
    col_1 <- unique(x[[diff_cols[[1]]]]) |>
      length()
    col_2 <- unique(x[[diff_cols[[2]]]]) |>
      length()

    if (col == "id") {
      expected_values <- rep(col_values, each = col_1 * col_2)
    } else if (col == "category") {
      expected_values <- rep(col_values, col_1 * col_2)
    } else {
      expected_values <- rep(col_values, col_1, each = col_2)
    }
  }

  if (!identical(x[[col]], expected_values)) {

    what <- switch(col,
                   "id" = "expert names",
                   "category" = "categories",
                   "option" = "options",
                   "confidence" = "confidence values")

    error <- "The column containing the {what} is not formatted as \\
              expected."
    info <- "See Data format in {.fn elicitr::cat_add_data}."

    cli::cli_abort(c("Malformatted dataset:",
                     "x" = error,
                     "i" = info),
                   call = rlang::caller_env())
  }
}

#' Check estimates
#'
#' Check if estimates for each expert and option sum to 1.
#'
#' @param x data.frame with the data to be checked.
#'
#' @return An error if estimates for each expert and option don't sum to 1.
#' @noRd
#'
#' @author Sergio Vignali
check_sum_1 <- function(x) {

  sums <- x |>
    # Convert to factor to avoid unwanted reorder of the table rows
    dplyr::mutate("id" = factor(.data[["id"]],
                                levels = unique(.data[["id"]]))) |>
    dplyr::group_by(.data[["id"]], .data[["option"]]) |>
    dplyr::summarise(sum = sum(.data[["estimate"]]), .groups = "drop")
  sums_vector <- sums |>
    dplyr::pull("sum")
  tol <- 1.5e-8
  #in case estimates were given in proportions
  bad_1 <- sums_vector > 1 + tol | sums_vector < 1 - tol
  #in case estimates were given in percents
  bad_100 <- sums_vector > 100 + tol | sums_vector < 100 - tol

  total <- sum(bad_1 & bad_100)

  if (total > 0) {

    idx <- which(bad_1 & bad_100)
    wrong_data <- sums[idx, ]

    if (total == 1) {
      error <- "Estimates of one expert and one option don't sum to 1 or 100."
    } else {
      error <- "Estimates of one/some experts for one/some options don't sum \\
                to 1 or 100."
    }

    msg <- paste0("{cli::symbol$bullet} Check {.field id} {.val ",
                  wrong_data[[1]],
                  "} for {.field option} {.val ",
                  wrong_data[[2]], "}: sum {.val {",
                  wrong_data[[3]], "}}")

    cli::cli_abort(c("Invalid value for {.arg estimate}:",
                     "x" = error,
                     msg),
                   call = rlang::caller_env())
  }
}
