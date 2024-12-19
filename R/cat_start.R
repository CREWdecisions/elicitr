#' Start elicitation
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `cat_start()` initialises an [elic_cat] object which stores important
#' metadata for the data collected during the elicitation process of categorical
#' data.
#'
#' @param categories character vector with the names of the categories. See
#' Categories for more.
#' @param options character vector with the names of all options investigated.
#' See Options for more.
#' @param experts numeric, indicating the maximum number of experts
#' participating in the elicitation process for one topic. See Experts for more.
#' @param topics character vector with the names of the topics.
#' @inheritParams cont_start
#'
#' @section Categories:
#'
#' Categories are inherited between topics. A minimum of two categories are
#' needed. If only one category is investigated, please refer to the functions
#' for the elicitation of continuous data (e.g. [cont_start]).
#'
#' @section Options:
#'
#' The option parameter is a character vector containing the names of all the
#' options investigated in the elicitation. However, not all options have to be
#' investigated in every topic.
#'
#' @section Experts:
#'
#' The expert parameter is a number indicating the maximum number of experts
#' taking part in the elicitation of one of the investigated topics. The number
#' and IDs of experts can differ between the topics.
#'
#' @return An object of class [elic_cat] binding metadata related to the
#' elicitation process. These metadata are used by other functions to validate
#' the correctness of the provided data.
#' @export
#'
#' @family cat data helpers
#'
#' @author Sergio Vignali and Maude Vernet
#'
#' @references Hemming, V., Burgman, M. A., Hanea, A. M., McBride, M. F., &
#' Wintle, B. C. (2018). A practical guide to structured expert elicitation
#' using the IDEA protocol. Methods in Ecology and Evolution, 9(1), 169–180.
#' <https://doi.org/10.1111/2041-210X.12857>
#' Vernet, M., Trask, A.E., Andrews, C.E., Ewen, J.E., Medina, S.,
#' Moehrenschlager, A. & Canessa, S. (2024) Assessing invasion risks using
#' EICAT‐based expert elicitation: application to a conservation translocation.
#' Biological Invasions, 26(8), 2707–2721.
#' <https://doi.org/10.1007/s10530-024-03341-2>
#'
#' @examples
#' # Create the elic_cat object for an elicitation process over 2 topics, 3
#' # options, 3 categories per options, and a maximum number of 8 experts per
#' # topic
#' my_categories <- c("category_1", "category_2", "category_3")
#' my_elicit <- cat_start(categories = my_categories,
#'                        options = c("option_1", "option_2", "option_3"),
#'                        experts = 8,
#'                        topics = c("topic_1","topic_2"))
#' my_elicit
#'
#' # A title can be added to bind a name to the object:
#' my_elicit <- cat_start(categories = my_categories,
#'                        options = c("option_1", "option_2", "option_3"),
#'                        experts = 8,
#'                        topics = c("topic_1","topic_2"),
#'                        title = "My elicitation")
#' my_elicit
cat_start <- function(categories,
                      options,
                      experts,
                      topics,
                      ...,
                      title = "Elicitation",
                      verbose = TRUE) {

  # Check that categories, options, and topics are character vectors
  check_is_character(categories, "categories")
  check_is_character(options, "options")
  check_is_character(topics, "topics")

  # Check that the argument `experts` is a number
  check_experts_arg(experts)

  obj <- new_elic_cat(categories = categories,
                      options = options,
                      experts = experts,
                      topics = topics,
                      title)

  if (verbose) {
    cli::cli_alert_success("{.cls elic_cat} object for {.val {title}} \\
                            correctly initialised")
  }

  obj
}

# Checkers----

#' Check if the argument `x` is a character vector
#'
#' @param x the object to be checked.
#' @param arg_name character string with the name of the argument to be checked.
#'
#' @return An error if `x` is not a character vector.
#' @noRd
#'
#' @author Sergio Vignali
check_is_character <- function(x, arg_name) {

  if (!is.character(x)) {
    error <- "Argument {.arg {arg_name}} must be {.cls character} not \\
              {.cls {class(x)}}."
    cli::cli_abort(c("Invalid value for {.arg {arg_name}}:",
                     "x" = error,
                     "i" = "See {.fn elicitr::cat_start}."),
                   call = rlang::caller_env())
  }
}
