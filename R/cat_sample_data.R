#' Sample categorical data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' `cat_sample_data()` samples data based on expert estimates stored in the
#' [`elic_cat`] object.
#'
#' @inheritParams cat_get_data
#' @param method character string with the name of the method to sample the
#' data. The available methods are: _unweighted_ and _weighted_, see Methods
#' below.
#' @param n_votes numeric indicating the number of votes to consider.
#' @param verbose logical, if TRUE it prints informative messages.
#'
#' @section Methods:
#' Two methods are implemented. These methods are explained in Vernet et al.
#' (2024), see references below.
#'
#' * _unweighted_: This method samples data based on the expert estimates
#' without accounting for their confidence. Values are sampled from a Dirichlet
#' distribution using the expert estimates as parameters. When only one estimate
#' is provided, i.e. 100 % for one category, the method assigns 100 % to all
#' votes for this category.
#'
#' * _weighted_: This method samples data based on the expert estimates
#' accounting for their confidence. The confidence is used to weight the
#' number of votes assigned to each expert. The method samples data from a
#' Dirichlet distribution using the expert estimates as parameters. When only
#' one estimate is provided, i.e. 100 % for one category, the method assigns 100
#' % to all votes for this category.
#'
#' @details
#' Weighted allocations distribute the full budget among answering experts.
#' Thus, the number of row in the sampled data will n_experts * n_votes. If
#' an expert doesn't provide data for the sampled variable, a placeholder row is
#' added. In such case, row counts can exceed n_experts * n_votes.
#'
#' @return An [`tibble`][tibble::tibble] with the sampled data. This object has
#' the additional class `cat_sample` used to implement the plotting method.
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
#' # Sample data from Topic 2 for option 1 and 3 using the weighted method
#' samp <- cat_sample_data(my_elicit,
#'                         method = "weighted",
#'                         topic = "topic_2",
#'                         option = c("option_1", "option_3"))
cat_sample_data <- function(x,
                            method,
                            topic,
                            ...,
                            n_votes = 100,
                            option = "all",
                            verbose = TRUE) {

  # Check if the object is of class elic_cat
  check_elic_obj(x, type = "cat", metadata = FALSE)

  # Check if method is a character string of length 1
  check_length(method, "method", 1)

  # Check if method is available
  check_method(x, method)

  # Check n_votes
  check_n_votes(n_votes)

  # Get data
  data <- cat_get_data(x, topic = topic, option = option)

  out <- do_sampling(data, method, n_votes)

  if (verbose) {
    cli::cli_alert_success("Data sampled successfully using {.val {method}} \\
                          method.")
  }

  # Prepend new class
  class(out) <- c("cat_sample", class(out))

  # Add attribute with the name of the topic
  attr(out, "topic") <- topic

  out
}
