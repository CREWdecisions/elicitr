#' Sample categorical data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `cat_sample_data()` samples data based on expert estimates stored in the
#' [`elic_cat`] object.
#'
#' @inheritParams cat_get_data
#' @param method character string with the name of the method to sample the
#' data. The available methods are: _basic_ and _bootstrap_, see Methods below.
#' @param n_votes numeric indicating the number of votes to consider.
#' @param verbose logical, if TRUE it prints informative messages.
#'
#' @section Methods:
#' Two methods are implemented. These methods are explained in Vernet et al.
#' (2024), see references below.
#'
#' * _basic_: This method samples data based on the expert estimates without
#' accounting for their confidence. Values are sampled from a Dirichlet
#' distribution using the expert estimates as parameters. When only one estimate
#' is provided, i.e. 100 % for one category, the method assigns 100 % to all
#' votes for this category.
#'
#' * _bootstrap_: This method samples data based on the expert estimates
#' accounting for their confidence. The confidence is used to weight the
#' number of votes assigned to each expert. The method samples data from a
#' Dirichlet distribution using the expert estimates as parameters. When only
#' one estimate is provided, i.e. 100 % for one category, the method assigns 100
#' % to all votes for this category.
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
#' # Sample data from Topic 1 for all options using the basic method
#' samp <- cat_sample_data(my_elicit,
#'                         method = "basic",
#'                         topic = "topic_1")
#'
#' # Sample data from Topic 2 for option 1 and 3 using the bootstrap method
#' samp <- cat_sample_data(my_elicit,
#'                         method = "bootstrap",
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
  check_elic_obj(x, type = "cat")

  # Check if method is a character string of length 1
  check_length(method, "method", 1)

  # Check if method is available
  check_method(x, method)

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

# Methods----

#' Do sampling
#'
#' Sample data based on expert estimates.
#'
#' @param x [`tibble`][tibble::tibble] with the expert estimates.
#' @param method character string with the name of the method to sample the
#' data, either _basic_ or _bootstrap_.
#' @param n_votes numeric indicating the number of votes to consider for each
#' expert.
#'
#' @returns A [`tibble`][tibble::tibble] with the sampled data.
#' @noRd
#'
#' @author Sergio Vignali and Maude Vernet
do_sampling <- function(x, method, n_votes) {

  experts <- unique(x[["id"]])
  categories <- unique(x[["category"]])
  options <- unique(x[["option"]])

  all_options <- vector(mode = "list", length = length(options))
  all_sp <- vector(mode = "list", length = length(experts))

  for (s in options) {

    estimates <- get_estimates(x, s)
    conf <- get_conf(x, s, length(categories))

    # Determine n sample of each expert
    if (method == "basic") {
      n_samp <- rep(n_votes, length(experts))
    } else {
      n_samp <- get_boostrap_n_sample(experts, n_votes, conf)
    }

    for (e in seq_along(experts)) {

      # Prepare data frame for sampled probabilities
      sp <- matrix(0,
                   nrow = n_samp[[e]],
                   ncol = length(categories) + 2,
                   dimnames = list(NULL, c("id", "option", categories))) |>
        tibble::as_tibble() |>
        dplyr::mutate("option" = s, "id" = experts[e])

      expert_est <- estimates[e, -1]

      # Strictly positive estimates
      idx <- which(expert_est > 0)

      if (length(idx) == 1) {
        # If there is only one positive estimate, this is assumed to be 1 since
        # the sum of probabilities must be 1. In this case, assign 100% to all
        # votes for this category.
        samp <- 1
      } else {
        samp <- extraDistr::rdirichlet(n = n_samp[[e]],
                                       alpha = expert_est[idx])
      }

      sp[, idx + 2] <- samp
      all_sp[[e]] <- sp
    }

    all_options[[s]] <- do.call(rbind, all_sp)
  }

  out <- do.call(rbind, all_options) |>
    tibble::remove_rownames()

  out
}

#' Get estimates
#'
#' Get the estimates for a specific option.
#'
#' @param x [`tibble`][tibble::tibble] with the expert estimates.
#' @param y character string with the option name.
#'
#' @returns A [`tibble`][tibble::tibble] with the estimates for the option.
#' @noRd
#'
#' @author Sergio Vignali
get_estimates <- function(x, y) {

  x |>
    dplyr::filter(.data[["option"]] == y) |>
    dplyr::select(!dplyr::all_of(c("confidence", "option"))) |>
    tidyr::pivot_wider(names_from = "category", values_from = "estimate")
}

#' Get confidence
#'
#' Get the confidence for a specific option.
#'
#' @param x [`tibble`][tibble::tibble] with the expert estimates.
#' @param y character string with the option name.
#' @param z numeric indicating the number of categories.
#'
#' @returns A numeric vector with the confidence values.
#' @noRd
#'
#' @author Sergio Vignali
get_conf <- function(x, y, z) {

  x |>
    dplyr::filter(dplyr::row_number() %% z == 1,
                  .data[["option"]] == y) |>
    dplyr::pull("confidence")
}
