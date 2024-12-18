#' Sample categorical data
#'
#' Sample data based on expert estimates stored in the [`elic_cat`] object.
#'
#' @inheritParams elic_cat_get_data
#' @param method character string with the name of the method to sample the
#' data. The available methods are: _basic_ and _bootstrap_, see Methods below.
#' @param n_votes numeric indicating the number of votes to consider, used only
#' in the _basic_ aggregation method.
#'
#' @section Methods:
#' Two methods are implemented. These methods are explained in Vernet et al.
#' (2024), see references below.
#'
#' * _basic_: This method samples data based on the expert estimates without
#' accounting for their confidence. Values are sampled from a Dirichlet
#' distribution using the expert estimates as parameters. When only one estimate
#' is provided, i.e. 100 % for one level, the method assigns 100 % to all votes
#' for this level.
#'
#' * _bootstrap_: This method samples data based on the expert estimates
#' accounting for their confidence. The confidence is used to weight the
#' number of votes assigned to each expert. The method samples data from a
#' Dirichlet distribution using the expert estimates as parameters. When only
#' one estimate is provided, i.e. 100 % for one level, the method assigns 100 %
#' to all votes for this level.
#'
#' @return A [`tibble`][tibble::tibble] with the sampled data.
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
cat_sample_data <- function(x,
                            method,
                            mechanism,
                            ...,
                            n_votes = 100,
                            site = "all") {

  # Check if the object is of class elic_cat
  check_elic_obj(x, type = "cat")

  # Check if method is a character string of length 1
  check_length(method, "method", 1)

  # Check if method is available
  check_method(x, method)

  # Get data
  data <- elic_cat_get_data(x, mechanism = mechanism, site = site)

  experts <- unique(data[["id"]])
  levels <- unique(data[["level"]])
  sites <- unique(data[["site"]])

  if (method == "basic") {
    out <- basic_sampling(data, n_votes, experts, levels, sites)
  } else {
    out <- bootstrap_sampling(data, n_votes, experts, levels, sites)
  }

  cli::cli_alert_success("Data sampled successfully using {.val {method}} \\
                          method.")

  out
}

# Methods----

basic_sampling <- function(x, n_votes, experts, levels, sites) {

  # Prepare data frame for sampled probabilities
  sp <- matrix(0,
               nrow = length(experts) * n_votes * length(sites),
               ncol = length(levels) + 2,
               dimnames = list(NULL, c("id", "site", levels))) |>
    tibble::as_tibble() |>
    dplyr::mutate("id" = rep(experts, each = n_votes, times = length(sites)),
                  "site" = rep(sites, each = n_votes * length(experts)))

  for (s in sites) {
    estimates <- get_estimates(x, s)

    for (e in seq_along(experts)) {

      expert_est <- estimates[e, -1]

      # Strictly positive estimates
      idx <- which(expert_est > 0)

      if (length(idx) == 1) {
        # If there is only one positive estimate, this is assumed to be 1 since
        # the sum of probabilities must be 1. In this case, assign 100% to all
        # votes for this level.
        samp <- 1
      } else {
        samp <- extraDistr::rdirichlet(n = n_votes,
                                       alpha = expert_est[idx])
      }

      sp[sp[["site"]] == s & sp[["id"]] == experts[e], idx + 2] <- samp
    }
  }

  sp
}

bootstrap_sampling <- function(x, n_votes, experts, levels, sites) {

  all_sites <- vector(mode = "list", length = length(sites))
  all_sp <- vector(mode = "list", length = length(experts))

  for (s in sites) {

    estimates <- get_estimates(x, s)
    conf <- get_conf(x, s, length(levels))

    # Determine weight of each expert proportional to their expressed confidence
    w <- (length(experts) * n_votes * conf / sum(conf)) |>
      miceadds::sumpreserving.rounding(digits = 0, preserve = TRUE)

    for (e in seq_along(experts)) {

      # Prepare data frame for sampled probabilities
      sp <- matrix(0,
                   nrow = w[[e]],
                   ncol = length(levels) + 2,
                   dimnames = list(NULL, c("id", "site", levels))) |>
        tibble::as_tibble() |>
        dplyr::mutate("site" = s, "id" = experts[e])

      expert_est <- estimates[e, -1]

      # Strictly positive estimates
      idx <- which(expert_est > 0)

      if (length(idx) == 1) {
        # If there is only one positive estimate, this is assumed to be 1 since
        # the sum of probabilities must be 1. In this case, assign 100% to all
        # votes for this level.
        samp <- 1
      } else {
        samp <- extraDistr::rdirichlet(n = w[[e]],
                                       alpha = expert_est[idx])
      }

      sp[, idx + 2] <- samp
      all_sp[[e]] <- sp
    }

    all_sites[[s]] <- do.call(rbind, all_sp)
  }

  out <- do.call(rbind, all_sites) |>
    tibble::remove_rownames()

  out
}

get_estimates <- function(x, y) {

  x |>
    dplyr::filter(.data[["site"]] == y) |>
    dplyr::select(!dplyr::all_of(c("confidence", "site"))) |>
    tidyr::pivot_wider(names_from = "level", values_from = "estimate")
}

get_conf <- function(x, y, z) {

  x |>
    dplyr::filter(dplyr::row_number() %% z == 1,
                  .data[["site"]] == y) |>
    dplyr::pull("confidence")
}
