# Methods----

#' Do sampling
#'
#' Sample data based on expert estimates.
#'
#' @param x [`tibble`][tibble::tibble] with the expert estimates.
#' @param method character string with the name of the method to sample the
#' data, either _unweighted_ or _weighted_.
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
    n_samp <- get_boostrap_n_sample(experts, n_votes, weights = conf,
                                    elic_type = method)

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

      if (length(idx) == 0) {
        # If there are no positive estimates -> aka is NA
        samp <- NA
        sp[, 3:(2 + length(categories))] <- samp
      } else if (length(idx) == 1) {
        # If there is only one positive estimate, this is assumed to be 1 since
        # the sum of probabilities must be 1. In this case, assign 100% to all
        # votes for this category.
        samp <- 1
      } else {
        samp <- extraDistr::rdirichlet(n = n_samp[[e]],
                                       alpha = expert_est[idx])
      }

      sp[, idx + 2] <- samp #only does it for non NAs
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
