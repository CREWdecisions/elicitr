# Helpers----

#' Get estimates
#'
#' Get the estimates based on the elicitation type. If the elicitation type is
#' the _four points elicitation_, the data are rescaled to be between 0 and 1.
#'
#' @param data [`tibble`][tibble::tibble] with the expert estimates.
#' @param v character string with the name of the variable.
#' @param n_experts numeric indicating the number of experts.
#' @param verbose logical, if TRUE it prints informative messages.
#' @param var_type character string with the variable type.
#' @param elic_type character string with the elicitation type.
#'
#' @returns A numeric vector with the expert estimates.
#' @noRd
#'
#' @author Sergio Vignali
get_est <- function(data, v, n_experts, var_type,
                    elic_type, verbose, scale_conf) {

  if (elic_type == "1p") {
    # One point elicitation
    estimates <- dplyr::pull(data,
                             2)

  } else {
    # Three or four points elicitation
    if (elic_type == "4p") {

      # Rescale min and max
      data <- rescale_data(data, scale_conf)
      needs_resc <- any(data[["min"]] < 0,
                        na.rm = TRUE) || any(data[["max"]] > 1,
                                             na.rm = TRUE)

      if (var_type == "p" && needs_resc) {

        data[["min"]] <- pmax(0, pmin(1, data[["min"]]))
        data[["max"]] <- pmax(0, pmin(1, data[["max"]]))
        fn <- as.list(sys.call(-1))[[1]]
        warn <- "Some values have been constrained to be between {.val {0}} \\
                 and {.val {1}}."
        cli::cli_warn(c("!" = warn,
                        " " = "See the {.field scale_conf} section in the \\
                               help page for {.fn elicitr::{fn}} for more \\
                               information."))
      }

      if (verbose) {
        cli::cli_alert_success("Rescaled min and max based on confidence \\
                                estimates for variable {.val {v}}.")
      }
    }

    estimates <- dplyr::select(data,
                               2:4)
  }

  estimates
}

#' Get sample
#'
#' Get the sample based on the elicitation type. If the elicitation type is the
#' _one point elicitation_, the sample is drawn from the expert estimates. If
#' the elicitation type is the  _three points elicitation_ or tje
#' _four points elicitation_, the sample is drawn from the PERT distribution.
#'
#' @param estimates numeric vector with the expert estimates.
#' @param n_samp numeric vector with the number of samples for each expert.
#' @param e numeric indicating the expert index.
#' @param elic_type character string with the elicitation type.
#'
#' @returns A numeric vector with the sampled data.
#' @noRd
#'
#' @author Sergio Vignali and Maude Vernet
get_sample <- function(estimates, n_samp, e, elic_type) {

  if (elic_type == "1p") {
    samp <- rep(estimates[e], n_samp[[e]])
  } else if (anyNA(c(estimates[[1]][[e]],
                     estimates[[2]][[e]],
                     estimates[[3]][[e]]))) {
    samp <- rep(NA, n_samp[[e]])
  } else {
    samp <- mc2d::rpert(n = n_samp[[e]],
                        min = estimates[[1]] [[e]],
                        mode = estimates[[3]] [[e]],
                        max = estimates[[2]] [[e]])
  }

  samp
}

#' Remove placeholders
#'
#' Remove placeholders with only NAs (might have been added in cat_add_data)
#'
#' @param x the data to sample
#' @param round teh round being sampled
#'
#' @returns The data without the potential placeholders row(s)
#' @noRd
#'
#' @author Maude Vernet
remove_placeholders <- function(x, round) {
  all_na <- NULL
  for (i in seq_len(nrow(x[["data"]][[round]]))) {
    if (all(is.na(x[["data"]][[round]][i, ]))) {

      all_na <- c(i, all_na)
    }
  }
  if (!is.null(all_na)) {
    x[["data"]][[round]] <- x[["data"]][[round]][-all_na, ]
  }

  x[["data"]][[round]]
}
