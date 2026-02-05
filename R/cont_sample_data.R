#' Sample continuous data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `cont_sample_data()` samples data based on expert estimates stored in the
#' [`elic_cont`] object.
#'
#' @inheritParams cont_get_data
#' @param method character string with the name of the method to sample the
#' data, only the _basic_ is implemented, see Method below.
#' @param n_votes numeric indicating the number of votes to consider.
#' @param weights numeric vector with the weights to apply to the estimates. If
#' equal to `1`, each experts get `n_votes` votes, see Weights below.
#' @param verbose logical, if TRUE it prints informative messages.
#'
#' @section Weights:
#' To provide a different number of votes to each expert, use the `weights`
#' argument. The length of the vector must be equal to the number of experts. If
#' provided when the elicitation type is the _four points elicitation_, their
#' values overwrite the confidence estimates.
#'
#' @section Method:
#' The function samples the data using the basic method. The basic method
#' samples the data based on the expert estimates with differences between the
#' different elicitation types:
#'
#' * _one point elicitation_: the best estimate of each expert represent the
#' pool of values that are sampled `n_votes` `*` `n_experts` times, with
#' repetition.
#'
#' * _three points elicitation_: the minimum, best, and maximum estimates of
#' each expert are used as scaling parameters of the PERT distribution from
#' which the data are sampled. The `weights` argument can be used to weight the
#' estimates of each expert.
#'
#' * _four points elicitation_: the minimum, best, and maximum estimates of
#' each expert are rescaled according to their confidence and used as scaling
#' parameters of the PERT distribution from which the data are sampled.
#'
#'
#' @section scale_conf:
#'
#' If the variable plotted is the result of a four points elicitation where
#' expert confidence is provided, the minimum and maximum values provided by
#' each expert are rescaled using their provided confidence categories. Users
#' can choose how they want to rescale minimum and maximum values by providing a
#' value for the `scale_conf` argument. If no argument is provided, a default
#' value of 100 is used for scale_conf.
#'
#' The scaled minimum and maximum values are obtained with:
#'
#' \eqn{minimum = best\ guess - (best\ guess - minimum)\frac{scale\_conf}
#' {confidence}}
#'
#' \eqn{maximum = best\ guess + (maximum - best\ guess) \frac{scale\_conf}
#' {confidence}}
#'
#' @returns A [`tibble`][tibble::tibble] with the sampled data. This object has
#' an additional class `cont_sample` used to implement the plotting method.
#' @export
#'
#' @family cont data helpers
#'
#' @author Sergio Vignali
#'
#' @examples
#' # Create the elict object and add data for the first and second round from a
#' # data.frame.
#' my_elicit <- cont_start(var_names = c("var1", "var2", "var3"),
#'                         var_types = "ZNp",
#'                         elic_types = "134",
#'                         experts = 6) |>
#'   cont_add_data(x, data_source = round_1, round = 1) |>
#'   cont_add_data(data_source = round_2, round = 2)
#'
#' # Sample data for the second round for all variables
#' samp <- cont_sample_data(my_elicit, round = 2)
#' samp
#'
#' # Sample data for the first round for the variable `var1` and `var2`
#' samp <- cont_sample_data(my_elicit, round = 1, var = c("var1", "var2"))
#'
#' # Sample data for the second round for the variable `var3`. Notice that the
#' # data are rescaled using the expert confidence before sampling.
#' samp <- cont_sample_data(my_elicit, round = 2, var = "var1")
#'
#' # Sample data for the first round for the variable `var3` providing the
#' # weights. Notice that the weights overwrite the confidence estimates and
#' # that some values are constrained to be between 0 and 1 after rescaling.
#' samp <- cont_sample_data(my_elicit, round = 1, var = "var3",
#'                          weights = c(0.8, 0.7, 0.9, 0.7, 0.6, 0.9))
cont_sample_data <- function(x,
                             round,
                             ...,
                             method = "basic",
                             var = "all",
                             n_votes = 1000,
                             weights = 1,
                             verbose = TRUE) {

  # # Check if the object is of class elic_cont
  check_elic_obj(x, type = "cont")

  # Check round
  check_round(round)

  # Check if method is available
  check_method(x, method)

  if (length(var) == 1 && var == "all") {
    vars <- x[["var_names"]]
  } else {
    # Check if var is available in the object
    check_var(x, var)
    vars <- var
  }

  experts <- unique(x[["data"]][[round]][["id"]])
  n_experts <- length(experts)
  all_sp <- vector(mode = "list", length = n_experts)
  all_vars <- vector(mode = "list", length = length(vars))

  # Check weights argument
  check_weights(weights, n_experts)

  if (length(weights) == 1) {
    weights <- rep(weights, n_experts)
  }

  for (v in vars) {

    elic_type <- get_type(x, v, "elic")
    var_type <- get_type(x, v, "var")

    data <- cont_get_data(x, round = round, var = v)
    colnames(data) <- gsub(paste0(v, "_"), "", colnames(data))

    if (elic_type == "4p") {
      if (sum(weights) == n_experts) {
        weights <- data[, 5, drop = TRUE] / 100
      } else {
        cli::cli_alert_info("Provided weights used instead of confidence \\
                             estimates")
      }
    }

    estimates <- get_est(data, v, n_experts, var_type, elic_type, verbose)

    n_samp <- get_boostrap_n_sample(experts, n_votes, weights)

    for (e in seq_along(experts)) {

      # Prepare data frame for sampled probabilities
      sp <- matrix(0,
                   nrow = n_samp[[e]],
                   ncol = 3,
                   dimnames = list(NULL, c("id", "var", "value"))) |>
        tibble::as_tibble() |>
        dplyr::mutate("id" = experts[e],
                      "var" = v)

      samp <- get_sample(estimates, n_samp, e, elic_type)

      sp[, 3] <- samp
      all_sp[[e]] <- sp
    }

    all_vars[[v]] <- do.call(rbind, all_sp)

  }

  out <- do.call(rbind, all_vars) |>
    tibble::remove_rownames()

  if (verbose) {
    info <- "Data for {.val {vars}} sampled successfully using the \\
             {.val {method}} method."
    cli::cli_alert_success(info)
  }

  # Prepend new class
  class(out) <- c("cont_sample", class(out))

  # Add attribute with the round number
  attr(out, "round") <- round

  out
}

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
get_est <- function(data, v, n_experts, var_type, elic_type, verbose) {

  if (elic_type == "1p") {
    # One point elicitation
    estimates <- dplyr::pull(data,
                             2)

  } else {
    # Three or four points elicitation
    if (elic_type == "4p") {

      # Rescale min and max
      data <- rescale_data(data)
      needs_resc <- any(data[["min"]] < 0) || any(data[["max"]] > 1)

      if (var_type == "p" && needs_resc) {

        data[["min"]] <- pmax(0, pmin(1, data[["min"]]))
        data[["max"]] <- pmax(0, pmin(1, data[["max"]]))
        warn <- "Some values have been constrained to be between {.val {0}} \\
                 and {.val {1}}."
        cli::cli_warn(c("!" = warn))
      }

      if (verbose) {
        cli::cli_alert_success("Rescaled min and max for variable {.val {v}}.")
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
#' @author Sergio Vignali
get_sample <- function(estimates, n_samp, e, elic_type) {

  if (elic_type == "1p") {
    samp <- sample(estimates, n_samp[[e]], replace = TRUE)
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
