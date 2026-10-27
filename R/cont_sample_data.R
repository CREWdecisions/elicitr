#' Sample continuous data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' `cont_sample_data()` samples data based on expert estimates stored in the
#' [`elic_cont`] object.
#'
#' @inheritParams cont_get_data
#' @param scale_conf numeric, the scale factor for the confidence interval.
#' @param method character string with the name of the method to sample the
#' data, only the _PERT_ Method is implemented, see Method below.
#' @param n_votes numeric indicating the number of votes to consider.
#' @param weights numeric vector with the weights to apply to the estimates. If
#' equal to `1`, each experts get `n_votes` votes, see Weights below.
#' @param verbose logical, if TRUE it prints informative messages.
#'
#' @section Weights:
#'
#' To provide a different number of votes to each expert use the `weights`
#' argument. The length of the vector must be equal to the number of
#' experts or have a length of one. Default is NULL, when no weight is provided.
#'
#' If provided when the elicitation type is a _four points elicitation_, the
#' weight values overwrite the confidence estimates.
#'
#' @section Method:
#'
#' The function samples the data differently depending on the elicitation type:
#'
#' * _one point elicitation_: the estimate provided by each expert is repeated
#' `n_votes` number of times. The `weights` argument can be used to weight the
#' estimates of each expert (give a certain number of vote to each expert) in
#' the overall distribution.
#'
#' * _three points elicitation_: the minimum, best, and maximum estimates of
#' each expert are used as scaling parameters of the PERT distribution from
#' which the data are sampled. The `weights` argument can be used to weight the
#' estimates of each expert (give a certain number of vote to each expert) in
#' the overall distribution.
#'
#' * _four points elicitation_: the minimum, best, and maximum estimates of
#' each expert are rescaled according to their confidence and used as scaling
#' parameters of the PERT distribution from which the data are sampled.
#' Furthermore, their confidence is used as the `weights` argument to weight the
#' estimates of each expert (give a number of vote to each expert) in
#' the overall distribution.
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
#' For probability variables (`var_types = "p"`), confidence scaling may
#' extend the estimated minimum and maximum value below zero or above one.
#' To keep the interval within the valid probability range, any scaled minimum
#' below zero is set to zero, and any scaled maximum above one is set to one.
#' A warning is issued when this adjustment is needed.
#'
#' The best estimate and the original responses stored in the elicitation object
#' remain unchanged. The adjusted bounds are used to define the PERT
#' distribution when sampling and the displayed intervals when plotting.
#'
#' @details
#' Weighted allocations distribute the full budget among answering experts.
#' Thus, the number of row in the sampled data will n_experts * n_votes. If
#' an expert doesn't provide data for the sampled variable, a placeholder row is
#' added. In such case, row counts can exceed n_experts * n_votes.
#'
#' @returns A [`tibble`][tibble::tibble] with the sampled data. This object has
#' an additional class `cont_sample` used to implement the plotting method.
#' @export
#'
#' @family cont data helpers
#'
#' @author Sergio Vignali and Maude Vernet
#'
#' @examples
#' # Create the elict object and add data for the first and second round from a
#' # data.frame.
#' my_elicit <- cont_start(var_names = c("var1", "var2", "var3"),
#'                         var_types = "ZNp",
#'                         elic_types = "134",
#'                         experts = 6) |>
#'   cont_add_data(data_source = round_1, round = 1) |>
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
#' samp <- cont_sample_data(my_elicit, round = 2, var = "var3")
#'
#' # Sample data for the first round for the variable `var3` providing the
#' # weights. Notice that the weights overwrite the confidence estimates and
#' # that some values are constrained to be between 0 and 1 after rescaling.
#' samp <- cont_sample_data(my_elicit, round = 1, var = "var3",
#'                          weights = c(0.8, 0.7, 0.9, 0.7, 0.6, 0.9))
cont_sample_data <- function(x,
                             round,
                             ...,
                             method = "PERT",
                             var = "all",
                             n_votes = 1000,
                             weights = NULL,
                             scale_conf = 100,
                             verbose = TRUE) {

  # # Check if the object is of class elic_cont
  check_elic_obj(x, type = "cont", metadata = FALSE)

  # Check round
  check_round(round)

  # Check if method is available
  check_method(x, method)

  # Check n_votes
  check_n_votes(n_votes)

  if (length(var) == 1 && var == "all") {
    vars <- x[["var_names"]]
  } else {
    # Check if var is available in the object
    check_var(x, var)
    vars <- var
  }

  #remove placeholders with only NAs (might have been added in cat_add_data)
  x[["data"]][[round]] <- remove_placeholders(x, round)

  experts <- x[["data"]][[round]][["id"]]
  n_experts <- length(experts)
  all_sp <- vector(mode = "list", length = n_experts)
  all_vars <- vector(mode = "list", length = length(vars))

  # Check weights argument
  if (!is.null(weights)) {
    if (length(weights) == 1) {
      weights <- rep(weights, n_experts)
    }

    check_weights(weights, n_experts)
  }

  for (v in vars) {

    elic_type <- get_type(x, v, "elic")
    var_type <- get_type(x, v, "var")

    data <- cont_get_data(x, round = round, var = v)
    prefix <- paste0(v, "_")
    cols <- names(data)

    idx <- startsWith(cols, prefix) & cols != "id"
    cols[idx] <- substring(cols[idx], nchar(prefix) + 1L)

    names(data) <- cols

    n_samp <- get_boostrap_n_sample(experts, n_votes, weights,
                                    elic_type, data)
    if (elic_type == "4p" && !is.null(weights)) {
      cli::cli_alert_info("Weights provided in argument {.arg weights} used \\
                           instead of confidence estimates to calculate the \\
                           number of vote attributed to each expert in \\
                           variable {.val {v}}.")
    }
    estimates <- get_est(data, v, n_experts, var_type,
                         elic_type, verbose, scale_conf)

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
