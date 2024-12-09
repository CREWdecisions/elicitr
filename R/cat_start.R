#' Start elicitation
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `elic_cat_start()` initialises an `elic_cat` object which stores important
#' metadata for the data collected during the elicitation process of categorical
#' data.
#'
#' @param levels character vector with the impact levels related to the topics.
#' @param sites character vector with the name of the sites for which the impact
#' of the topics is estimated.
#' @param mechanisms character vector with the name of the mechanisms to be
#' estimated.
#' @inheritParams elic_cont_start
#'
#' @return An object of class `elic_cat` binding metadata related to the
#' elicitation process. These metadata are used by other functions to validate
#' the correctness of the provided data.
#'
#' @export
elic_cat_start <- function(levels,
                           sites,
                           experts,
                           mechanisms,
                           ...,
                           title = "Elicitation",
                           verbose = TRUE) {

  # Check that the argument `experts` is a number
  check_experts_arg(experts)

  obj <- new_elic_cat(levels = levels,
                      sites = sites,
                      experts = experts,
                      mechanisms = mechanisms,
                      title)

  if (verbose) {
    cli::cli_alert_success("{.cls elic_cat} object for {.val {title}} \\
                            correctly initialised")
  }

  obj
}
