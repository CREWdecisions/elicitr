#' Elicitation data for continuous variables
#'
#' Simulated data for the first and second round of an elicitation process
#' estimating three variables with three different elicitation methods.
#'
#' @format A data frame with 6 rows and 9 columns:
#' \describe{
#'   \item{name}{Name of the expert (randomly generater).}
#'   \item{var1_best}{Best estimate of `var1`. The estimate contains integer
#'         numbers referring to the one point elicitation method.}
#'   \item{var2_min, var2_max, var2_best}{Minimum, maximum, and best estimates
#'         of `var2`. The estimates contain positive integer numbers referring
#'         to the three points elicitation method.}
#'   \item{var3_min, var3_max, var3_best, var3_conf}{Minimum, maximum, best, and
#'         confidence estimates of `var3`. The estimates contain probabilities
#'         referring to the four points elicitation method.}
#' }
#' @source Randomly generated numbers and names.
#' @name cont_data
NULL

#' Elicitation data for Round 1
#' @rdname cont_data
"round_1"

#' Elicitation data for Round 2
#' @rdname cont_data
"round_2"
