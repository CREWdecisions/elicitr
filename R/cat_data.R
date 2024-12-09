#' Elicitation data for categorical variables
#'
#' Simulated data for two mechanisms of impact investigated in an elicitation
#' process.
#'
#' @format A data frame with 120 rows and 5 columns:
#' \describe{
#'   \item{expert}{Name of the experts (randomly generated).}
#'   \item{level}{The name of the levels. There are 5 different levels per
#'         site.}
#'   \item{site}{The name of the sites. There are 4 different sites}
#'   \item{confidence}{The confidence of the experts given in percentages.
#'         One confidence level is given for each site.}
#'   \item{estimate}{Expert estimates given in probabilities.}
#' }
#' @source Randomly generated numbers and names.
#' @name cat_data
NULL

#' Elicitation data for Round 1
#' @rdname cat_data
"mechanism_1"

#' Elicitation data for Round 2
#' @rdname cat_data
"mechanism_2"
