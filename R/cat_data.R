#' Elicitation data for categorical variables
#'
#' Simulated data for two mechanisms of impact investigated in an elicitation
#' process.
#'
#' @format A data frame with:
#' \describe{
#'   \item{expert}{Name of the experts (randomly generated).}
#'   \item{level}{The name of the levels. There are 5 different levels per
#'         site.}
#'   \item{site}{The name of the sites. Mechanism 1 and 2 have 4 sites, while
#'         mechanism 3 has 3 sites.}
#'   \item{confidence}{The confidence of the experts given in percentages.
#'         One confidence level is given for each site.}
#'   \item{estimate}{Expert estimates given in probabilities.}
#' }
#' @source Randomly generated numbers and names.
#' @name cat_data
NULL

#' Elicitation data for mechanism 1
#' @rdname cat_data
"mechanism_1"

#' Elicitation data for mechanism 2
#' @rdname cat_data
"mechanism_2"

#' Elicitation data for mechanism 3
#' @rdname cat_data
"mechanism_3"
