#' Elicitation data for categorical variables
#'
#' Simulated data for three topics investigated in an elicitation process.
#'
#' @format A data frame with:
#' \describe{
#'   \item{expert}{Name of the experts (randomly generated).}
#'   \item{level}{The name of the levels. There are 5 different levels per
#'         site.}
#'   \item{site}{The name of the sites. Topic 1 and 2 have 4 sites, while
#'         topic 3 has 3 sites.}
#'   \item{confidence}{The confidence of the experts given in percentages.
#'         One confidence level is given for each site.}
#'   \item{estimate}{Expert estimates given in probabilities.}
#' }
#' @source Randomly generated numbers and names.
#' @name cat_data
NULL

#' Elicitation data for topic 1
#' @rdname cat_data
"topic_1"

#' Elicitation data for topic 2
#' @rdname cat_data
"topic_2"

#' Elicitation data for topic 3
#' @rdname cat_data
"topic_3"
