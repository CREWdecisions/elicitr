% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cat_data.R
\docType{data}
\name{cat_data}
\alias{cat_data}
\alias{topic_1}
\alias{topic_2}
\alias{topic_3}
\title{Elicitation data for categorical variables}
\format{
A data frame with:
\describe{
\item{expert}{Name of the experts (randomly generated).}
\item{category}{The name of the categories. There are 5 different
categories per option.}
\item{option}{The name of the options. Topic 1 and 2 have 4 options, while
topic 3 has 3 options.}
\item{confidence}{The confidence of the experts given in percentages.
One confidence value is given for each option.}
\item{estimate}{Expert estimates given in probabilities.}
}

An object of class \code{tbl_df} (inherits from \code{tbl}, \code{data.frame}) with 120 rows and 5 columns.

An object of class \code{tbl_df} (inherits from \code{tbl}, \code{data.frame}) with 100 rows and 5 columns.

An object of class \code{tbl_df} (inherits from \code{tbl}, \code{data.frame}) with 90 rows and 5 columns.
}
\source{
Randomly generated numbers and names.
}
\usage{
topic_1

topic_2

topic_3
}
\description{
Simulated data for three topics investigated in an elicitation process.
}
\keyword{datasets}
