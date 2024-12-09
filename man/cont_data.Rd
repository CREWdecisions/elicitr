% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cont_data.R
\docType{data}
\name{cont_data}
\alias{cont_data}
\alias{round_1}
\alias{round_2}
\title{Elicitation data for continuous variables}
\format{
A data frame with 6 rows and 9 columns:
\describe{
\item{name}{Name of the experts (randomly generated).}
\item{var1_best}{Best estimate of \code{var1}. The estimate contains integer
numbers referring to the one point elicitation method.}
\item{var2_min, var2_max, var2_best}{Minimum, maximum, and best estimates
of \code{var2}. The estimates contain positive integer numbers referring
to the three points elicitation method.}
\item{var3_min, var3_max, var3_best, var3_conf}{Minimum, maximum, best, and
confidence estimates of \code{var3}. The estimates contain probabilities
referring to the four points elicitation method.}
}

An object of class \code{tbl_df} (inherits from \code{tbl}, \code{data.frame}) with 6 rows and 9 columns.

An object of class \code{tbl_df} (inherits from \code{tbl}, \code{data.frame}) with 6 rows and 9 columns.
}
\source{
Randomly generated numbers and names.
}
\usage{
round_1

round_2
}
\description{
Simulated data for the first and second round of an elicitation process
estimating three variables with three different elicitation types.
}
\keyword{datasets}
