% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cat_add_data.R
\name{cat_add_data}
\alias{cat_add_data}
\title{Add data}
\usage{
cat_add_data(
  x,
  data_source,
  mechanism,
  ...,
  sep = ",",
  sheet = 1,
  overwrite = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{x}{an object of class \link{elic_cat}.}

\item{data_source}{either a \code{\link[base:data.frame]{data.frame}} or
\code{\link[tibble:tibble]{tibble}}, a string with the path to a \emph{csv} or \emph{xlsx} file,
or anything accepted by the \link[googlesheets4:range_read]{read_sheet} function.}

\item{mechanism}{character string that indicates the machanism to which the
data belongs.}

\item{...}{Unused arguments, included only for future extensions of the
function.}

\item{sep}{character used as field separator, used only when \code{data_source} is
a path to a \emph{csv} file.}

\item{sheet}{integer or character to select the sheet. The sheet can be
referenced by its position with a number or by its name with a string. Used
only when \code{data_source} is a path to a \emph{xlsx} file or when data are imported
from \emph{Google Sheets}.}

\item{overwrite}{logical, whether to overwrite existing data already added to
the \link{elic_cont} object.}

\item{verbose}{logical, if \code{TRUE} it prints informative messages.}
}
\value{
The provided object of class \link{elic_cat} updated with the data.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

\code{cat_add_data()} adds data to an \link{elic_cat} object from different sources.
}
\section{Data format}{


For each mechanism, data are expected to have five columns, built as follows:
\itemize{
\item The first column of the data should hold the names of the experts. The name
of each expert should be repeated as many times as many times as the number
of impact levels and sites.
(i.e. each expert should appear \eqn{number\ of\ levels
\cdot number\ of\ sites} times).
\item The second column should be the names of the levels of impact considered in
the elicitation. Each block of levels of impact should be repeated as many
times as the number of sites investigated/considered.
\item The third column should hold the names of the sites considered in the
study. The name of each site should be repeated as many times as the number
of levels of impact considered.
\item The fourth column should be the experts confidence in their own estimates
(given in percent). Experts should estimate how confident they are in their
estimates for each block of levels of impact and for each site. Therefore,
expert confidence estimates should be repeated as many times as the number of
levels of impact considered.
\item The final column should be the estimates of each expert for each site and
impact level.
}

The name of the columns is not important, \code{cat_add_data()} will overwrite
them according to the following convention:

The first column will be renamed \code{id}, the second column \code{level}, the third
column \code{site}, the fourth column \code{confidence}, and the fifth column
\code{estimate}.

Here is an example of data correctly formatted for an elicitation with two
levels of impact and two sites (only one expert is shown):

\if{html}{\out{<div class="sourceCode">}}\preformatted{name       level       site      confidence      estimate
---------------------------------------------------------
expert 1   level 1     site 1            15          0.08
expert 1   level 2     site 1            15             0
expert 1   level 3     site 1            15          0.84
expert 1   level 4     site 1            15          0.02
expert 1   level 5     site 1            15          0.06
expert 1   level 1     site 2            35          0.02
expert 1   level 2     site 2            35          0.11
expert 1   level 3     site 2            35          0.19
expert 1   level 4     site 2            35          0.02
expert 1   level 5     site 2            35          0.66
}\if{html}{\out{</div>}}
}

\section{Data cleaning}{


When data are added to the \link{elic_cat} object, first names are standardised
by converting capital letters to lower case, and by removing any whitespaces
and punctuation. Then, data are anonymised by converting names to short sha1
hashes. In this way, sensible information collected during the elicitation
process never reaches the \link{elic_cat} object.
}

\examples{
# Create the elic_cat object for an elicitation process with three
# mechanisms, four sites, five levels and a maximum of six experts per
# mechanism
my_levels <- c("level_1", "level_2", "level_3", "level_4", "level_5")
my_sites <- c("site_1", "site_2", "site_3", "site_4")
my_mechanisms <- c("mechanism_1", "mechanism_2", "mechanism_3")
x <- cat_start(levels = my_levels,
               sites = my_sites,
               experts = 6,
               mechanisms = my_mechanisms)

# Add data for the three mechanisms from a data.frame. Notice that the
# three commands can be piped
my_elicit <- cat_add_data(x,
                          data_source = mechanism_1,
                          mechanism = "mechanism_1") |>
  cat_add_data(data_source = mechanism_2, mechanism = "mechanism_2") |>
  cat_add_data(data_source = mechanism_3, mechanism = "mechanism_3")
my_elicit

# Add data for the first and second round from a csv file
files <- list.files(path = system.file("extdata", package = "elicitr"),
                    pattern = "mechanism_",
                    full.names = TRUE)
my_elicit <- cat_add_data(x,
                          data_source = files[1],
                          mechanism = "mechanism_1") |>
  cat_add_data(data_source = files[2], mechanism = "mechanism_2") |>
  cat_add_data(data_source = files[3], mechanism = "mechanism_3")
my_elicit

# Add data for the first and second round from a xlsx file with three sheets
file <- list.files(path = system.file("extdata", package = "elicitr"),
                   pattern = "mechanisms",
                   full.names = TRUE)
# Using the sheet index
my_elicit <- cat_add_data(x,
                          data_source = file,
                          sheet = 1,
                          mechanism = "mechanism_1") |>
  cat_add_data(data_source = file,
               sheet = 2,
               mechanism = "mechanism_2") |>
  cat_add_data(data_source = file,
               sheet = 3,
               mechanism = "mechanism_3")
my_elicit
# Using the sheet name
my_elicit <- cat_add_data(x,
                          data_source = file,
                          sheet = "Mechanism 1",
                          mechanism = "mechanism_1") |>
  cat_add_data(data_source = file,
               sheet = "Mechanism 2",
               mechanism = "mechanism_2") |>
  cat_add_data(data_source = file,
               sheet = "Mechanism 3",
               mechanism = "mechanism_3")
my_elicit

\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# Add data for the first and second round from Google Sheets
googlesheets4::gs4_deauth()
gs <- "18VHeHB89P1s-6banaVoqOP-ggFmQZYx-z_31nMffAb8"
# Using the sheet index
my_elicit <- cat_add_data(x,
                          data_source = gs,
                          sheet = 1,
                          mechanism = "mechanism_1") |>
  cat_add_data(data_source = gs,
               sheet = 2,
               mechanism = "mechanism_2") |>
  cat_add_data(data_source = gs,
               sheet = 3,
               mechanism = "mechanism_3")
my_elicit
# Using the sheet name
my_elicit <- cat_add_data(x, data_source = gs,
                          sheet = "Mechanism 1",
                          mechanism = "mechanism_1") |>
  cat_add_data(data_source = gs,
               sheet = "Mechanism 2",
               mechanism = "mechanism_2") |>
  cat_add_data(data_source = gs,
               sheet = "Mechanism 3",
               mechanism = "mechanism_3")
my_elicit
\dontshow{\}) # examplesIf}
}
\seealso{
Other cat data helpers: 
\code{\link{cat_get_data}()},
\code{\link{cat_sample_data}()},
\code{\link{cat_start}()}
}
\author{
Sergio Vignali and Maude Vernet
}
\concept{cat data helpers}
