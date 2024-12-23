% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cat_sample_data.R
\name{cat_sample_data}
\alias{cat_sample_data}
\title{Sample categorical data}
\usage{
cat_sample_data(
  x,
  method,
  topic,
  ...,
  n_votes = 100,
  option = "all",
  verbose = TRUE
)
}
\arguments{
\item{x}{an object of class \link{elic_cat}.}

\item{method}{character string with the name of the method to sample the
data. The available methods are: \emph{basic} and \emph{bootstrap}, see Methods below.}

\item{topic}{character string that indicates the mechanism to which the data
belongs.}

\item{...}{Unused arguments, included only for future extensions of the
function.}

\item{n_votes}{numeric indicating the number of votes to consider.}

\item{option}{character string with the name of the option or character
vector with the options that you want to extract from the data. Use \code{all} for
all options.}

\item{verbose}{logical, if TRUE it prints informative messages.}
}
\value{
An \code{\link[tibble:tibble]{tibble}} with the sampled data. This object has
the additional class \code{cat_sample} used to implement the plotting method.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

\code{cat_sample_data()} samples data based on expert estimates stored in the
\code{\link{elic_cat}} object.
}
\section{Methods}{

Two methods are implemented. These methods are explained in Vernet et al.
(2024), see references below.
\itemize{
\item \emph{basic}: This method samples data based on the expert estimates without
accounting for their confidence. Values are sampled from a Dirichlet
distribution using the expert estimates as parameters. When only one estimate
is provided, i.e. 100 \% for one category, the method assigns 100 \% to all
votes for this category.
\item \emph{bootstrap}: This method samples data based on the expert estimates
accounting for their confidence. The confidence is used to weight the
number of votes assigned to each expert. The method samples data from a
Dirichlet distribution using the expert estimates as parameters. When only
one estimate is provided, i.e. 100 \% for one category, the method assigns 100
\% to all votes for this category.
}
}

\examples{
# Create the elic_cat object for an elicitation process with three topics,
# four options, five categories and a maximum of six experts per topic
my_categories <- c("category_1", "category_2", "category_3",
                   "category_4", "category_5")
my_options <- c("option_1", "option_2", "option_3", "option_4")
my_topics <- c("topic_1", "topic_2", "topic_3")
my_elicit <- cat_start(categories = my_categories,
                       options = my_options,
                       experts = 6,
                       topics = my_topics) |>
  cat_add_data(data_source = topic_1, topic = "topic_1") |>
  cat_add_data(data_source = topic_2, topic = "topic_2") |>
  cat_add_data(data_source = topic_3, topic = "topic_3")

# Sample data from Topic 1 for all options using the basic method
samp <- cat_sample_data(my_elicit,
                        method = "basic",
                        topic = "topic_1")

# Sample data from Topic 2 for option 1 and 3 using the bootstrap method
samp <- cat_sample_data(my_elicit,
                        method = "bootstrap",
                        topic = "topic_2",
                        option = c("option_1", "option_3"))
}
\references{
Vernet, M., Trask, A.E., Andrews, C.E., Ewen, J.E., Medina, S.,
Moehrenschlager, A. & Canessa, S. (2024) Assessing invasion risks using
EICAT‐based expert elicitation: application to a conservation translocation.
Biological Invasions, 26(8), 2707–2721.
\url{https://doi.org/10.1007/s10530-024-03341-2}
}
\seealso{
Other cat data helpers: 
\code{\link{cat_add_data}()},
\code{\link{cat_get_data}()},
\code{\link{cat_start}()},
\code{\link{summary.cat_sample}()}
}
\author{
Sergio Vignali and Maude Vernet
}
\concept{cat data helpers}
