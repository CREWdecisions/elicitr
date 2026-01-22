# Sample categorical data

**\[experimental\]**

`cat_sample_data()` samples data based on expert estimates stored in the
[`elic_cat`](https://crewdecisions.github.io/elicitr/reference/elic_cat.md)
object.

## Usage

``` r
cat_sample_data(
  x,
  method,
  topic,
  ...,
  n_votes = 100,
  option = "all",
  verbose = TRUE
)
```

## Arguments

- x:

  an object of class
  [elic_cat](https://crewdecisions.github.io/elicitr/reference/elic_cat.md).

- method:

  character string with the name of the method to sample the data. The
  available methods are: *unweighted* and *weighted*, see Methods below.

- topic:

  character string that indicates the mechanism to which the data
  belongs.

- ...:

  Unused arguments, included only for future extensions of the function.

- n_votes:

  numeric indicating the number of votes to consider.

- option:

  character string with the name of the option or character vector with
  the options that you want to extract from the data. Use `all` for all
  options.

- verbose:

  logical, if TRUE it prints informative messages.

## Value

An [`tibble`](https://tibble.tidyverse.org/reference/tibble.html) with
the sampled data. This object has the additional class `cat_sample` used
to implement the plotting method.

## Methods

Two methods are implemented. These methods are explained in Vernet et
al. (2024), see references below.

- *unweighted*: This method samples data based on the expert estimates
  without accounting for their confidence. Values are sampled from a
  Dirichlet distribution using the expert estimates as parameters. When
  only one estimate is provided, i.e. 100 % for one category, the method
  assigns 100 % to all votes for this category.

- *weighted*: This method samples data based on the expert estimates
  accounting for their confidence. The confidence is used to weight the
  number of votes assigned to each expert. The method samples data from
  a Dirichlet distribution using the expert estimates as parameters.
  When only one estimate is provided, i.e. 100 % for one category, the
  method assigns 100 % to all votes for this category.

## References

Vernet, M., Trask, A.E., Andrews, C.E., Ewen, J.E., Medina, S.,
Moehrenschlager, A. & Canessa, S. (2024) Assessing invasion risks using
EICAT‐based expert elicitation: application to a conservation
translocation. Biological Invasions, 26(8), 2707–2721.
<https://doi.org/10.1007/s10530-024-03341-2>

## See also

Other cat data helpers:
[`cat_add_data()`](https://crewdecisions.github.io/elicitr/reference/cat_add_data.md),
[`cat_get_data()`](https://crewdecisions.github.io/elicitr/reference/cat_get_data.md),
[`cat_start()`](https://crewdecisions.github.io/elicitr/reference/cat_start.md),
[`summary.cat_sample()`](https://crewdecisions.github.io/elicitr/reference/summary.cat_sample.md)

## Author

Sergio Vignali and Maude Vernet

## Examples

``` r
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
#> ✔ <elic_cat> object for "Elicitation" correctly initialised
#> ℹ Estimates sum to 1. Rescaling to 100.
#> ✔ Data added to Topic "topic_1" from "data.frame"
#> ℹ Estimates sum to 1. Rescaling to 100.
#> ✔ Data added to Topic "topic_2" from "data.frame"
#> ℹ Estimates sum to 1. Rescaling to 100.
#> ✔ Data added to Topic "topic_3" from "data.frame"

# Sample data from Topic 1 for all options using the unweighted method
samp <- cat_sample_data(my_elicit,
                        method = "unweighted",
                        topic = "topic_1")
#> ✔ Data sampled successfully using "unweighted" method.

# Sample data from Topic 2 for option 1 and 3 using the weighted method
samp <- cat_sample_data(my_elicit,
                        method = "weighted",
                        topic = "topic_2",
                        option = c("option_1", "option_3"))
#> ✔ Data sampled successfully using "weighted" method.
```
