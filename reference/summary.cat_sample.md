# Summarise samples of categorical data

**\[experimental\]**

[`summary()`](https://rdrr.io/r/base/summary.html) summarises the
sampled data and provides the minimum, first quartile, median, mean,
third quartile, and maximum values for each category.

## Usage

``` r
# S3 method for class 'cat_sample'
summary(object, option, ...)
```

## Arguments

- object:

  an object of class `cat_sample` created by the function
  [cat_sample_data](https://crewdecisions.github.io/elicitr/reference/cat_sample_data.md).

- option:

  character string with the name of the option.

- ...:

  Unused arguments, included only for future extensions of the function.

## Value

A [`tibble`](https://tibble.tidyverse.org/reference/tibble.html) with
the summary statistics.

## See also

Other cat data helpers:
[`cat_add_data()`](https://crewdecisions.github.io/elicitr/reference/cat_add_data.md),
[`cat_get_data()`](https://crewdecisions.github.io/elicitr/reference/cat_get_data.md),
[`cat_sample_data()`](https://crewdecisions.github.io/elicitr/reference/cat_sample_data.md),
[`cat_start()`](https://crewdecisions.github.io/elicitr/reference/cat_start.md)

## Author

Sergio Vignali

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

# Summarise the sampled data
summary(samp, option = "option_1")
#> # A tibble: 5 × 7
#>   Category         Min      Q1 Median   Mean     Q3   Max
#>   <chr>          <dbl>   <dbl>  <dbl>  <dbl>  <dbl> <dbl>
#> 1 category_1 0.0195    0.0851  0.124  0.128  0.168  0.328
#> 2 category_2 0         0.00189 0.0105 0.0547 0.0894 0.317
#> 3 category_3 0.0324    0.138   0.245  0.357  0.575  0.929
#> 4 category_4 0.0000726 0.00972 0.0987 0.154  0.293  0.456
#> 5 category_5 0.00392   0.0567  0.338  0.306  0.477  0.771
```
