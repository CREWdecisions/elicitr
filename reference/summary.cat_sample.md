# Summarise samples of categorical data

**\[experimental\]**

[`summary()`](https://rdrr.io/r/base/summary.html) summarises the
sampled data and provides the minimum, first quartile, median, mean,
third quartile, and maximum values for each category.

## Usage

``` r
# S3 method for class 'cat_sample'
summary(object, option = "all", ...)
```

## Arguments

- object:

  an object of class `cat_sample` created by the function
  [cat_sample_data](https://crewdecisions.github.io/elicitr/reference/cat_sample_data.md).

- option:

  character string with the name of the option(s). If `option = "all"`,
  all options are summarised.

- ...:

  Unused arguments, included only for future extensions of the function.

## Value

A [table](https://rdrr.io/r/base/table.html) with the summary
statistics.

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
#> $option_1
#> # A tibble: 5 × 7
#>   category       Min     Q1 Median  Mean    Q3   Max
#>   <chr>        <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>
#> 1 category_1 0.0691  0.168   0.375 0.383 0.570 0.790
#> 2 category_2 0       0.0624  0.116 0.121 0.184 0.322
#> 3 category_3 0.00192 0.0448  0.104 0.130 0.189 0.504
#> 4 category_4 0.00158 0.0508  0.232 0.221 0.317 0.579
#> 5 category_5 0.0152  0.0907  0.145 0.144 0.192 0.346
#> 
```
