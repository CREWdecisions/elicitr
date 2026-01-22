# \#' Get data

**\[experimental\]**

Get data from an
[elic_cat](https://crewdecisions.github.io/elicitr/reference/elic_cat.md)
object.

## Usage

``` r
cat_get_data(x, topic, ..., option = "all")
```

## Arguments

- x:

  an object of class
  [elic_cat](https://crewdecisions.github.io/elicitr/reference/elic_cat.md).

- topic:

  character string that indicates the mechanism to which the data
  belongs.

- ...:

  Unused arguments, included only for future extensions of the function.

- option:

  character string with the name of the option or character vector with
  the options that you want to extract from the data. Use `all` for all
  options.

## Value

A [`tibble`](https://tibble.tidyverse.org/reference/tibble.html) with
the extracted data.

## See also

Other cat data helpers:
[`cat_add_data()`](https://crewdecisions.github.io/elicitr/reference/cat_add_data.md),
[`cat_sample_data()`](https://crewdecisions.github.io/elicitr/reference/cat_sample_data.md),
[`cat_start()`](https://crewdecisions.github.io/elicitr/reference/cat_start.md),
[`summary.cat_sample()`](https://crewdecisions.github.io/elicitr/reference/summary.cat_sample.md)

## Author

Sergio Vignali and Maude Vernet

## Examples

``` r
# Create the elic_cat object for an elicitation process with three topics,
# four options, five categories and a maximum of six experts per topic
my_topics <- c("topic_1", "topic_2", "topic_3")
my_options <- c("option_1", "option_2", "option_3", "option_4")
my_categories <- c("category_1", "category_2", "category_3",
                   "category_4", "category_5")
my_elicit <- cat_start(topics = my_topics,
                       options = my_options,
                       categories = my_categories,
                       experts = 6) |>
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

# Get all data from Topic 1
cat_get_data(my_elicit, topic = "topic_1")
#> # A tibble: 120 × 5
#>    id      option   category   confidence estimate
#>    <chr>   <chr>    <chr>           <dbl>    <dbl>
#>  1 5ac97e0 option_1 category_1         15        8
#>  2 5ac97e0 option_1 category_2         15        0
#>  3 5ac97e0 option_1 category_3         15       85
#>  4 5ac97e0 option_1 category_4         15        2
#>  5 5ac97e0 option_1 category_5         15        5
#>  6 5ac97e0 option_2 category_1         35        2
#>  7 5ac97e0 option_2 category_2         35       11
#>  8 5ac97e0 option_2 category_3         35       18
#>  9 5ac97e0 option_2 category_4         35        2
#> 10 5ac97e0 option_2 category_5         35       67
#> # ℹ 110 more rows

# Get data by option name----
# Get data for option_1 from Topic 2
cat_get_data(my_elicit, topic = "topic_2", option = "option_1")
#> # A tibble: 25 × 5
#>    id      option   category   confidence estimate
#>    <chr>   <chr>    <chr>           <dbl>    <dbl>
#>  1 e51202e option_1 category_1        100        9
#>  2 e51202e option_1 category_2        100       21
#>  3 e51202e option_1 category_3        100       11
#>  4 e51202e option_1 category_4        100       59
#>  5 e51202e option_1 category_5        100        0
#>  6 e78cbf4 option_1 category_1         75       31
#>  7 e78cbf4 option_1 category_2         75       27
#>  8 e78cbf4 option_1 category_3         75        9
#>  9 e78cbf4 option_1 category_4         75       17
#> 10 e78cbf4 option_1 category_5         75       16
#> # ℹ 15 more rows

# Get data for option_1 and option_3 from Topic 3
cat_get_data(my_elicit,
             topic = "topic_3",
             option = c("option_1", "option_3"))
#> # A tibble: 60 × 5
#>    id      option   category   confidence estimate
#>    <chr>   <chr>    <chr>           <dbl>    <dbl>
#>  1 5ac97e0 option_1 category_1         80        2
#>  2 5ac97e0 option_1 category_2         80        2
#>  3 5ac97e0 option_1 category_3         80        1
#>  4 5ac97e0 option_1 category_4         80       87
#>  5 5ac97e0 option_1 category_5         80        8
#>  6 5ac97e0 option_3 category_1         15       59
#>  7 5ac97e0 option_3 category_2         15        8
#>  8 5ac97e0 option_3 category_3         15       11
#>  9 5ac97e0 option_3 category_4         15        0
#> 10 5ac97e0 option_3 category_5         15       22
#> # ℹ 50 more rows
```
