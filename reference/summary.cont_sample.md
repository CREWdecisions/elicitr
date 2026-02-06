# Summarise samples of continuous data

**\[experimental\]**

[`summary()`](https://rdrr.io/r/base/summary.html) summarises the
sampled data and provides the minimum, first quartile, median, mean,
third quartile, and maximum values for each variable.

## Usage

``` r
# S3 method for class 'cont_sample'
summary(object, ..., var = "all")
```

## Arguments

- object:

  an object of class `cont_sample` created by the function
  [cont_sample_data](https://crewdecisions.github.io/elicitr/reference/cont_sample_data.md).

- ...:

  Unused arguments, included only for future extensions of the function.

- var:

  character vector with the names of the variables to summarise. If
  `var = "all"`, all variables are summarised.

## Value

A [`tibble`](https://tibble.tidyverse.org/reference/tibble.html) with
the summary statistics.

## See also

Other cont data helpers:
[`cont_add_data()`](https://crewdecisions.github.io/elicitr/reference/cont_add_data.md),
[`cont_get_data()`](https://crewdecisions.github.io/elicitr/reference/cont_get_data.md),
[`cont_sample_data()`](https://crewdecisions.github.io/elicitr/reference/cont_sample_data.md),
[`cont_start()`](https://crewdecisions.github.io/elicitr/reference/cont_start.md)

## Author

Sergio Vignali

## Examples

``` r
# Create the elict object and add data for the first and second round from a
# data.frame.
my_elicit <- cont_start(var_names = c("var1", "var2", "var3"),
                        var_types = "ZNp",
                        elic_types = "134",
                        experts = 6) |>
  cont_add_data(x, data_source = round_1, round = 1) |>
  cont_add_data(data_source = round_2, round = 2)
#> ✔ <elic_cont> object for "Elicitation" correctly initialised
#> ✔ Data added to "Round 1" from "data.frame"
#> ✔ Data added to "Round 2" from "data.frame"

# Sample data for the second round for all variables
samp <- cont_sample_data(my_elicit, round = 2)
#> ✔ Rescaled min and max for variable "var3".
#> ✔ Data for "var1", "var2", and "var3" sampled successfully using the "basic" method.

# Summarise the sampled data for all variables
summary(samp)
#> # A tibble: 3 × 7
#>   Var      Min     Q1 Median   Mean     Q3    Max
#>   <chr>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1 var1  -4     -2      0     -0.971  1      1    
#> 2 var2  11.1   14.5   16.3   16.3   18.4   22.9  
#> 3 var3   0.501  0.670  0.717  0.715  0.761  0.872

# Summarise the sampled data for the variable "var1"
summary(samp, var = "var1")
#> # A tibble: 1 × 7
#>   Var     Min    Q1 Median   Mean    Q3   Max
#>   <chr> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>
#> 1 var1     -4    -2      0 -0.971     1     1

# Summarise the sampled data for the variables "var1" and "var3"
summary(samp, var = c("var1", "var3"))
#> # A tibble: 2 × 7
#>   Var      Min     Q1 Median   Mean    Q3   Max
#>   <chr>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>
#> 1 var1  -4     -2      0     -0.971 1     1    
#> 2 var3   0.501  0.670  0.717  0.715 0.761 0.872
```
