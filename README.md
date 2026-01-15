
<!-- README.md is generated from README.Rmd. Please edit the .rmd file and then use devtools::build_readme() to update the md file -->

# elicitr <a href="#"><img src="man/figures/logo.png" height="138" align="right">

<!-- badges: start -->

[![Project
Status](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R-CMD-check](https://github.com/CREWdecisions/elicitr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CREWdecisions/elicitr/actions/workflows/R-CMD-check.yaml)
[![lint](https://github.com/CREWdecisions/elicitr/actions/workflows/lint.yaml/badge.svg)](https://github.com/CREWdecisions/elicitr/actions/workflows/lint.yaml)
[![Codecov test
coverage](https://codecov.io/gh/CREWdecisions/elicitr/graph/badge.svg)](https://app.codecov.io/gh/CREWdecisions/elicitr)
<!-- badges: end -->

## Description

elicitr is an R package used to standardise, visualise and aggregate
data from expert elicitation.  
The package is in active development and will implement functions based
on two formal elicitation methods:

- Elicitation of continuous variables  
  Adapted from Hemming, V. et al. (2018). A practical guide to
  structured expert elicitation using the IDEA protocol. Methods in
  Ecology and Evolution, 9(1), 169–180.
  <https://doi.org/10.1111/2041-210X.12857>
- Elicitation of categorical data  
  Adapted from Vernet, M. et al. (2024). Assessing invasion risks using
  EICAT-based expert elicitation: application to a conservation
  translocation. Biological Invasions, 26(8), 2707–2721.
  <https://doi.org/10.1007/s10530-024-03341-2>

## Installation

You can install the development version of elicitr from GitHub with:

``` r
# install.packages("pak")
pak::pak("CREWdecisions/elicitr")
```

## How elicitr works

Just as one creates a form to collect estimates in an elicitation
process, with elicitr one creates an object to store metadata
information. This allows to check whether experts have given their
answers in the expected way.  
All the functions in the elicitr package start with two prefixes: `cont`
and `cat`. This design choice is intended to enhance functions
discovery. `cont` functions are used for the elicitation of continuous
variables while `cat` functions for the elicitation of categorical
variables.

## Getting started

``` r
library(elicitr)
```

### Elicitation of continuous variables

Create the metadata object that will be able to hold the continuous data
based on the elicitation design:

``` r
my_elic_cont <- cont_start(var_names = c("var1", "var2", "var3"),
                           var_types = "ZNp",
                           elic_types = "134",
                           experts = 6)
#> ✔ <elic_cont> object for "Elicitation" correctly initialised

my_elic_cont
#> 
#> ── Elicitation ──
#> 
#> • Variables: "var1", "var2", and "var3"
#> • Variable types: "Z", "N", and "p"
#> • Elicitation types: "1p", "3p", and "4p"
#> • Number of experts: 6
#> • Number of rounds: 0
```

Load the continuous data into the metadata object (round_1 and round_2
data are provided as example datasets in the package):

``` r
my_elic_cont <- cont_add_data(my_elic_cont,
                              data_source = round_1,
                              round = 1)
#> ✔ Data added to "Round 1" from "data.frame"
my_elic_cont <- cont_add_data(my_elic_cont,
                              data_source = round_2,
                              round = 2)
#> ✔ Data added to "Round 2" from "data.frame"
my_elic_cont
#> 
#> ── Elicitation ──
#> 
#> • Variables: "var1", "var2", and "var3"
#> • Variable types: "Z", "N", and "p"
#> • Elicitation types: "1p", "3p", and "4p"
#> • Number of experts: 6
#> • Number of rounds: 2
```

View the data stored in the elicitation object:

``` r
cont_get_data(my_elic_cont, round = 1)
#> # A tibble: 6 × 9
#>   id      var1_best var2_min var2_max var2_best var3_min var3_max var3_best
#>   <chr>       <int>    <int>    <int>     <int>    <dbl>    <dbl>     <dbl>
#> 1 5ac97e0         1       20       24        22     0.43     0.83      0.73
#> 2 e51202e         0        7       10         9     0.67     0.87      0.77
#> 3 e78cbf4         0       10       15        12     0.65     0.95      0.85
#> 4 9fafbee        -7        4       12         9     0.44     0.84      0.64
#> 5 3cc9c29        -5       13       18        16     0.38     0.88      0.68
#> 6 3d32ab9         3       20       26        25     0.35     0.85      0.65
#> # ℹ 1 more variable: var3_conf <int>
```

Plot raw data for variable 2 in round 1:

``` r
plot(my_elic_cont, round = 1, var = "var2")
#> `height` was translated to `width`.
```

<img src="man/figures/README-plot-raw-data-1.png" width="100%" />

When the elicitation process is part of a workshop and is used for
demonstration, it can be useful to show a truth argumenton the plot.
This argument can be added as a list of estimates.

``` r
plot(my_elic_cont, round = 1, var = "var2",
     truth = list(min = 10, max = 20, best = 15))
#> `height` was translated to `width`.
```

<img src="man/figures/README-plot-truth-1.png" width="100%" />

Estimates can also be plotted grouped across experts:

``` r
plot(my_elic_cont, round = 1, var = "var2",
     truth = list(min = 10, max = 20, best = 15),
     group = TRUE)
#> `height` was translated to `width`.
```

<img src="man/figures/README-plot-group-1.png" width="100%" />

Data can be sampled from the elicitation object:

``` r
samp_cont <- cont_sample_data(my_elic_cont, round = 2)
#> ✔ Rescaled min and max for variable "var3".
#> ✔ Data for "var1", "var2", and "var3" sampled successfully using the "basic" method.

samp_cont
#> # A tibble: 18,000 × 3
#>    id      var   value
#>    <chr>   <chr> <dbl>
#>  1 5ac97e0 var1     -2
#>  2 5ac97e0 var1     -2
#>  3 5ac97e0 var1      0
#>  4 5ac97e0 var1      1
#>  5 5ac97e0 var1      1
#>  6 5ac97e0 var1      0
#>  7 5ac97e0 var1     -2
#>  8 5ac97e0 var1      1
#>  9 5ac97e0 var1      1
#> 10 5ac97e0 var1      1
#> # ℹ 17,990 more rows
```

And the sample summarised:

``` r
summary(samp_cont)
#> # A tibble: 3 × 7
#>   Var      Min     Q1 Median   Mean     Q3    Max
#>   <chr>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1 var1  -4     -2     -2     -1.04   1      1    
#> 2 var2  11.0   14.4   16.3   16.3   18.3   22.6  
#> 3 var3   0.452  0.671  0.716  0.715  0.762  0.872
```

And plotted as violin plots:

``` r
plot(samp_cont, var = "var2", type = "violin")
```

<img src="man/figures/README-sample-plot-violin-1.png" alt="Violin plot of the sampled data for variable 2." width="100%" />

Or plotted as density plots:

``` r
plot(samp_cont, var = "var3", type = "density")
```

<img src="man/figures/README-sample-plot-density-1.png" alt="Density plot of the sampled data for variable 3." width="100%" />

And can be grouped across experts:

``` r
plot(samp_cont, var = "var3", type = "density",
     group = TRUE)
```

<img src="man/figures/README-sample-plot-density-group-1.png" alt="Density plot of the sampled data for variable 3, grouped by experts." width="100%" />

### Elicitation of categorical variables

Create the metadata object that will be able to hold the categorical
data based on the elicitation design: Categories correspond to impact
levels and options to islands in Vernet, M. et al. (2024).

``` r
my_elic_cat <- cat_start(topics = c("Mechanism1",
                                    "Mechanism2",
                                    "Mechanism3"),
                         categories = c("category_1",
                                        "category_2",
                                        "category_3",
                                        "category_4",
                                        "category_5"),
                         options = c("option_1",
                                     "option_2",
                                     "option_3",
                                     "option_4"),
                         experts = 6)
#> ✔ <elic_cat> object for "Elicitation" correctly initialised

my_elic_cat
#> 
#> ── Elicitation ──
#> 
#> • Categories: "category_1", "category_2", "category_3", "category_4", and
#> "category_5"
#> • Options: "option_1", "option_2", "option_3", and "option_4"
#> • Number of experts: 6
#> • Topics: "Mechanism1", "Mechanism2", and "Mechanism3"
#> • Data available for 0 topics
```

Load the categorical data into the metadata object (topic_1, topic_2 and
topic_3 data are provided as example datasets in the package):

``` r
my_elic_cat <- cat_add_data(my_elic_cat,
                            data_source = topic_1,
                            topic = "Mechanism1")
#> ✔ Data added to Topic "Mechanism1" from "data.frame"

my_elic_cat <- cat_add_data(my_elic_cat,
                            data_source = topic_2,
                            topic = "Mechanism2")
#> ✔ Data added to Topic "Mechanism2" from "data.frame"

my_elic_cat <- cat_add_data(my_elic_cat,
                            data_source = topic_3,
                            topic = "Mechanism3")
#> ✔ Data added to Topic "Mechanism3" from "data.frame"

my_elic_cat
#> 
#> ── Elicitation ──
#> 
#> • Categories: "category_1", "category_2", "category_3", "category_4", and
#> "category_5"
#> • Options: "option_1", "option_2", "option_3", and "option_4"
#> • Number of experts: 6
#> • Topics: "Mechanism1", "Mechanism2", and "Mechanism3"
#> • Data available for topics "Mechanism1", "Mechanism2", and "Mechanism3"
```

View the data stored in the elicitation object:

``` r
cat_get_data(my_elic_cat,
             topic = "Mechanism1")
#> # A tibble: 120 × 5
#>    id      category   option   confidence estimate
#>    <chr>   <chr>      <chr>         <dbl>    <dbl>
#>  1 5ac97e0 category_1 option_1         15     0.08
#>  2 5ac97e0 category_2 option_1         15     0   
#>  3 5ac97e0 category_3 option_1         15     0.85
#>  4 5ac97e0 category_4 option_1         15     0.02
#>  5 5ac97e0 category_5 option_1         15     0.05
#>  6 5ac97e0 category_1 option_2         35     0.02
#>  7 5ac97e0 category_2 option_2         35     0.11
#>  8 5ac97e0 category_3 option_2         35     0.18
#>  9 5ac97e0 category_4 option_2         35     0.02
#> 10 5ac97e0 category_5 option_2         35     0.67
#> # ℹ 110 more rows
```

Data can be sampled from the elicitation object using the basic or
bootstrap method:

``` r
samp_cat <- cat_sample_data(my_elic_cat,
                            topic = "Mechanism1",
                            method = "basic")
#> ✔ Data sampled successfully using "basic" method.
samp_cat
#> # A tibble: 2,400 × 7
#>    id      option   category_1 category_2 category_3 category_4 category_5
#>    <chr>   <chr>         <dbl>      <dbl>      <dbl>      <dbl>      <dbl>
#>  1 5ac97e0 option_1   1.34e-12          0     1.000    1.32e-18   1.54e-12
#>  2 5ac97e0 option_1   3.88e- 1          0     0.612    1.89e- 9   2.98e- 9
#>  3 5ac97e0 option_1   3.53e- 8          0     1.000    1.41e-43   2.56e-18
#>  4 5ac97e0 option_1   5.66e- 4          0     0.999    5.26e-11   7.76e-13
#>  5 5ac97e0 option_1   7.82e-27          0     1.000    1.51e-32   1.62e- 5
#>  6 5ac97e0 option_1   1.13e- 3          0     0.999    1.89e-16   3.19e- 4
#>  7 5ac97e0 option_1   1.24e- 3          0     0.999    1.43e-12   3.33e- 8
#>  8 5ac97e0 option_1   9.42e- 1          0     0.0311   2.69e- 2   3.15e- 4
#>  9 5ac97e0 option_1   3.77e- 2          0     0.962    5.16e- 4   6.58e- 9
#> 10 5ac97e0 option_1   1.77e-15          0     0.999    9.17e- 5   5.47e- 4
#> # ℹ 2,390 more rows

samp_cat <- cat_sample_data(my_elic_cat,
                            topic = "Mechanism3",
                            method = "bootstrap")
#> ✔ Data sampled successfully using "bootstrap" method.
samp_cat
#> # A tibble: 1,800 × 7
#>    id      option   category_1 category_2 category_3 category_4 category_5
#>    <chr>   <chr>         <dbl>      <dbl>      <dbl>      <dbl>      <dbl>
#>  1 5ac97e0 option_1   1.50e-16   2.11e-94  2.89e- 26      1.000   2.71e- 6
#>  2 5ac97e0 option_1   4.28e- 9   4.41e-64  5.51e- 20      1.000   6.55e- 5
#>  3 5ac97e0 option_1   2.77e- 9   5.40e- 4  4.16e- 70      0.999   1.18e-11
#>  4 5ac97e0 option_1   4.72e-47   5.34e-26  3.95e-114      0.368   6.32e- 1
#>  5 5ac97e0 option_1   2.35e- 7   3.36e- 3  4.58e- 88      0.995   1.92e- 3
#>  6 5ac97e0 option_1   3.57e-81   2.76e-30  9.17e- 64      0.584   4.16e- 1
#>  7 5ac97e0 option_1   5.58e- 6   2.02e- 3  5.46e- 17      0.750   2.48e- 1
#>  8 5ac97e0 option_1   1.52e- 1   3.89e-43  2.66e- 78      0.831   1.65e- 2
#>  9 5ac97e0 option_1   7.79e- 5   1.08e- 2  1.24e- 64      0.989   3.40e-17
#> 10 5ac97e0 option_1   4.58e-29   1.33e-30  2.30e-  4      1.000   5.88e-25
#> # ℹ 1,790 more rows
```

And the sample summarised:

``` r
summary(samp_cat, option = "option_2")
#> # A tibble: 5 × 7
#>   Category        Min       Q1     Median   Mean     Q3   Max
#>   <chr>         <dbl>    <dbl>      <dbl>  <dbl>  <dbl> <dbl>
#> 1 category_1 0        1.88e-13 0.0139     0.211  0.277  1.000
#> 2 category_2 1.96e-24 5.72e- 4 0.0256     0.200  0.324  0.996
#> 3 category_3 4.56e-33 2.08e- 4 0.0163     0.148  0.163  0.994
#> 4 category_4 0        0        0.00000324 0.0876 0.0169 0.992
#> 5 category_5 1.43e-15 2.26e- 2 0.199      0.353  0.682  1.000
```

And plotted as violin plots:

``` r
plot(samp_cat,
     topic = "Mechanism1",
     title = "Sampled data for Mechanism1",
     ylab = "Probability")
```

<img src="man/figures/README-plot sampled data from categorical data - violin-1.png" alt="Density plot of the sampled data for variable 3." width="100%" />

## Similar packages

- {shelf} : Oakley, J. (2024). Package “SHELF” Tools to Support the
  Sheffield Elicitation Framework.
  <https://doi.org/10.32614/CRAN.package.SHELF>
- {prefR} : Lepird, J. (2022). Package “prefeR” R Package for Pairwise
  Preference Elicitation. <https://doi.org/10.32614/CRAN.package.prefeR>
