
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
```

<img src="man/figures/README-plot-raw-data-1.png" alt="" width="100%" />

When the elicitation process is part of a workshop and is used for
demonstration, it can be useful to show a truth argumenton the plot.
This argument can be added as a list of estimates.

``` r
plot(my_elic_cont, round = 1, var = "var2",
     truth = list(min = 10, max = 20, best = 15))
```

<img src="man/figures/README-plot-truth-1.png" alt="" width="100%" />

Estimates can also be plotted grouped across experts:

``` r
plot(my_elic_cont, round = 1, var = "var2",
     truth = list(min = 10, max = 20, best = 15),
     group = TRUE)
```

<img src="man/figures/README-plot-group-1.png" alt="" width="100%" />

Data can be sampled from the elicitation object:

``` r
samp_cont <- cont_sample_data(my_elic_cont, round = 2)
#> ✔ Rescaled min and max for variable "var3".
#> ✔ Data for "var1", "var2", and "var3" sampled successfully using the "basic" method.

samp_cont
#> # A tibble: 18,000 × 3
#>    id      var   value
#>    <chr>   <chr> <dbl>
#>  1 5ac97e0 var1      0
#>  2 5ac97e0 var1      0
#>  3 5ac97e0 var1     -2
#>  4 5ac97e0 var1      0
#>  5 5ac97e0 var1      0
#>  6 5ac97e0 var1      0
#>  7 5ac97e0 var1     -4
#>  8 5ac97e0 var1     -2
#>  9 5ac97e0 var1     -2
#> 10 5ac97e0 var1     -2
#> # ℹ 17,990 more rows
```

And the sample summarised:

``` r
summary(samp_cont)
#> # A tibble: 3 × 7
#>   Var      Min     Q1 Median   Mean     Q3    Max
#>   <chr>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1 var1  -4     -2     -2     -1.01   1      1    
#> 2 var2  11.0   14.5   16.3   16.3   18.3   22.7  
#> 3 var3   0.486  0.672  0.715  0.715  0.761  0.875
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
#> ℹ Estimates sum to 1. Rescaling to 100.
#> ✔ Data added to Topic "Mechanism1" from "data.frame"

my_elic_cat <- cat_add_data(my_elic_cat,
                            data_source = topic_2,
                            topic = "Mechanism2")
#> ℹ Estimates sum to 1. Rescaling to 100.
#> ✔ Data added to Topic "Mechanism2" from "data.frame"

my_elic_cat <- cat_add_data(my_elic_cat,
                            data_source = topic_3,
                            topic = "Mechanism3")
#> ℹ Estimates sum to 1. Rescaling to 100.
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
#>  1 5ac97e0 category_1 option_1         15        8
#>  2 5ac97e0 category_2 option_1         15        0
#>  3 5ac97e0 category_3 option_1         15       85
#>  4 5ac97e0 category_4 option_1         15        2
#>  5 5ac97e0 category_5 option_1         15        5
#>  6 5ac97e0 category_1 option_2         35        2
#>  7 5ac97e0 category_2 option_2         35       11
#>  8 5ac97e0 category_3 option_2         35       18
#>  9 5ac97e0 category_4 option_2         35        2
#> 10 5ac97e0 category_5 option_2         35       67
#> # ℹ 110 more rows
```

Data can be sampled from the elicitation object using the basic or
bootstrap method:

``` r
samp_cat_basic <- cat_sample_data(my_elic_cat,
                            topic = "Mechanism1",
                            method = "basic")
#> ✔ Data sampled successfully using "basic" method.
samp_cat_basic
#> # A tibble: 2,400 × 7
#>    id      option   category_1 category_2 category_3 category_4 category_5
#>    <chr>   <chr>         <dbl>      <dbl>      <dbl>      <dbl>      <dbl>
#>  1 5ac97e0 option_1     0.0710          0      0.870    0.0185      0.0400
#>  2 5ac97e0 option_1     0.0886          0      0.819    0.0234      0.0695
#>  3 5ac97e0 option_1     0.147           0      0.799    0.00641     0.0472
#>  4 5ac97e0 option_1     0.0857          0      0.847    0.0207      0.0467
#>  5 5ac97e0 option_1     0.104           0      0.822    0.00639     0.0678
#>  6 5ac97e0 option_1     0.0499          0      0.836    0.0844      0.0296
#>  7 5ac97e0 option_1     0.0858          0      0.834    0.00791     0.0722
#>  8 5ac97e0 option_1     0.105           0      0.820    0.0309      0.0439
#>  9 5ac97e0 option_1     0.0557          0      0.863    0.00943     0.0718
#> 10 5ac97e0 option_1     0.0636          0      0.795    0.0351      0.106 
#> # ℹ 2,390 more rows

samp_cat_bootstrap <- cat_sample_data(my_elic_cat,
                            topic = "Mechanism3",
                            method = "bootstrap")
#> ✔ Data sampled successfully using "bootstrap" method.
samp_cat_bootstrap
#> # A tibble: 1,800 × 7
#>    id      option   category_1 category_2 category_3 category_4 category_5
#>    <chr>   <chr>         <dbl>      <dbl>      <dbl>      <dbl>      <dbl>
#>  1 5ac97e0 option_1     0.0210    0.0170    0.0133        0.809     0.139 
#>  2 5ac97e0 option_1     0.0103    0.0228    0.00143       0.894     0.0712
#>  3 5ac97e0 option_1     0.0310    0.0397    0.0278        0.780     0.121 
#>  4 5ac97e0 option_1     0.0451    0.0158    0.0110        0.830     0.0977
#>  5 5ac97e0 option_1     0.0258    0.00895   0.0251        0.828     0.113 
#>  6 5ac97e0 option_1     0.0463    0.0228    0.000724      0.840     0.0898
#>  7 5ac97e0 option_1     0.0111    0.00310   0.0140        0.918     0.0542
#>  8 5ac97e0 option_1     0.0106    0.00862   0.0128        0.897     0.0711
#>  9 5ac97e0 option_1     0.0124    0.0361    0.0236        0.836     0.0917
#> 10 5ac97e0 option_1     0.0462    0.0107    0.0131        0.821     0.109 
#> # ℹ 1,790 more rows
```

And the sample summarised:

``` r
summary(samp_cat_basic, option = "option_2")
#> # A tibble: 5 × 7
#>   Category        Min     Q1 Median   Mean    Q3   Max
#>   <chr>         <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>
#> 1 category_1 0.000924 0.100  0.159  0.194  0.323 0.475
#> 2 category_2 0.0180   0.0848 0.134  0.206  0.342 0.584
#> 3 category_3 0.00532  0.115  0.208  0.223  0.290 0.607
#> 4 category_4 0.000489 0.0264 0.0731 0.0953 0.144 0.407
#> 5 category_5 0.000643 0.148  0.247  0.282  0.351 0.802
```

And plotted as violin plots:

``` r
plot(samp_cat_basic,
     title = "Sampled data for Mechanism1")
```

<img src="man/figures/README-plot sampled data from categorical data - violin-1.png" alt="Density plot of the sampled data for variable 3." width="100%" />

## Similar packages

- {shelf} : Oakley, J. (2024). Package “SHELF” Tools to Support the
  Sheffield Elicitation Framework.
  <https://doi.org/10.32614/CRAN.package.SHELF>
- {prefR} : Lepird, J. (2022). Package “prefeR” R Package for Pairwise
  Preference Elicitation. <https://doi.org/10.32614/CRAN.package.prefeR>
