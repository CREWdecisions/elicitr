
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

### Description

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

### Installation

You can install the development version of elicitr from GitHub with:

``` r
# install.packages("pak")
pak::pak("CREWdecisions/elicitr")
```

### Getting started

``` r
library(elicitr)
```

All the functions in the elicitr package start with the prefix `elic_`.
After that, two prefixes are available: `elic_cont` and `elic_cat`. This
design choice is intended to enhance functions discovery.

### Elicitation of continuous variables

##### Simulated datasets

Two simulated datasets are included in elicitr:  

``` r
elicitr::round_1
#> # A tibble: 6 × 9
#>   name         var1_best var2_min var2_max var2_best var3_min var3_max var3_best
#>   <chr>            <int>    <int>    <int>     <int>    <dbl>    <dbl>     <dbl>
#> 1 Derek Macle…         1       20       24        22     0.43     0.83      0.73
#> 2 Christopher…         0        7       10         9     0.67     0.87      0.77
#> 3 Mar'Quasa B…         0       10       15        12     0.65     0.95      0.85
#> 4 Mastoora al…        -7        4       12         9     0.44     0.84      0.64
#> 5 Eriberto Mu…        -5       13       18        16     0.38     0.88      0.68
#> 6 Paul Bol             3       20       26        25     0.35     0.85      0.65
#> # ℹ 1 more variable: var3_conf <int>
```

### 

``` r
elicitr::round_2
#> # A tibble: 6 × 9
#>   name         var1_best var2_min var2_max var2_best var3_min var3_max var3_best
#>   <chr>            <int>    <int>    <int>     <int>    <dbl>    <dbl>     <dbl>
#> 1 Mar'Quasa B…        -2       15       21        18     0.62     0.82      0.72
#> 2 Mastoora al…        -4       11       15        12     0.52     0.82      0.72
#> 3 Eriberto Mu…         1       15       20        17     0.58     0.78      0.68
#> 4 Derek Macle…         0       11       18        15     0.52     0.82      0.72
#> 5 Christopher…        -2       14       18        15     0.55     0.85      0.75
#> 6 Paul Bol             1       18       23        20     0.66     0.86      0.76
#> # ℹ 1 more variable: var3_conf <int>
```

The core of the elicitr package is an object of class `elicit`. Just as
you create a form to collect estimates in an elicitation process, you
need to create the metadata file to hold the data from that elicitation.
Any analysis starts by creating the `elicit` object with the function
`elic_cont_start()`. This will allows to check whether all experts have
given their answers in the expected way.  
To build this `elicit` object, three parameters must be specified:

- the number of variables `var` (i.e. the number of topics in your
  elicitation),
- the type of variables `var_types` for each of these variables (many
  options are available, ranging from reel numbers to probabilities),
- the type of elicitation `elic_types` for each of these variables
  (three options are available, ranging from one estimate elicitations,
  to more elaborate ones with minimum and maximum values, best guess,
  and confidence level of experts)

``` r
elicit_object <- elicitr::elic_cont_start(var = c("var1", "var2", "var3"),
                                          var_types = "ZNp",
                                          elic_types = "134",
                                          experts = 6,
                                          title = "elicitation example",
                                          verbose = FALSE)
```

    #> 
    #> ── elicitation example ──
    #> 
    #> • Variables: "var1", "var2", and "var3"
    #> • Variable types: "Z", "N", and "p"
    #> • Elicitation types: "1p", "3p", and "4p"
    #> • Number of experts: 6
    #> • Number of rounds: 0

Once the metadata has been created, the data of the first round of
elicitation can be added with the function `elic_cont_add_data()`.

``` r
round1_elicit <- elicitr::elic_cont_add_data(elicit_object,
                                             elicitr::round_1,
                                             round = 1)
#> ✔ Data added to "Round 1" from "data.frame"
```

The information message confirms that the data for the first round has
been added to the metadata from a `data.frame`.  

If you conducted further rounds of elicitation, these can be added
subsequently to the metadata:

``` r
round2_elicit <- elicitr::elic_cont_add_data(round1_elicit,
                                             elicitr::round_2,
                                             round = 2)
#> ✔ Data added to "Round 2" from "data.frame"
```

To keep the anonymity of the experts, their names are converted to short
sha1 hashes and saved in the `id` column. These are then used to match
the expert’s answers in the multiple rounds.  

The function `elic_cont_get_data()` retrieves data from an `elicit`
object. It is possible to get the whole dataset of a given round, or
extract only the data for a given `var`, `var_types`, or `elic_types`.

``` r
elicitr::elic_cont_get_data(round2_elicit, round = 1, var = "all")
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

Finally, data can be plotted using the function `elic_cont_plot()`. This
function plots data belonging to a given `round` and for a given `var`.

``` r
elicitr::elic_cont_plot(round2_elicit,
                        round = 2,
                        group = TRUE,
                        var = "var3",
                        xlab = "Variable 3",
                        ylab = "Experts")
#> ✔ Rescaled min and max
```

<img src="man/figures/README-continuous plot-1.png" width="100%" />

### Elicitation of continuous variables

###### In development

### Similar packages

- {shelf} : Oakley, J. (2024). Package “SHELF” Tools to Support the
  Sheffield Elicitation Framework.
  <https://doi.org/10.32614/CRAN.package.SHELF>
- {prefR} : Lepird, J. (2022). Package “prefeR” R Package for Pairwise
  Preference Elicitation. <https://doi.org/10.32614/CRAN.package.prefeR>
