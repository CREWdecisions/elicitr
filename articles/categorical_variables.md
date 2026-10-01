# Categorical variables

``` r

library(elicitr)
#> Registered S3 method overwritten by 'car':
#>   method           from
#>   na.action.merMod lme4
```

Many of the concepts introduced in
[`vignette("continuous_variables")`](https://crewdecisions.github.io/elicitr/articles/continuous_variables.md)
are also applicable to categorical variables, and the name of the
functions are the same but have the prefix `cat` instead of `cont`.
However, there are some differences in the workflow for loading and
analysing data collected during the elicitation of categorical
variables. This vignette will guide you through the process of loading
and analysing categorical data.

## Datasets

There are three datasets included in the package for demonstration
purposes:
[`?topic_1`](https://crewdecisions.github.io/elicitr/reference/cat_data.md),
[`?topic_2`](https://crewdecisions.github.io/elicitr/reference/cat_data.md),
and
[`?topic_3`](https://crewdecisions.github.io/elicitr/reference/cat_data.md):

``` r

topic_1
#> # A tibble: 120 × 5
#>    name            option   category   confidence estimate
#>    <chr>           <chr>    <chr>           <dbl>    <dbl>
#>  1 Derek Maclellan option_1 category_1         66     0.57
#>  2 Derek Maclellan option_1 category_2         66     0.18
#>  3 Derek Maclellan option_1 category_3         66     0.02
#>  4 Derek Maclellan option_1 category_4         66     0.02
#>  5 Derek Maclellan option_1 category_5         66     0.21
#>  6 Derek Maclellan option_2 category_1         86     0.06
#>  7 Derek Maclellan option_2 category_2         86     0.04
#>  8 Derek Maclellan option_2 category_3         86     0.12
#>  9 Derek Maclellan option_2 category_4         86     0.42
#> 10 Derek Maclellan option_2 category_5         86     0.36
#> # ℹ 110 more rows
```

``` r

topic_2
#> # A tibble: 100 × 5
#>    name              option   category   confidence estimate
#>    <chr>             <chr>    <chr>           <dbl>    <dbl>
#>  1 Christopher Felix option_1 category_1         86     0.28
#>  2 Christopher Felix option_1 category_2         86     0.13
#>  3 Christopher Felix option_1 category_3         86     0.55
#>  4 Christopher Felix option_1 category_4         86     0.04
#>  5 Christopher Felix option_1 category_5         86     0   
#>  6 Christopher Felix option_2 category_1         81     0.06
#>  7 Christopher Felix option_2 category_2         81     0.26
#>  8 Christopher Felix option_2 category_3         81     0.02
#>  9 Christopher Felix option_2 category_4         81     0.47
#> 10 Christopher Felix option_2 category_5         81     0.19
#> # ℹ 90 more rows
```

``` r

topic_3
#> # A tibble: 90 × 5
#>    name            option   category   confidence estimate
#>    <chr>           <chr>    <chr>           <dbl>    <dbl>
#>  1 Derek Maclellan option_1 category_1         61     0.12
#>  2 Derek Maclellan option_1 category_2         61     0.5 
#>  3 Derek Maclellan option_1 category_3         61     0.16
#>  4 Derek Maclellan option_1 category_4         61     0.02
#>  5 Derek Maclellan option_1 category_5         61     0.2 
#>  6 Derek Maclellan option_2 category_1         91     0.02
#>  7 Derek Maclellan option_2 category_2         91     0.76
#>  8 Derek Maclellan option_2 category_3         91     0.15
#>  9 Derek Maclellan option_2 category_4         91     0.06
#> 10 Derek Maclellan option_2 category_5         91     0.01
#> # ℹ 80 more rows
```

In each dataset the first column contains the name of the expert, the
second the options considered and the third the categories of the
categorical variable. The fourth column contains the expert’s
confidence, and the fifth the expert’s estimate. Expert estimates for
each option should represent probabilities or percentages, and should
thus sum up to 1 or 100. Both are accepted and data scaled to 1 will be
automatically rescaled to 100.

## Load data

We start by creating the
[`?elic_cat`](https://crewdecisions.github.io/elicitr/reference/elic_cat.md)
object with the function
[`cat_start()`](https://crewdecisions.github.io/elicitr/reference/cat_start.md).
As for the continuous variables, this objects stores the metadata of the
elicitation process:

``` r

my_categories <- c("category_1", "category_2", "category_3",
                   "category_4", "category_5")
my_options <- c("option_1", "option_2", "option_3", "option_4")
my_topics <- c("topic_1", "topic_2", "topic_3")
my_elicitation <- cat_start(categories = my_categories,
                            options = my_options,
                            experts = 6,
                            topics = my_topics)
#> ✔ <elic_cat> object for "Elicitation" correctly initialised
my_elicitation
#> 
#> ── Elicitation ──
#> 
#> • Categories: "category_1", "category_2", "category_3", "category_4", and
#> "category_5"
#> • Options: "option_1", "option_2", "option_3", and "option_4"
#> • Number of experts: 6
#> • Topics: "topic_1", "topic_2", and "topic_3"
#> • Data available for 0 topics
```

This elicitation process is for a categorical variables with 5
categories estimated for four options and three topics by six experts.

As we did for continuous variables, we can load the data with the
function
[`cat_add_data()`](https://crewdecisions.github.io/elicitr/reference/cat_add_data.md):

``` r

my_elicitation <- cat_add_data(my_elicitation,
                               data_source = topic_1,
                               topic = "topic_1") |>
  cat_add_data(data_source = topic_2, topic = "topic_2") |>
  cat_add_data(data_source = topic_3, topic = "topic_3")
#> ℹ Estimates sum to 1. Rescaling to 100.
#> ✔ Data added to Topic "topic_1" from "data.frame"
#> 
#> ℹ Estimates sum to 1. Rescaling to 100.
#> ✔ Data added to Topic "topic_2" from "data.frame"
#> 
#> ℹ Estimates sum to 1. Rescaling to 100.
#> ✔ Data added to Topic "topic_3" from "data.frame"
```

As mentioned before, estimates can also sum up to 100.

``` r

topic_1_percent <- dplyr::mutate(topic_1,
                                 estimate = estimate * 100)

my_elicitation <- cat_add_data(my_elicitation,
                               data_source = topic_1_percent,
                               topic = "topic_1")
#> ✔ Data added to Topic "topic_1" from "data.frame"
```

Expert anonymisation is automatic but optional. If expert names are not
to be anonymised, the argument `anonymise` can be set to `FALSE` in the
[`cat_add_data()`](https://crewdecisions.github.io/elicitr/reference/cat_add_data.md)
function.

``` r

my_elicitation <- cat_add_data(my_elicitation,
                               data_source = topic_1,
                               topic = "topic_1",
                               anonymise = FALSE)
#> ℹ Estimates sum to 1. Rescaling to 100.
#> ✔ Data added to Topic "topic_1" from "data.frame"
```

Again, metadata are used to validate the data. If the data is not
consistent with the metadata, an error message will be displayed. For
example, if we try to load data with a category not defined in the
metadata:

``` r

malformed_data <- topic_1
malformed_data[1, 2] <- "category_6"
cat_add_data(my_elicitation,
             data_source = malformed_data,
             topic = "topic_1")
#> Error in `cat_add_data()`:
#> ! The column with the name of the options contains unexpected values:
#> ✖ The value "category_6" is not valid.
#> ℹ Check the metadata in the <elic_cat> object.
```

## Get data

Data can be retrieved from the `elic_cat` object with the
[`cat_get_data()`](https://crewdecisions.github.io/elicitr/reference/cat_get_data.md)
function:

``` r

cat_get_data(my_elicitation, topic = "topic_1")
#> # A tibble: 120 × 5
#>    id              option   category   confidence estimate
#>    <chr>           <chr>    <chr>           <dbl>    <dbl>
#>  1 Derek Maclellan option_1 category_1         66       57
#>  2 Derek Maclellan option_1 category_2         66       18
#>  3 Derek Maclellan option_1 category_3         66        2
#>  4 Derek Maclellan option_1 category_4         66        2
#>  5 Derek Maclellan option_1 category_5         66       21
#>  6 Derek Maclellan option_2 category_1         86        6
#>  7 Derek Maclellan option_2 category_2         86        4
#>  8 Derek Maclellan option_2 category_3         86       12
#>  9 Derek Maclellan option_2 category_4         86       42
#> 10 Derek Maclellan option_2 category_5         86       36
#> # ℹ 110 more rows
```

Notice that the name of the expert is not anonymised in this case and
assigned to the column `id`.

Data can also be retrieved only for given options:

``` r

cat_get_data(my_elicitation, topic = "topic_2", option = "option_1")
#> # A tibble: 25 × 5
#>    id      option   category   confidence estimate
#>    <chr>   <chr>    <chr>           <dbl>    <dbl>
#>  1 e51202e option_1 category_1         86       28
#>  2 e51202e option_1 category_2         86       13
#>  3 e51202e option_1 category_3         86       55
#>  4 e51202e option_1 category_4         86        4
#>  5 e51202e option_1 category_5         86        0
#>  6 e78cbf4 option_1 category_1         71        5
#>  7 e78cbf4 option_1 category_2         71       20
#>  8 e78cbf4 option_1 category_3         71        7
#>  9 e78cbf4 option_1 category_4         71       47
#> 10 e78cbf4 option_1 category_5         71       21
#> # ℹ 15 more rows
```

Here, the data is anonymised, following what was specified when loading
the data.

## Data analysis

Contrary to continuous variables, there is not yet a function for
plotting the raw data. However, we can plot the distribution of the
sampled data.

### Sample data

Data can be sampled using the function
[`cat_sample_data()`](https://crewdecisions.github.io/elicitr/reference/cat_sample_data.md)
(see the variable documentation for the explanation of the sampling
methods). Here we sample 100 values for each option:

``` r

samp <- cat_sample_data(my_elicitation,
                        method = "unweighted",
                        topic = "topic_1",
                        n_votes = 100)
#> ✔ Data sampled successfully using "unweighted" method.
samp
#> # A tibble: 2,400 × 7
#>    id              option category_1 category_2 category_3 category_4 category_5
#>    <chr>           <chr>       <dbl>      <dbl>      <dbl>      <dbl>      <dbl>
#>  1 Derek Maclellan optio…      0.577      0.168     0.0185   0.0317        0.205
#>  2 Derek Maclellan optio…      0.669      0.143     0.0110   0.00777       0.169
#>  3 Derek Maclellan optio…      0.618      0.149     0.0147   0.0427        0.176
#>  4 Derek Maclellan optio…      0.623      0.131     0.0606   0.0123        0.173
#>  5 Derek Maclellan optio…      0.585      0.187     0.0101   0.0161        0.202
#>  6 Derek Maclellan optio…      0.635      0.165     0.0123   0.0128        0.175
#>  7 Derek Maclellan optio…      0.501      0.138     0.0342   0.00630       0.321
#>  8 Derek Maclellan optio…      0.550      0.194     0.0160   0.000700      0.239
#>  9 Derek Maclellan optio…      0.607      0.210     0.0184   0.0269        0.137
#> 10 Derek Maclellan optio…      0.522      0.192     0.0180   0.0158        0.252
#> # ℹ 2,390 more rows
```

Sampled data can be summarised for any option:

``` r

summary(samp, option = "option_1")
#> $option_1
#> # A tibble: 5 × 7
#>   category        Min     Q1 Median  Mean    Q3   Max
#>   <chr>         <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>
#> 1 category_1 0.0738   0.175   0.374 0.383 0.563 0.777
#> 2 category_2 0        0.0571  0.116 0.118 0.175 0.354
#> 3 category_3 0.000827 0.0506  0.103 0.131 0.191 0.415
#> 4 category_4 0.000700 0.0474  0.229 0.220 0.313 0.610
#> 5 category_5 0.00987  0.0913  0.147 0.148 0.195 0.348
```

And plotted as violin plot:

``` r

plot(samp)
```

![Violin plot of the sampled data for all
options.](categorical_variables_files/figure-html/plot-all-1.png)

Or as beeswarm plot, which can be adapted as needed:

``` r

plot(samp, type = "beeswarm")
```

![Beeswarm plot of the sampled data for all options. Each point
represents a sampled
value.](categorical_variables_files/figure-html/cat-plot-beeswarm-1.png)

``` r

plot(samp, type = "beeswarm",
     beeswarm_cex = 0.9, beeswarm_corral = "wrap")
```

![Beeswarm plot of the sampled data for all options. Each point
represents a sampled
value.](categorical_variables_files/figure-html/cat-plot-beeswarm-wrap-1.png)

We can also plot the distribution for a specific option:

``` r

plot(samp, option = "option_2")
```

![Violin plot of the sampled data for option
2.](categorical_variables_files/figure-html/plot-option2-1.png)
