# Plot categorical samples

**\[experimental\]**

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) aggregates and
plots categorical samples as violin plot.

## Usage

``` r
# S3 method for class 'cat_sample'
plot(
  x,
  type = "violin",
  ...,
  option = "all",
  title = NULL,
  ylab = "Probability",
  colours = NULL,
  family = "sans",
  theme = NULL,
  beeswarm_cex = 0.6,
  beeswarm_corral = "none"
)
```

## Arguments

- x:

  an object of class `cat_sample` created by the function
  [cat_sample_data](https://crewdecisions.github.io/elicitr/reference/cat_sample_data.md).

- type:

  character string with the type of plot, either *beeswarm* or *violin*.

- ...:

  Unused arguments, included only for future extensions of the function.

- option:

  character string with the name of the option or character vector with
  the options that you want to extract from the data. Use `all` for all
  options.

- title:

  character string with the title of the plot. If `NULL`, the title will
  be the topic name.

- ylab:

  character string with the label of the y-axis.

- colours:

  vector of colours to use for the categories.

- family:

  character string with the font family to use in the plot.

- theme:

  a [`theme`](https://ggplot2.tidyverse.org/reference/theme.html)
  function to overwrite the default theme.

- beeswarm_cex:

  numeric, the space between points in the beeswarm plot.

- beeswarm_corral:

  character string, the wrapping corral for the beeswarm plot. Anything
  accepted by the
  [geom_beeswarm](https://rdrr.io/pkg/ggbeeswarm/man/geom_beeswarm.html)
  function.

## Value

Invisibly a
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

## Details

If a `theme` is provided, the `family` argument is ignored.

## See also

Other plot helpers:
[`plot.cont_sample()`](https://crewdecisions.github.io/elicitr/reference/plot.cont_sample.md),
[`plot.elic_cont()`](https://crewdecisions.github.io/elicitr/reference/plot.elic_cont.md)

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

# Plot the sampled data for all options
plot(samp)


# Plot the sampled data as beeswarm plot

if (FALSE) { # \dontrun{
plot(samp, type = "beeswarm", beeswarm_corral = "wrap")
} # }

if (FALSE) { # \dontrun{
# Plot the sampled data for option 1
plot(samp, option = "option_1")
} # }
if (FALSE) { # \dontrun{
# Plot the sampled data for option 1 and 3
plot(samp, option = c("option_1", "option_3"))
} # }
if (FALSE) { # \dontrun{
# Provide custom colours
plot(samp, colours = c("steelblue4", "darkcyan", "chocolate1",
                       "chocolate3", "orangered4"))
} # }
if (FALSE) { # \dontrun{
# Overwrite the default theme
plot(samp, theme = ggplot2::theme_minimal())
} # }
```
