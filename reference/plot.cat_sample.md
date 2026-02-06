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
