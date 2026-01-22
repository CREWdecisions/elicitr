# Plot continuous samples

**\[experimental\]**

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) aggregates and
plots continuous samples as violin or density plot.

## Usage

``` r
# S3 method for class 'cont_sample'
plot(
  x,
  var,
  ...,
  group = FALSE,
  type = "violin",
  title = NULL,
  xlab = "",
  ylab = "",
  colours = NULL,
  line_width = 0.7,
  family = "sans",
  expert_names = NULL,
  theme = NULL,
  beeswarm_cex = 0.6,
  beeswarm_corral = "none"
)
```

## Arguments

- x:

  n object of class `cont_sample` created by the function
  [cont_sample_data](https://crewdecisions.github.io/elicitr/reference/cont_sample_data.md).

- var:

  character string with the name of the variable to be plotted.

- ...:

  Unused arguments, included only for future extensions of the function.

- group:

  logical, if `TRUE` data are aggregated by expert.

- type:

  character string with the type of plot, either *beeswarm* or *violin*
  or *density*.

- title:

  character string with the title of the plot.

- xlab:

  character string with the x-axis label.

- ylab:

  character string with the y-axis label.

- colours:

  character vector with the colours to be used in the plot.

- line_width:

  numeric with the width of the lines in the density plot.

- family:

  character string with the font family to be used in the plot.

- expert_names:

  numeric or character, the labels for the experts.

- theme:

  [`theme`](https://ggplot2.tidyverse.org/reference/theme.html) function
  to be used in the plot.

- beeswarm_cex:

  numeric, the space between points in the beeswarm plot.

- beeswarm_corral:

  character string, the wrapping corral for the beeswarm plot. Anything
  accepted by the
  [geom_beeswarm](https://rdrr.io/pkg/ggbeeswarm/man/geom_beeswarm.html)
  function.

  \#' @section scale_conf:

  If the variable plotted is the result of a four points elicitation
  where expert confidence is provided, the minimum and maximum values
  provided by each expert are rescaled using their provided confidence
  categories. Users can choose how they want to rescale minimum and
  maximum values by providing a value for the `scale_conf` argument. If
  no argument is provided, a default value of 100 is used for
  scale_conf.

  The scaled minimum and maximum values are obtained with:

  \\minimum = best\\ guess - (best\\ guess - minimum)\frac{scale\\conf}
  {confidence}\\

  \\maximum = best\\ guess + (maximum - best\\ guess) \frac{scale\\conf}
  {confidence}\\

## Value

Invisibly a
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

## Details

If a `theme` is provided, the `family` argument is ignored.

## See also

Other plot helpers:
[`plot.cat_sample()`](https://crewdecisions.github.io/elicitr/reference/plot.cat_sample.md),
[`plot.elic_cont()`](https://crewdecisions.github.io/elicitr/reference/plot.elic_cont.md)

## Author

Sergio Vignali and Maude Vernet

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

# Sample data for the first round for all variables
samp <- cont_sample_data(my_elicit, round = 1)
#> Warning: ! Some values have been constrained to be between 0 and 1.
#> ✔ Rescaled min and max for variable "var3".
#> ✔ Data for "var1", "var2", and "var3" sampled successfully using the "basic" method.

# Plot the sampled data for the variable `var3` as violin plot
plot(samp, var = "var3", type = "violin")


# Plot the sampled data for the variable `var1` as beeswarm plot
plot(samp, var = "var1", type = "beeswarm")


# Plot the sampled data for the variable `var2` as density plot
plot(samp, var = "var2", type = "density")


# Plot the sampled data for the variable `var1` as density plot for the group
plot(samp, var = "var1", group = TRUE, type = "density")


# Plot the sampled data for the variable `var3` as violin plot passing the
# colours
plot(samp, var = "var3", type = "violin",
     colours = c("steelblue4", "darkcyan", "chocolate1",
                 "chocolate3", "orangered4", "royalblue1"))
```
