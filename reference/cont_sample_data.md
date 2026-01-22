# Sample continuous data

**\[experimental\]**

`cont_sample_data()` samples data based on expert estimates stored in
the
[`elic_cont`](https://crewdecisions.github.io/elicitr/reference/elic_cont.md)
object.

## Usage

``` r
cont_sample_data(
  x,
  round,
  ...,
  method = "basic",
  var = "all",
  n_votes = 1000,
  weights = 1,
  verbose = TRUE
)
```

## Arguments

- x:

  an object of class
  [elic_cont](https://crewdecisions.github.io/elicitr/reference/elic_cont.md).

- round:

  integer indicating if the data belongs to the first or second
  elicitation round.

- ...:

  Unused arguments, included only for future extensions of the function.

- method:

  character string with the name of the method to sample the data, only
  the *basic* is implemented, see Method below.

- var:

  character string with the name of the variable or character vector
  with more variable names that you want to extract from the data. Use
  `all` for all variables.

- n_votes:

  numeric indicating the number of votes to consider.

- weights:

  numeric vector with the weights to apply to the estimates. If equal to
  `1`, each experts get `n_votes` votes, see Weights below.

- verbose:

  logical, if TRUE it prints informative messages.

## Value

A [`tibble`](https://tibble.tidyverse.org/reference/tibble.html) with
the sampled data. This object has an additional class `cont_sample` used
to implement the plotting method.

## Weights

To provide a different number of votes to each expert, use the `weights`
argument. The length of the vector must be equal to the number of
experts. If provided when the elicitation type is the *four points
elicitation*, their values overwrite the confidence estimates.

## Method

The function samples the data using the basic method. The basic method
samples the data based on the expert estimates with differences between
the different elicitation types:

- *one point elicitation*: the best estimate of each expert represent
  the pool of values that are sampled `n_votes` `*` `n_experts` times,
  with repetition.

- *three points elicitation*: the minimum, best, and maximum estimates
  of each expert are used as scaling parameters of the PERT distribution
  from which the data are sampled. The `weights` argument can be used to
  weight the estimates of each expert.

- *five points elicitation*: the minimum, best, and maximum estimates of
  each expert are rescaled according to their confidence and used as
  scaling parameters of the PERT distribution from which the data are
  sampled.

## See also

Other cont data helpers:
[`cont_add_data()`](https://crewdecisions.github.io/elicitr/reference/cont_add_data.md),
[`cont_get_data()`](https://crewdecisions.github.io/elicitr/reference/cont_get_data.md),
[`cont_start()`](https://crewdecisions.github.io/elicitr/reference/cont_start.md),
[`summary.cont_sample()`](https://crewdecisions.github.io/elicitr/reference/summary.cont_sample.md)

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
samp
#> # A tibble: 18,000 × 3
#>    id      var   value
#>    <chr>   <chr> <dbl>
#>  1 5ac97e0 var1     -2
#>  2 5ac97e0 var1     -2
#>  3 5ac97e0 var1      1
#>  4 5ac97e0 var1      0
#>  5 5ac97e0 var1      1
#>  6 5ac97e0 var1     -4
#>  7 5ac97e0 var1     -2
#>  8 5ac97e0 var1     -4
#>  9 5ac97e0 var1      1
#> 10 5ac97e0 var1     -4
#> # ℹ 17,990 more rows

# Sample data for the first round for the variable `var1` and `var2`
samp <- cont_sample_data(my_elicit, round = 1, var = c("var1", "var2"))
#> ✔ Data for "var1" and "var2" sampled successfully using the "basic" method.

# Sample data for the second round for the variable `var3`. Notice that the
# data are rescaled using the expert confidence before sampling.
samp <- cont_sample_data(my_elicit, round = 2, var = "var1")
#> ✔ Data for "var1" sampled successfully using the "basic" method.

# Sample data for the first round for the variable `var3` providing the
# weights. Notice that the weights overwrite the confidence estimates and
# that some values are constrained to be between 0 and 1 after rescaling.
samp <- cont_sample_data(my_elicit, round = 1, var = "var3",
                         weights = c(0.8, 0.7, 0.9, 0.7, 0.6, 0.9))
#> ℹ Provided weights used instead of confidence estimates
#> Warning: ! Some values have been constrained to be between 0 and 1.
#> ✔ Rescaled min and max for variable "var3".
#> ✔ Data for "var3" sampled successfully using the "basic" method.
```
