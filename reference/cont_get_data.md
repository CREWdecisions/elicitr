# Get data

**\[experimental\]**

`cont_get_data()` gets data from an
[elic_cont](https://crewdecisions.github.io/elicitr/reference/elic_cont.md)
object.

## Usage

``` r
cont_get_data(
  x,
  round,
  ...,
  var = "all",
  var_types = "all",
  elic_types = "all"
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

- var:

  character string with the name of the variable or character vector
  with more variable names that you want to extract from the data. Use
  `all` for all variables.

- var_types:

  character string with short codes indicating the variable type. See
  Variable types for more.

- elic_types:

  character string with the short codes codes indicating the elicitation
  type. Use `all` for all elicitation types. See Elicitation Types for
  more.

## Value

A [`tibble`](https://tibble.tidyverse.org/reference/tibble.html) with
the extracted data.

## Details

One one optional argument can be specified. If more than one is
provided, the first of the following will be used: `var`, `var_types`,
or `elic_types`.

## Variable types

Variable types must be provided as a single string containing short
codes, e.g. "pPN".

Valid short codes are:

- `Z`: *integers*, when the estimate must be an integer number in the
  interval (-Inf, Inf).

- `N`: *positive integers*, when the estimate must be an integer number
  in the interval (0, Inf).

- `z`: *negative integers*, when the estimate must be an integer number
  in the interval (-Inf, 0\].

- `R`: *reals*, when the estimate must be a real number in the interval
  (-Inf, Inf).

- `s`: *positive reals*, when the estimate must be a real number in the
  interval (0, Inf).

- `r`: *negative reals*, when the estimate must be a real number in the
  interval (-Inf, \].

- `p`: *probability*, when the estimate must be a real number in the
  interval (0, 1).

## Elicitation types

Elicitation types must be provided as a single string containing short
codes, e.g. "134".

Valid short codes are:

- `1`: *one point elicitation*, when only the best estimate is provided.

- `3`: *three points elicitation*, when the minimum, maximum, and best
  estimates are provided.

- `4`: *four points elicitation*, when the minimum, maximum, best, and
  confidence estimates are provided.

## See also

Other cont data helpers:
[`cont_add_data()`](https://crewdecisions.github.io/elicitr/reference/cont_add_data.md),
[`cont_sample_data()`](https://crewdecisions.github.io/elicitr/reference/cont_sample_data.md),
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
  cont_add_data(data_source = round_1, round = 1) |>
  cont_add_data(data_source = round_2, round = 2)
#> ✔ <elic_cont> object for "Elicitation" correctly initialised
#> ✔ Data added to "Round 1" from "data.frame"
#> ✔ Data added to "Round 2" from "data.frame"

# Get all data from round 1
cont_get_data(my_elicit, round = 1)
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

# Get data by variable name----
# Get data for var3 from round 2
cont_get_data(my_elicit, round = 2, var = "var3")
#> # A tibble: 6 × 5
#>   id      var3_min var3_max var3_best var3_conf
#>   <chr>      <dbl>    <dbl>     <dbl>     <int>
#> 1 5ac97e0     0.52     0.82      0.72        72
#> 2 e51202e     0.55     0.85      0.75        97
#> 3 e78cbf4     0.62     0.82      0.72        72
#> 4 9fafbee     0.52     0.82      0.72        88
#> 5 3cc9c29     0.58     0.78      0.68        92
#> 6 3d32ab9     0.66     0.86      0.76        80

# Get data for var1 and var2 from round 1
cont_get_data(my_elicit, round = 1, var = c("var1", "var2"))
#> # A tibble: 6 × 5
#>   id      var1_best var2_min var2_max var2_best
#>   <chr>       <int>    <int>    <int>     <int>
#> 1 5ac97e0         1       20       24        22
#> 2 e51202e         0        7       10         9
#> 3 e78cbf4         0       10       15        12
#> 4 9fafbee        -7        4       12         9
#> 5 3cc9c29        -5       13       18        16
#> 6 3d32ab9         3       20       26        25

# Get data by variable type----
# Get data for variables containing integer numbers
cont_get_data(my_elicit, round = 2, var_types = "Z")
#> # A tibble: 6 × 2
#>   id      var1_best
#>   <chr>       <int>
#> 1 5ac97e0         0
#> 2 e51202e        -2
#> 3 e78cbf4        -2
#> 4 9fafbee        -4
#> 5 3cc9c29         1
#> 6 3d32ab9         1
# Get data for variables containing positive integers and probabilities
cont_get_data(my_elicit, round = 2, var_types = "Np")
#> # A tibble: 6 × 8
#>   id      var2_min var2_max var2_best var3_min var3_max var3_best var3_conf
#>   <chr>      <int>    <int>     <int>    <dbl>    <dbl>     <dbl>     <int>
#> 1 5ac97e0       11       18        15     0.52     0.82      0.72        72
#> 2 e51202e       14       18        15     0.55     0.85      0.75        97
#> 3 e78cbf4       15       21        18     0.62     0.82      0.72        72
#> 4 9fafbee       11       15        12     0.52     0.82      0.72        88
#> 5 3cc9c29       15       20        17     0.58     0.78      0.68        92
#> 6 3d32ab9       18       23        20     0.66     0.86      0.76        80

# Get data by elicitation type----
# Get data for three points estimates
cont_get_data(my_elicit, round = 2, elic_types = "3")
#> # A tibble: 6 × 4
#>   id      var2_min var2_max var2_best
#>   <chr>      <int>    <int>     <int>
#> 1 5ac97e0       11       18        15
#> 2 e51202e       14       18        15
#> 3 e78cbf4       15       21        18
#> 4 9fafbee       11       15        12
#> 5 3cc9c29       15       20        17
#> 6 3d32ab9       18       23        20
# Get data for one and four points estimates
cont_get_data(my_elicit, round = 2, elic_types = "14")
#> # A tibble: 6 × 6
#>   id      var1_best var3_min var3_max var3_best var3_conf
#>   <chr>       <int>    <dbl>    <dbl>     <dbl>     <int>
#> 1 5ac97e0         0     0.52     0.82      0.72        72
#> 2 e51202e        -2     0.55     0.85      0.75        97
#> 3 e78cbf4        -2     0.62     0.82      0.72        72
#> 4 9fafbee        -4     0.52     0.82      0.72        88
#> 5 3cc9c29         1     0.58     0.78      0.68        92
#> 6 3d32ab9         1     0.66     0.86      0.76        80
```
