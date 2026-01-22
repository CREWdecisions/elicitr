# Start elicitation

**\[experimental\]**

`cont_start()` initialises an
[elic_cont](https://crewdecisions.github.io/elicitr/reference/elic_cont.md)
object which stores important metadata for the data collected during the
elicitation process of continuous variables.

## Usage

``` r
cont_start(
  var_names,
  var_types,
  elic_types,
  experts,
  ...,
  title = "Elicitation",
  verbose = TRUE
)
```

## Arguments

- var_names:

  character vector with the name of the estimated variables.

- var_types:

  character string with short codes indicating the variable type. If
  only one `var_type` is provided, its value is recycled for all
  variables. See Variable types for more.

- elic_types:

  character string with short codes indicating the elicitation type. If
  only one `elic_type` is provided, its value is recycled for all
  variables. See Elicitation Types for more.

- experts:

  numeric indicating the number of experts participating in the
  elicitation process.

- ...:

  Unused arguments, included only for future extensions of the function.

- title:

  character, used to bind a name to the object.

- verbose:

  logical, if `TRUE` it prints informative messages.

## Value

An object of class
[elic_cont](https://crewdecisions.github.io/elicitr/reference/elic_cont.md)
binding metadata related to the elicitation process. These metadata are
used by other functions to validate the correctness of the provided
data.

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

## References

Hemming, V., Burgman, M. A., Hanea, A. M., McBride, M. F., & Wintle, B.
C. (2018). A practical guide to structured expert elicitation using the
IDEA protocol. Methods in Ecology and Evolution, 9(1), 169–180.
<https://doi.org/10.1111/2041-210X.12857>

## See also

Other cont data helpers:
[`cont_add_data()`](https://crewdecisions.github.io/elicitr/reference/cont_add_data.md),
[`cont_get_data()`](https://crewdecisions.github.io/elicitr/reference/cont_get_data.md),
[`cont_sample_data()`](https://crewdecisions.github.io/elicitr/reference/cont_sample_data.md),
[`summary.cont_sample()`](https://crewdecisions.github.io/elicitr/reference/summary.cont_sample.md)

## Author

Sergio Vignali

## Examples

``` r
# Create the elic_cont object for an elicitation process that estimates 3
# variables, the first for a one point estimation of a positive integer, the
# second for three points estimation of a negative real, and the last for a
# four point estimation of a probability
my_elicit <- cont_start(var_names = c("var1", "var2", "var3"),
                        var_types = "Nrp",
                        elic_types = "134",
                        experts = 4)
#> ✔ <elic_cont> object for "Elicitation" correctly initialised
my_elicit
#> 
#> ── Elicitation ──
#> 
#> • Variables: "var1", "var2", and "var3"
#> • Variable types: "N", "r", and "p"
#> • Elicitation types: "1p", "3p", and "4p"
#> • Number of experts: 4
#> • Number of rounds: 0

# A title can be added to bind a name to the object:
my_elicit <- cont_start(var_names = c("var1", "var2", "var3"),
                        var_types = "Nrp",
                        elic_types = "134",
                        experts = 4,
                        title = "My elicitation")
#> ✔ <elic_cont> object for "My elicitation" correctly initialised
my_elicit
#> 
#> ── My elicitation ──
#> 
#> • Variables: "var1", "var2", and "var3"
#> • Variable types: "N", "r", and "p"
#> • Elicitation types: "1p", "3p", and "4p"
#> • Number of experts: 4
#> • Number of rounds: 0
# Notice that if var_types and elic_types are provided as single character,
# their value is recycled and applied to all variables. In the following
# example all three variables will be considered for a four point estimation
# to estimate a probability:
my_elicit <- cont_start(var_names = c("var1", "var2", "var3"),
                        var_types = "p",
                        elic_types = "4",
                        experts = 4)
#> ✔ <elic_cont> object for "Elicitation" correctly initialised
my_elicit
#> 
#> ── Elicitation ──
#> 
#> • Variables: "var1", "var2", and "var3"
#> • Variable type: "p"
#> • Elicitation type: "4p"
#> • Number of experts: 4
#> • Number of rounds: 0
```
