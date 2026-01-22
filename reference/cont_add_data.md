# Add data

**\[experimental\]**

`cont_add_data()` adds data to an
[elic_cont](https://crewdecisions.github.io/elicitr/reference/elic_cont.md)
object from different sources.

## Usage

``` r
cont_add_data(
  x,
  data_source,
  round,
  ...,
  sep = ",",
  sheet = 1,
  overwrite = FALSE,
  verbose = TRUE,
  anonymise = TRUE
)
```

## Arguments

- x:

  an object of class
  [elic_cont](https://crewdecisions.github.io/elicitr/reference/elic_cont.md).

- data_source:

  either a [`data.frame`](https://rdrr.io/r/base/data.frame.html) or
  [`tibble`](https://tibble.tidyverse.org/reference/tibble.html), a
  string with the path to a *csv* or *xlsx* file, or anything accepted
  by the
  [read_sheet](https://googlesheets4.tidyverse.org/reference/range_read.html)
  function.

- round:

  integer indicating if the data belongs to the first or second
  elicitation round.

- ...:

  Unused arguments, included only for future extensions of the function.

- sep:

  character used as field separator, used only when `data_source` is a
  path to a *csv* file.

- sheet:

  integer or character to select the sheet. The sheet can be referenced
  by its position with a number or by its name with a string. Used only
  when `data_source` is a path to a *xlsx* file or when data are
  imported from *Google Sheets*.

- overwrite:

  logical, whether to overwrite existing data already added to the
  [elic_cont](https://crewdecisions.github.io/elicitr/reference/elic_cont.md)
  object.

- verbose:

  logical, if `TRUE` it prints informative messages.

- anonymise:

  logical, if `TRUE` expert names are anonymised before adding the data
  to the
  [elic_cont](https://crewdecisions.github.io/elicitr/reference/elic_cont.md)
  object.

## Value

The provided object of class
[elic_cont](https://crewdecisions.github.io/elicitr/reference/elic_cont.md)
updated with the data.

## Data format

Data are expected to have the name of the expert always as first column.
The only exception is for data coming from *Google Sheet* which can have
an additional column with a timestamp. This column is automatically
removed before the data are added to the
[elic_cont](https://crewdecisions.github.io/elicitr/reference/elic_cont.md)
object (see "Data cleaning"). After the name there should be one or more
blocks which follow the specifications below:

*One point elicitation*:

- `var_best`: best estimate for the variable

*Three points elicitation*:

- `var_min`: minimum estimate for the variable

- `var_max`: maximum estimate for the variable

- `var_best`: best estimate for the variable

*Four points elicitation*:

- `var_min`: minimum estimate for the variable

- `var_max`: maximum estimate for the variable

- `var_best`: best estimate for the variable

- `var_conf`: confidence for the estimate

The column with names is unique, the other columns are a block and can
be repeated for each variable.

Moreover, the name of the columns is not important, `cont_add_data()`
will overwrite it according to the following convention:

*variable_name*\_*suffix*

with *suffix* being one of *min*, *max*, *best*, or *conf*. The
information to build the column names is taken from the metadata
available in the
[elic_cont](https://crewdecisions.github.io/elicitr/reference/elic_cont.md)
object.

`var_conf`, given as percentage, can be any number in the range (50,
100\]. Any value smaller or equal to 50 would imply that the accuracy of
the estimates is only due to chance).

## Data cleaning

When data are added to the
[elic_cont](https://crewdecisions.github.io/elicitr/reference/elic_cont.md)
object, first names are standardised by converting capital letters to
lower case, and by removing any whitespaces and punctuation. Then, data
are anonymised by converting names to short sha1 hashes. In this way,
sensible information collected during the elicitation process never
reaches the
[elic_cont](https://crewdecisions.github.io/elicitr/reference/elic_cont.md)
object. For three and four points elicitation processes, the order of
the values is checked for each expert. If it is not *min-max-best*, the
values are swapped accordingly and a informative warn is raised.

If the data are imported from *Google Sheets*, `cont_add_data()`
performs additional data cleaning operations. This is relevant when data
are collected with Google Forms because, for example, there could be
multiple submission by the same expert or a different decimal separator
could be used. When data are collected with Google Form, a column with
the date and time is recorded. First, the function checks for multiple
submissions and if present, only the last submission is retained.
Second, the function removes the column with the timestamp. Then it
checks for consistency of the decimal separator, i.e. commas *,* are
replaced with periods *.*. Finally, all columns but the first one (which
contains the names) are forced to numeric.

## See also

Other cont data helpers:
[`cont_get_data()`](https://crewdecisions.github.io/elicitr/reference/cont_get_data.md),
[`cont_sample_data()`](https://crewdecisions.github.io/elicitr/reference/cont_sample_data.md),
[`cont_start()`](https://crewdecisions.github.io/elicitr/reference/cont_start.md),
[`summary.cont_sample()`](https://crewdecisions.github.io/elicitr/reference/summary.cont_sample.md)

## Author

Sergio Vignali and Maude Vernet

## Examples

``` r
# Create the elic_cont object for an elicitation process that estimates 3
# variables, the first for a one point estimation of a positive integer, the
# second for three points estimation of a negative real, and the last for a
# four point estimation of a probability
x <- cont_start(var_names = c("var1", "var2", "var3"),
                var_types = "ZNp",
                elic_types = "134",
                experts = 6)
#> ✔ <elic_cont> object for "Elicitation" correctly initialised

# Add data for the first and second round from a data.frame. Notice that the
# two commands can be piped
my_elicit <- cont_add_data(x, data_source = round_1, round = 1) |>
  cont_add_data(data_source = round_2, round = 2)
#> ✔ Data added to "Round 1" from "data.frame"
#> ✔ Data added to "Round 2" from "data.frame"
my_elicit
#> 
#> ── Elicitation ──
#> 
#> • Variables: "var1", "var2", and "var3"
#> • Variable types: "Z", "N", and "p"
#> • Elicitation types: "1p", "3p", and "4p"
#> • Number of experts: 6
#> • Number of rounds: 2

# Add data for the first and second round from a csv file
files <- list.files(path = system.file("extdata", package = "elicitr"),
                    pattern = "round_",
                    full.names = TRUE)
my_elicit <- cont_add_data(x, data_source = files[1], round = 1) |>
  cont_add_data(data_source = files[2], round = 2)
#> ✔ Data added to "Round 1" from "csv file"
#> ✔ Data added to "Round 2" from "csv file"
my_elicit
#> 
#> ── Elicitation ──
#> 
#> • Variables: "var1", "var2", and "var3"
#> • Variable types: "Z", "N", and "p"
#> • Elicitation types: "1p", "3p", and "4p"
#> • Number of experts: 6
#> • Number of rounds: 2

# Add data for the first and second round from a xlsx file with two sheets
file <- list.files(path = system.file("extdata", package = "elicitr"),
                   pattern = "rounds",
                   full.names = TRUE)
# Using the sheet index
my_elicit <- cont_add_data(x, data_source = file, sheet = 1, round = 1) |>
  cont_add_data(data_source = file, sheet = 2, round = 2)
#> ✔ Data added to "Round 1" from "xlsx file"
#> ✔ Data added to "Round 2" from "xlsx file"
my_elicit
#> 
#> ── Elicitation ──
#> 
#> • Variables: "var1", "var2", and "var3"
#> • Variable types: "Z", "N", and "p"
#> • Elicitation types: "1p", "3p", and "4p"
#> • Number of experts: 6
#> • Number of rounds: 2
# Using the sheet name
my_elicit <- cont_add_data(x, data_source = file,
                           sheet = "Round 1", round = 1) |>
  cont_add_data(data_source = file, sheet = "Round 2", round = 2)
#> ✔ Data added to "Round 1" from "xlsx file"
#> ✔ Data added to "Round 2" from "xlsx file"
my_elicit
#> 
#> ── Elicitation ──
#> 
#> • Variables: "var1", "var2", and "var3"
#> • Variable types: "Z", "N", and "p"
#> • Elicitation types: "1p", "3p", and "4p"
#> • Number of experts: 6
#> • Number of rounds: 2

if (FALSE) { # interactive()
# Add data for the first and second round from Google Sheets
googlesheets4::gs4_deauth()
gs1 <- "12lGIPa-jJOh3fogUDaERmkf04pVpPu9i8SloL2jAdqc"
gs2 <- "1wImcfJYnC9a423jlxZiU_BFKXZpTZ7AIsZSxFtEsBQw"
my_elicit <- cont_add_data(x, data_source = gs1, round = 1) |>
  cont_add_data(data_source = gs2, round = 2)
my_elicit
}
```
