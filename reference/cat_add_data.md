# Add data

**\[experimental\]**

`cat_add_data()` adds data to an
[elic_cat](https://crewdecisions.github.io/elicitr/reference/elic_cat.md)
object from different sources.

## Usage

``` r
cat_add_data(
  x,
  data_source,
  topic,
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
  [elic_cat](https://crewdecisions.github.io/elicitr/reference/elic_cat.md).

- data_source:

  either a [`data.frame`](https://rdrr.io/r/base/data.frame.html) or
  [`tibble`](https://tibble.tidyverse.org/reference/tibble.html), a
  string with the path to a *csv* or *xlsx* file, or anything accepted
  by the
  [read_sheet](https://googlesheets4.tidyverse.org/reference/range_read.html)
  function.

- topic:

  character string that indicates the mechanism to which the data
  belongs.

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
[elic_cat](https://crewdecisions.github.io/elicitr/reference/elic_cat.md)
updated with the data.

## Data format

For each topic, data are expected to have five columns, built as
follows:

- The first column of the data should hold the names of the experts. The
  name of each expert should be repeated as many times as the number of
  categories and options. (i.e. each expert should appear \\number\\
  of\\ categories \cdot number\\ of\\ options\\ times).

- The second column should hold the names of the options considered in
  the study. The name of each option should be repeated as many times as
  the number of categories considered. If you do not use multiple
  options in your study, please input 1 option for all elements.

- The third column should be the names of the categories considered in
  the elicitation. Each block of categories should be repeated as many
  times as the number of options considered.

- The fourth column should be the experts confidence in their own
  estimates (given in percent). Experts should estimate how confident
  they are in their estimates for each block of categories and for each
  option. Therefore, expert confidence estimates should be repeated as
  many times as the number of categories of impact considered for each
  option.

- The final column should be the estimates of each expert for each
  option and category. These estimates should sum up to 1
  (probabilities) (or 100 (percentages)) for each expert and option.

The name of the columns is not important, `cat_add_data()` will
overwrite them according to the following convention:

The first column will be renamed `id`, the second column `category`, the
third column `option`, the fourth column `confidence`, and the fifth
column `estimate`.

Here is an example of data correctly formatted for an elicitation with
five categories and two options (only one expert is shown):

    name         option       category      confidence      estimate
    ----------------------------------------------------------------
    expert 1     option 1     category 1            15          0.08
    expert 1     option 1     category 2            15          0
    expert 1     option 1     category 3            15          0.84
    expert 1     option 1     category 4            15          0.02
    expert 1     option 1     category 5            15          0.06
    expert 1     option 2     category 1            35          0.02
    expert 1     option 2     category 2            35          0.11
    expert 1     option 2     category 3            35          0.19
    expert 1     option 2     category 4            35          0.02
    expert 1     option 2     category 5            35          0.66

## Data cleaning

When data are added to the
[elic_cat](https://crewdecisions.github.io/elicitr/reference/elic_cat.md)
object, first names are standardised by converting capital letters to
lower case, and by removing any whitespaces and punctuation. Then, data
are anonymised by converting names to short sha1 hashes. In this way,
sensible information collected during the elicitation process never
reaches the
[elic_cat](https://crewdecisions.github.io/elicitr/reference/elic_cat.md)
object.

## See also

Other cat data helpers:
[`cat_get_data()`](https://crewdecisions.github.io/elicitr/reference/cat_get_data.md),
[`cat_sample_data()`](https://crewdecisions.github.io/elicitr/reference/cat_sample_data.md),
[`cat_start()`](https://crewdecisions.github.io/elicitr/reference/cat_start.md),
[`summary.cat_sample()`](https://crewdecisions.github.io/elicitr/reference/summary.cat_sample.md)

## Author

Sergio Vignali and Maude Vernet

## Examples

``` r
# Create the elic_cat object for an elicitation process with three topics,
# four options, five categories and a maximum of six experts per topic
my_topics <- c("topic_1", "topic_2", "topic_3")
my_categories <- c("category_1", "category_2", "category_3",
                   "category_4", "category_5")
my_options <- c("option_1", "option_2", "option_3", "option_4")
x <- cat_start(topics = my_topics,
               options = my_options,
               categories = my_categories,
               experts = 6)
#> ✔ <elic_cat> object for "Elicitation" correctly initialised

# Add data for the three topics from a data.frame. Notice that the three
# commands can be piped
my_elicit <- cat_add_data(x,
                          data_source = topic_1,
                          topic = "topic_1") |>
  cat_add_data(data_source = topic_2, topic = "topic_2") |>
  cat_add_data(data_source = topic_3, topic = "topic_3")
#> ℹ Estimates sum to 1. Rescaling to 100.
#> ✔ Data added to Topic "topic_1" from "data.frame"
#> ℹ Estimates sum to 1. Rescaling to 100.
#> ✔ Data added to Topic "topic_2" from "data.frame"
#> ℹ Estimates sum to 1. Rescaling to 100.
#> ✔ Data added to Topic "topic_3" from "data.frame"
my_elicit
#> 
#> ── Elicitation ──
#> 
#> • Categories: "category_1", "category_2", "category_3", "category_4", and
#> "category_5"
#> • Options: "option_1", "option_2", "option_3", and "option_4"
#> • Number of experts: 6
#> • Topics: "topic_1", "topic_2", and "topic_3"
#> • Data available for topics "topic_1", "topic_2", and "topic_3"

# Add data for the first and second round from a csv file
files <- list.files(path = system.file("extdata", package = "elicitr"),
                    pattern = "topic_",
                    full.names = TRUE)
my_elicit <- cat_add_data(x,
                          data_source = files[1],
                          topic = "topic_1") |>
  cat_add_data(data_source = files[2], topic = "topic_2") |>
  cat_add_data(data_source = files[3], topic = "topic_3")
#> ℹ Estimates sum to 1. Rescaling to 100.
#> ✔ Data added to Topic "topic_1" from "csv file"
#> ℹ Estimates sum to 1. Rescaling to 100.
#> ✔ Data added to Topic "topic_2" from "csv file"
#> ℹ Estimates sum to 1. Rescaling to 100.
#> ✔ Data added to Topic "topic_3" from "csv file"
my_elicit
#> 
#> ── Elicitation ──
#> 
#> • Categories: "category_1", "category_2", "category_3", "category_4", and
#> "category_5"
#> • Options: "option_1", "option_2", "option_3", and "option_4"
#> • Number of experts: 6
#> • Topics: "topic_1", "topic_2", and "topic_3"
#> • Data available for topics "topic_1", "topic_2", and "topic_3"

# Add data for the first and second round from a xlsx file with three sheets
file <- list.files(path = system.file("extdata", package = "elicitr"),
                   pattern = "topics",
                   full.names = TRUE)
# Using the sheet index
my_elicit <- cat_add_data(x,
                          data_source = file,
                          sheet = 1,
                          topic = "topic_1") |>
  cat_add_data(data_source = file,
               sheet = 2,
               topic = "topic_2") |>
  cat_add_data(data_source = file,
               sheet = 3,
               topic = "topic_3")
#> ℹ Estimates sum to 1. Rescaling to 100.
#> ✔ Data added to Topic "topic_1" from "xlsx file"
#> ℹ Estimates sum to 1. Rescaling to 100.
#> ✔ Data added to Topic "topic_2" from "xlsx file"
#> ℹ Estimates sum to 1. Rescaling to 100.
#> ✔ Data added to Topic "topic_3" from "xlsx file"
my_elicit
#> 
#> ── Elicitation ──
#> 
#> • Categories: "category_1", "category_2", "category_3", "category_4", and
#> "category_5"
#> • Options: "option_1", "option_2", "option_3", and "option_4"
#> • Number of experts: 6
#> • Topics: "topic_1", "topic_2", and "topic_3"
#> • Data available for topics "topic_1", "topic_2", and "topic_3"
# Using the sheet name
my_elicit <- cat_add_data(x,
                          data_source = file,
                          sheet = "Topic 1",
                          topic = "topic_1") |>
  cat_add_data(data_source = file,
               sheet = "Topic 2",
               topic = "topic_2") |>
  cat_add_data(data_source = file,
               sheet = "Topic 3",
               topic = "topic_3")
#> ℹ Estimates sum to 1. Rescaling to 100.
#> ✔ Data added to Topic "topic_1" from "xlsx file"
#> ℹ Estimates sum to 1. Rescaling to 100.
#> ✔ Data added to Topic "topic_2" from "xlsx file"
#> ℹ Estimates sum to 1. Rescaling to 100.
#> ✔ Data added to Topic "topic_3" from "xlsx file"
my_elicit
#> 
#> ── Elicitation ──
#> 
#> • Categories: "category_1", "category_2", "category_3", "category_4", and
#> "category_5"
#> • Options: "option_1", "option_2", "option_3", and "option_4"
#> • Number of experts: 6
#> • Topics: "topic_1", "topic_2", and "topic_3"
#> • Data available for topics "topic_1", "topic_2", and "topic_3"

if (FALSE) { # interactive()
# Add data for the first and second round from Google Sheets
googlesheets4::gs4_deauth()
gs <- "18VHeHB89P1s-6banaVoqOP-ggFmQZYx-z_31nMffAb8"

# Using the sheet index
my_elicit <- cat_add_data(x,
                          data_source = gs,
                          sheet = 1,
                          topic = "topic_1") |>
  cat_add_data(data_source = gs,
               sheet = 2,
               topic = "topic_2") |>
  cat_add_data(data_source = gs,
               sheet = 3,
               topic = "topic_3")
my_elicit

# (You can also do this using the sheet name)
if (FALSE) { # \dontrun{
my_elicit <- cat_add_data(x, data_source = gs,
                          sheet = "Topic 1",
                          topic = "topic_1") |>
  cat_add_data(data_source = gs,
               sheet = "Topic 2",
               topic = "topic_2") |>
  cat_add_data(data_source = gs,
               sheet = "Topic 3",
               topic = "topic_3")
my_elicit
} # }
}
```
