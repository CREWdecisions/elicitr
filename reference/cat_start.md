# Start elicitation

**\[experimental\]**

`cat_start()` initialises an
[elic_cat](https://crewdecisions.github.io/elicitr/reference/elic_cat.md)
object which stores important metadata for the data collected during the
elicitation process of categorical data.

## Usage

``` r
cat_start(
  topics,
  options,
  categories,
  experts,
  ...,
  title = "Elicitation",
  verbose = TRUE
)
```

## Arguments

- topics:

  character vector with the names of the topics.

- options:

  character vector with the names of all options investigated. See
  Options for more.

- categories:

  character vector with the names of the categories. See Categories for
  more.

- experts:

  numeric, indicating the maximum number of experts participating in the
  elicitation process for one topic. See Experts for more.

- ...:

  Unused arguments, included only for future extensions of the function.

- title:

  character, used to bind a name to the object.

- verbose:

  logical, if `TRUE` it prints informative messages.

## Value

An object of class
[elic_cat](https://crewdecisions.github.io/elicitr/reference/elic_cat.md)
binding metadata related to the elicitation process. These metadata are
used by other functions to validate the correctness of the provided
data.

## Experts

The expert parameter is a number indicating the maximum number of
experts taking part in the elicitation of one of the investigated
topics. The number and IDs of experts can differ between the topics.

## Options

The option parameter is a character vector containing the names of all
the options investigated in the elicitation. However, not all options
have to be investigated in every topic. If you do not use multiple
options in your study, please still input 1 option which will apply to
all topics, categories and experts (e.g., options = "none").

## Categories

Categories are inherited between topics. A minimum of two categories are
needed. If only one category is investigated, please refer to the
functions for the elicitation of continuous data (e.g.
[cont_start](https://crewdecisions.github.io/elicitr/reference/cont_start.md)).

## References

Hemming, V., Burgman, M. A., Hanea, A. M., McBride, M. F., & Wintle, B.
C. (2018). A practical guide to structured expert elicitation using the
IDEA protocol. Methods in Ecology and Evolution, 9(1), 169–180.
<https://doi.org/10.1111/2041-210X.12857> Vernet, M., Trask, A.E.,
Andrews, C.E., Ewen, J.E., Medina, S., Moehrenschlager, A. & Canessa, S.
(2024) Assessing invasion risks using EICAT‐based expert elicitation:
application to a conservation translocation. Biological Invasions,
26(8), 2707–2721. <https://doi.org/10.1007/s10530-024-03341-2>

## See also

Other cat data helpers:
[`cat_add_data()`](https://crewdecisions.github.io/elicitr/reference/cat_add_data.md),
[`cat_get_data()`](https://crewdecisions.github.io/elicitr/reference/cat_get_data.md),
[`cat_sample_data()`](https://crewdecisions.github.io/elicitr/reference/cat_sample_data.md),
[`summary.cat_sample()`](https://crewdecisions.github.io/elicitr/reference/summary.cat_sample.md)

## Author

Sergio Vignali and Maude Vernet

## Examples

``` r
# Create the elic_cat object for an elicitation process over 2 topics, 3
# options, 3 categories per options, and a maximum number of 8 experts per
# topic
my_categories <- c("category_1", "category_2", "category_3")
my_elicit <- cat_start(topics = c("topic_1","topic_2"),
                       options = c("option_1", "option_2", "option_3"),
                       categories = my_categories,
                       experts = 8)
#> ✔ <elic_cat> object for "Elicitation" correctly initialised
my_elicit
#> 
#> ── Elicitation ──
#> 
#> • Categories: "category_1", "category_2", and "category_3"
#> • Options: "option_1", "option_2", and "option_3"
#> • Number of experts: 8
#> • Topics: "topic_1" and "topic_2"
#> • Data available for 0 topics

# A title can be added to bind a name to the object:
my_elicit <- cat_start(topics = c("topic_1","topic_2"),
                       options = c("option_1", "option_2", "option_3"),
                       categories = my_categories,
                       experts = 8,
                       title = "My elicitation")
#> ✔ <elic_cat> object for "My elicitation" correctly initialised
my_elicit
#> 
#> ── My elicitation ──
#> 
#> • Categories: "category_1", "category_2", and "category_3"
#> • Options: "option_1", "option_2", and "option_3"
#> • Number of experts: 8
#> • Topics: "topic_1" and "topic_2"
#> • Data available for 0 topics
```
