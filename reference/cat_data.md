# Elicitation data for categorical variables

Simulated data for three topics investigated in an elicitation process.

## Usage

``` r
topic_1

topic_2

topic_3
```

## Format

A data frame with:

- expert:

  Name of the experts (randomly generated).

- category:

  The name of the categories. There are 5 different categories per
  option.

- option:

  The name of the options. Topic 1 and 2 have 4 options, while topic 3
  has 3 options.

- confidence:

  The confidence of the experts given in percentages. One confidence
  value is given for each option.

- estimate:

  Expert estimates given in probabilities.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 120
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 100
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 90
rows and 5 columns.

## Source

Randomly generated numbers and names.
