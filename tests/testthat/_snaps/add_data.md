# Raises errors 

    Code
      elic_add_data(x, data_source = "test.csv", round = 1)
    Condition
      Error in `elic_add_data()`:
      x File 'test.csv' doesn't exist!

---

    Code
      elic_add_data(x, data_source = file, round = 1)
    Condition
      Error in `elic_add_data()`:
      ! Unsupported file extension:
      x The extension of the provided file is ".txt", supported are ".csv" or ".xlsx".
      i See `elicitr::elic_add_data()`.

---

    Code
      elic_add_data(x, data_source = round_1, round = 2)
    Condition
      Error in `elic_add_data()`:
      ! Data for "Round 1" are not present:
      i Data for "Round 2" can be added only after those for "Round 1".

---

    Code
      elic_add_data(y, data_source = round_1, round = 1)
    Condition
      Error in `elic_add_data()`:
      ! Data for "Round 1" already present:
      i Set `overwrite = TRUE` if you want to overwrite them.

---

    Code
      elic_add_data(x, data_source = round_1[, -1], round = 1)
    Condition
      Error in `elic_add_data()`:
      ! Unexpected number of columns:
      x The imported dataset has 8 columns but are expected to be 9.
      i See Data Format in `elicitr::elic_add_data()`.

---

    Code
      elic_add_data(x, data_source = rbind(round_1, round_1), round = 1, verbose = FALSE)
    Condition
      Error in `elic_add_data()`:
      ! Incorrect number of rows in dataset:
      x The dataset for "Round 1" contains 12 rows but are expected estimates from 6 experts.
      i Check raw data or modify the <elicit> object with `obj$experts = 12` and then use `elicitr::elic_add_data()` with `overwrite = TRUE`.

---

    Code
      elic_add_data("abc", data_source = round_1, round = 1)
    Condition
      Error in `elic_add_data()`:
      ! Argument `x` must be an object of class <elicit>:
      x An object of class <character> is invalid.
      See `elicitr::elic_add_data()`.

---

    Code
      elic_add_data(x, data_source = round_1, round = 3)
    Condition
      Error in `elic_add_data()`:
      ! Argument `round` can be only 1 or 2:
      x The value 3 is invalid.
      i See `elicitr::elic_add_data()`.

---

    Code
      elic_add_data(x, data_source = round_1, round = 0)
    Condition
      Error in `elic_add_data()`:
      ! Argument `round` can be only 1 or 2:
      x The value 0 is invalid.
      i See `elicitr::elic_add_data()`.

---

    Code
      out <- elic_add_data(y, data_source = z, round = 2, verbose = FALSE)
    Condition
      Error in `elic_add_data()`:
      ! Dataset for "Round 2" has 2 <id> not present in "Round 1". Automatic match between the two datasets is not possible:
      x The <id> not present in "Round 1" are "06d2130" and "3b842bc".
      i Check raw data.

---

    Code
      out <- elic_add_data(y, data_source = z, round = 2, verbose = FALSE)
    Condition
      Warning:
      ! Dataset for "Round 2" has 4 rows but are expected 6 experts. Missing <id> have been filled with "NAs".
      i Check raw data and if you want to update the dataset use `elicitr::elic_add_data()` with `overwrite = TRUE`.
      Error in `elic_add_data()`:
      ! Dataset for "Round 2" has 1 <id> not present in "Round 1" and 2 entries with NAs. Automatic match between the two datasets is not possible:
      x The <id> not present in "Round 1" are "06d2130".
      i Check raw data.

---

    Code
      out <- elic_add_data(y, data_source = z, round = 2, verbose = FALSE)
    Condition
      Warning:
      ! Dataset for "Round 2" has 4 rows but are expected 6 experts. Missing <id> have been filled with "NAs".
      i Check raw data and if you want to update the dataset use `elicitr::elic_add_data()` with `overwrite = TRUE`.
      Error in `elic_add_data()`:
      ! Dataset for "Round 2" has 2 <id> not present in "Round 1". Automatic match between the two datasets is not possible:
      x The <id> not present in "Round 1" are "06d2130" and "3b842bc".
      i Check raw data.

---

    Code
      out <- elic_add_data(y, data_source = z, round = 2, verbose = FALSE)
    Condition
      Warning:
      ! Dataset for "Round 2" has 4 rows but are expected 6 experts. Missing <id> have been filled with "NAs".
      i Check raw data and if you want to update the dataset use `elicitr::elic_add_data()` with `overwrite = TRUE`.
      Error in `elic_add_data()`:
      ! Dataset for "Round 2" has 4 <id> not present in "Round 1". Automatic match between the two datasets is not possible:
      x The <id> not present in "Round 1" are "06d2130", "6c074fa", "7aa9fc1", and "3b842bc".
      i Check raw data.

# Raises warns

    Code
      y <- elic_add_data(x, data_source = round_1[1:4, ], round = 1, verbose = FALSE)
    Condition
      Warning:
      ! The dataset for "Round 1" has 4 rows but are expected 6 experts, added 2 rows with "NAs".
      i Check raw data and if you want to update the dataset use `elicitr::elic_add_data()` with `overwrite = TRUE`.

---

    Code
      out <- elic_add_data(y, data_source = z, round = 2, verbose = FALSE)
    Condition
      Warning:
      ! Dataset for "Round 2" has "1" <id> not present in "Round 1". This is considered a typo by the expert "3cc9c29" in "Round 2" and its value has been replaced.
      i Check raw data and if you want to update the dataset in "Round 2" use `elicitr::elic_add_data()` with `overwrite = TRUE`.

---

    Code
      out <- elic_add_data(y, data_source = z, round = 2, verbose = FALSE)
    Condition
      Warning:
      ! Dataset for "Round 2" has 4 rows but are expected 6 experts. Missing <id> have been filled with "NAs".
      i Check raw data and if you want to update the dataset use `elicitr::elic_add_data()` with `overwrite = TRUE`.

---

    Code
      out <- elic_add_data(y, data_source = round_2[1:5, ], round = 2, verbose = FALSE)
    Condition
      Warning:
      ! Dataset for "Round 2" has 5 rows but are expected 6 experts. Missing <id> have been filled with "NAs".
      i Check raw data and if you want to update the dataset use `elicitr::elic_add_data()` with `overwrite = TRUE`.
    Message
      i The dataset in "Round 2" has 2 <id> not present in "Round 1". These <id> have been added to "Round 1" with NA values.

---

    Code
      out <- elic_add_data(y, data_source = round_2[1:4, ], round = 2, verbose = FALSE)
    Condition
      Warning:
      ! Dataset for "Round 2" has 4 rows but are expected 6 experts. Missing <id> have been filled with "NAs".
      i Check raw data and if you want to update the dataset use `elicitr::elic_add_data()` with `overwrite = TRUE`.
      Warning:
      The dataset in "Round 2" has 2 <id> not present in "Round 1". These <id> have been added to "Round 1" with "NA" values but could be typos in the raw data.
      i Check raw data and if you want to update the dataset in "Round 2" use `elicitr::elic_add_data()` with `overwrite = TRUE`.

# Raises info

    Code
      out <- elic_add_data(y, data_source = round_2, round = 2, verbose = FALSE)
    Message
      i The dataset in "Round 2" has 1 <id> not present in "Round 1". This <id> has been added to "Round 1" with NA values.

---

    Code
      out <- elic_add_data(y, data_source = round_2, round = 2, verbose = FALSE)
    Message
      i The dataset in "Round 2" has 3 <id> not present in "Round 1". These <id> have been added to "Round 1" with NA values.

# Output format

    Code
      z
    Message
      
      -- Elicitation --
      
      * Variables: "cat", "dog", and "fish"
      * Variable types: "Z", "N", and "p"
      * Elicitation types: "1p", "3p", and "4p"
      * Number of experts: 6
      * Number of rounds: 2
      
      Round 1
    Output
      # A tibble: 6 x 9
        id     cat_best dog_min dog_max dog_best fish_min fish_max fish_best fish_conf
        <chr>     <int>   <int>   <int>    <int>    <dbl>    <dbl>     <dbl>     <int>
      1 5ac97~        1      20      24       22     0.43     0.83      0.73        94
      2 e5120~        0       7      10        9     0.67     0.87      0.77        69
      3 e78cb~        0      10      15       12     0.65     0.95      0.85        79
      4 9fafb~       -7       4      12        9     0.44     0.84      0.64        89
      5 3cc9c~       -5      13      18       16     0.38     0.88      0.68        56
      6 3d32a~        3      20      26       25     0.35     0.85      0.65        89
    Message
      
      Round 2
    Output
      # A tibble: 6 x 9
        id     cat_best dog_min dog_max dog_best fish_min fish_max fish_best fish_conf
        <chr>     <int>   <int>   <int>    <int>    <dbl>    <dbl>     <dbl>     <int>
      1 5ac97~        0      11      18       15     0.52     0.82      0.72        72
      2 e5120~       -2      14      18       15     0.55     0.85      0.75        97
      3 e78cb~       -2      15      21       18     0.62     0.82      0.72        72
      4 9fafb~       -4      11      15       12     0.52     0.82      0.72        88
      5 3cc9c~        1      15      20       17     0.58     0.78      0.68        92
      6 3d32a~        1      18      23       20     0.66     0.86      0.76        80

