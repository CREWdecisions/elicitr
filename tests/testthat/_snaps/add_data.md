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

# Raises warns

    Code
      y <- elic_add_data(x, data_source = round_1[1:4, ], round = 1, verbose = FALSE)
    Condition
      Warning:
      ! Dataset for "Round 1" has 2 rows but are expected 6 experts, added 2 rows with "NAs".
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

