# Errors 

    Code
      elic_cont_add_data(x, data_source = "test.csv", round = 1)
    Condition
      Error in `elic_cont_add_data()`:
      x File 'test.csv' doesn't exist!

---

    Code
      elic_cont_add_data(x, data_source = file, round = 1)
    Condition
      Error in `elic_cont_add_data()`:
      ! Unsupported file extension:
      x The extension of the provided file is ".txt", supported are ".csv" or ".xlsx".
      i See `elicitr::elic_cont_add_data()`.

---

    Code
      elic_cont_add_data(x, data_source = round_1, round = 2)
    Condition
      Error in `elic_cont_add_data()`:
      ! Data for "Round 1" are not present:
      i Data for "Round 2" can be added only after those for "Round 1".

---

    Code
      elic_cont_add_data(y, data_source = round_1, round = 1)
    Condition
      Error in `elic_cont_add_data()`:
      ! Data for "Round 1" already present:
      i Set `overwrite = TRUE` if you want to overwrite them.

---

    Code
      elic_cont_add_data(x, data_source = round_1[, -1], round = 1)
    Condition
      Error in `elic_cont_add_data()`:
      ! Unexpected number of columns:
      x The imported dataset has 8 columns but 9 are expected.
      i See Data Format in `elicitr::elic_cont_add_data()`.

---

    Code
      elic_cont_add_data(x, data_source = rbind(round_1, round_1), round = 1,
      verbose = FALSE)
    Condition
      Error in `elic_cont_add_data()`:
      ! Incorrect number of rows in dataset:
      x The dataset for "Round 1" contains 12 rows but expects estimates from 6 experts.
      i Check raw data or modify the <elic_cont> object by setting the number of experts to 12 with `obj$experts = 12`.

---

    Code
      elic_cont_add_data(y, data_source = rbind(round_2, round_2), round = 2,
      verbose = FALSE)
    Condition
      Error in `elic_cont_add_data()`:
      ! Incorrect number of rows in dataset:
      x The dataset for "Round 2" contains 12 rows but expects estimates from 6 experts.
      i Check raw data.

---

    Code
      elic_cont_add_data("abc", data_source = round_1, round = 1)
    Condition
      Error in `elic_cont_add_data()`:
      ! Invalid value for `x`:
      x Argument `x` must be an object of class <elic_cont> and not of class <character>.
      See `elicitr::elic_cont_add_data()`.

---

    Code
      elic_cont_add_data(x, data_source = round_1, round = 3)
    Condition
      Error in `elic_cont_add_data()`:
      ! Incorrect value for `round`:
      x `round` can only be 1 or 2.
      i See `elicitr::elic_cont_add_data()`.

---

    Code
      elic_cont_add_data(x, data_source = round_1, round = 0)
    Condition
      Error in `elic_cont_add_data()`:
      ! Incorrect value for `round`:
      x `round` can only be 1 or 2.
      i See `elicitr::elic_cont_add_data()`.

---

    Code
      out <- elic_cont_add_data(y, data_source = z, round = 2, verbose = FALSE)
    Condition
      Error in `elic_cont_add_data()`:
      ! Dataset for "Round 2" has 2 <id> not present in "Round 1". Automatic match between the two datasets is not possible:
      x The <id> not present in "Round 1" are "06d2130" and "3b842bc".
      i Check raw data to identify the mismatch.

---

    Code
      out <- elic_cont_add_data(y, data_source = z, round = 2, verbose = FALSE)
    Condition
      Warning:
      ! The dataset for "Round 2" has 4 rows but expects 6 experts. NAs added to missing <id>.
      i Check raw data and if you want to update the dataset use `elicitr::elic_cont_add_data()` with `overwrite = TRUE`.
      Error in `elic_cont_add_data()`:
      ! Dataset for "Round 2" has 1 <id> not present in "Round 1" and 2 entries with NAs. Automatic match between the two datasets is not possible:
      x The <id> not present in "Round 1" is "06d2130".
      i Check raw data.

---

    Code
      out <- elic_cont_add_data(y, data_source = z, round = 2, verbose = FALSE)
    Condition
      Warning:
      ! The dataset for "Round 2" has 4 rows but expects 6 experts. NAs added to missing <id>.
      i Check raw data and if you want to update the dataset use `elicitr::elic_cont_add_data()` with `overwrite = TRUE`.
      Error in `elic_cont_add_data()`:
      ! Dataset for "Round 2" has 2 <id> not present in "Round 1". Automatic match between the two datasets is not possible:
      x The <id> not present in "Round 1" are "06d2130" and "3b842bc".
      i Check raw data to identify the mismatch.

---

    Code
      out <- elic_cont_add_data(y, data_source = z, round = 2, verbose = FALSE)
    Condition
      Warning:
      ! The dataset for "Round 2" has 4 rows but expects 6 experts. NAs added to missing <id>.
      i Check raw data and if you want to update the dataset use `elicitr::elic_cont_add_data()` with `overwrite = TRUE`.
      Error in `elic_cont_add_data()`:
      ! Dataset for "Round 2" has 4 <id> not present in "Round 1". Automatic match between the two datasets is not possible:
      x The <id> not present in "Round 1" are "06d2130", "6c074fa", "7aa9fc1", and "3b842bc".
      i Check raw data to identify the mismatch.

---

    Code
      out <- elic_cont_add_data(y, data_source = z, round = 2, verbose = FALSE)
    Condition
      Warning:
      ! The dataset for "Round 2" has 4 rows but expects 6 experts. NAs added to missing <id>.
      i Check raw data and if you want to update the dataset use `elicitr::elic_cont_add_data()` with `overwrite = TRUE`.
      Error in `elic_cont_add_data()`:
      ! Impossible to combine "Round 1" and "Round 2" datasets:
      x "Round 2" has 4 <id> not present in "Round 1" which has only 2 "NA" rows.
      i Check raw data and use `elicitr::elic_cont_add_data()` to add the dataset after manual corrections.

# Warnings

    Code
      y <- elic_cont_add_data(x, data_source = round_1[1:4, ], round = 1, verbose = FALSE)
    Condition
      Warning:
      ! The dataset for "Round 1" has 4 rows but expects 6 experts. NAs added to missing <id>.
      i Check raw data and if you want to update the dataset use `elicitr::elic_cont_add_data()` with `overwrite = TRUE`.

---

    Code
      out <- elic_cont_add_data(y, data_source = z, round = 2, verbose = FALSE)
    Condition
      Warning:
      ! Dataset for "Round 2" has 1 <id> not present in "Round 1". This is considered a typo by expert "3cc9c29" in "Round 2" and the <id> has been replaced.
      i Check raw data and if you want to update the dataset in "Round 2" use `elicitr::elic_cont_add_data()` with `overwrite = TRUE`.

---

    Code
      out <- elic_cont_add_data(y, data_source = z, round = 2, verbose = FALSE)
    Condition
      Warning:
      ! The dataset for "Round 2" has 4 rows but expects 6 experts. NAs added to missing <id>.
      i Check raw data and if you want to update the dataset use `elicitr::elic_cont_add_data()` with `overwrite = TRUE`.

---

    Code
      out <- elic_cont_add_data(y, data_source = round_2[1:5, ], round = 2, verbose = FALSE)
    Condition
      Warning:
      ! The dataset for "Round 2" has 5 rows but expects 6 experts. NAs added to missing <id>.
      i Check raw data and if you want to update the dataset use `elicitr::elic_cont_add_data()` with `overwrite = TRUE`.
    Message
      i The dataset in "Round 2" has 2 <id> not present in "Round 1". These <id> have been added to "Round 1" with NA values.

---

    Code
      out <- elic_cont_add_data(y, data_source = round_2[1:4, ], round = 2, verbose = FALSE)
    Condition
      Warning:
      ! The dataset for "Round 2" has 4 rows but expects 6 experts. NAs added to missing <id>.
      i Check raw data and if you want to update the dataset use `elicitr::elic_cont_add_data()` with `overwrite = TRUE`.
      Warning:
      The dataset in "Round 2" has 2 <id> not present in "Round 1". These <id> have been added to "Round 1" with "NA" values but could be typos in the raw data.
      i Check raw data and if you want to update the dataset in "Round 2" use `elicitr::elic_cont_add_data()` with `overwrite = TRUE`.

---

    Code
      out <- elic_cont_add_data(x, data_source = z, round = 1, verbose = FALSE)
    Condition
      Warning:
      ! var2 of <id> "5ac97e0" and "e78cbf4" reordered following the order "min-max-best".
      Warning:
      ! var3 of <id> "3d32ab9" reordered following the order "min-max-best".

# Info

    Code
      out <- elic_cont_add_data(y, data_source = round_2, round = 2, verbose = FALSE)
    Message
      i The dataset in "Round 2" has 1 <id> not present in "Round 1". This <id> has been added to "Round 1" with NA values.

---

    Code
      out <- elic_cont_add_data(y, data_source = round_2, round = 2, verbose = FALSE)
    Message
      i The dataset in "Round 2" has 3 <id> not present in "Round 1". These <id> have been added to "Round 1" with NA values.

---

    Code
      out <- elic_cont_add_data(x, data_source = round_1, round = 1)
    Message
      v Data added to "Round 1" from "data.frame"

---

    Code
      out <- elic_cont_add_data(x, data_source = files[[1]], round = 1)
    Message
      v Data added to "Round 1" from "csv file"

---

    Code
      out <- elic_cont_add_data(x, data_source = file, round = 1)
    Message
      v Data added to "Round 1" from "xlsx file"

# Output

    Code
      z
    Message
      
      -- Elicitation --
      
      * Variables: "cat", "dog", and "fish"
      * Variable types: "Z", "N", and "p"
      * Elicitation types: "1p", "3p", and "4p"
      * Number of experts: 6
      * Number of rounds: 2

