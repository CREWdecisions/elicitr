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
      x The dataset contains 12 rows but are expected estimates from 6 experts.
      i Check raw data or modify the <elicit> object with `obj$experts = 12` and then use `elicitr::elic_add_data()` with `overwrite = TRUE`.

