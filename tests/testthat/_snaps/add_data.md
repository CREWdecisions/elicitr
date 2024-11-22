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

