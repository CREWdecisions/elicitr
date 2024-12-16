# Errors

    Code
      elic_cat_add_data("abc", data_source = mechanism_1, mechanism = "mechanism_1")
    Condition
      Error in `elic_cat_add_data()`:
      ! Invalid value for `x`:
      x Argument `x` must be an object of class <elic_cat> and not of class <character>.
      See `elicitr::elic_cat_add_data()`.

---

    Code
      elic_cat_add_data(x, data_source = "test.csv", mechanism = "mechanism_1")
    Condition
      Error in `elic_cat_add_data()`:
      x File 'test.csv' doesn't exist!

---

    Code
      elic_cat_add_data(x, data_source = file, mechanism = "mechanism_1")
    Condition
      Error in `elic_cat_add_data()`:
      ! Unsupported file extension:
      x The extension of the provided file is ".txt", supported are ".csv" or ".xlsx".
      i See `elicitr::elic_cont_add_data()`.

---

    Code
      elic_cat_add_data(x, data_source = mechanism_1, mechanism = 1)
    Condition
      Error in `elic_cat_add_data()`:
      ! Invalid value for `mechanism`:
      x Argument `mechanism` must be <character> not <numeric>.
      i See `elicitr::elic_cat_start()`.

---

    Code
      elic_cat_add_data(x, data_source = mechanism_1, mechanism = c("mechanism_1",
        "mechanism_2"))
    Condition
      Error in `elic_cat_add_data()`:
      ! Incorrect value for `mechanism`:
      x Argument `mechanism` must have length 1 not 2.
      i See `elicitr::elic_cat_add_data()`.

---

    Code
      elic_cat_add_data(x, data_source = mechanism_1, mechanism = "mechanism_3")
    Condition
      Error in `elic_cat_add_data()`:
      ! Invalid value for `mechanism`:
      x "mechanism_3" not present in the <elic_cat> object.
      i Available mechanisms: "mechanism_1" and "mechanism_2".

---

    Code
      elic_cat_add_data(x, data_source = mechanism_1[, 1:4], mechanism = "mechanism_1")
    Condition
      Error in `elic_cat_add_data()`:
      ! Unexpected number of columns:
      x The imported dataset has 4 columns but 5 are expected.
      i See Data format in `elicitr::elic_cat_add_data()`.

---

    Code
      elic_cat_add_data(x, data_source = y, mechanism = "mechanism_1")
    Condition
      Error in `elic_cat_add_data()`:
      ! Unexpected column type:
      x The column "confidence" is not of type "numeric" or "integer" but of type "character".
      i See Data format in `elicitr::elic_cat_add_data()`.

---

    Code
      elic_cat_add_data(x, data_source = y, mechanism = "mechanism_1")
    Condition
      Error in `elic_cat_add_data()`:
      ! Unexpected column types:
      x The columns "level" and "site" are not of type "character" but of type "factor".
      i See Data format in `elicitr::elic_cat_add_data()`.

---

    Code
      elic_cat_add_data(x, data_source = y, mechanism = "mechanism_1")
    Condition
      Error in `elic_cat_add_data()`:
      ! The number of unique names is greater than the expected number of experts:
      x There are 7 unique names but they should be no more than 6.
      i Check the metadata in the <elic_cat> object.

---

    Code
      elic_cat_add_data(x, data_source = y, mechanism = "mechanism_1")
    Condition
      Error in `elic_cat_add_data()`:
      ! The column with the name of the levels contains unexpected values:
      x The value "level_6" is not valid.
      i Check the metadata in the <elic_cat> object.

---

    Code
      elic_cat_add_data(x, data_source = y, mechanism = "mechanism_1")
    Condition
      Error in `elic_cat_add_data()`:
      ! The column with the name of the levels contains unexpected values:
      x The values "level_6" and "level_7" are not valid.
      i Check the metadata in the <elic_cat> object.

---

    Code
      elic_cat_add_data(x, data_source = y, mechanism = "mechanism_1")
    Condition
      Error in `elic_cat_add_data()`:
      ! The column with the name of the sites contains unexpected values:
      x The value "site_5" is not valid.
      i Check the metadata in the <elic_cat> object.

---

    Code
      elic_cat_add_data(x, data_source = y, mechanism = "mechanism_1")
    Condition
      Error in `elic_cat_add_data()`:
      ! The column with the name of the sites contains unexpected values:
      x The values "site_5" and "site_6" are not valid.
      i Check the metadata in the <elic_cat> object.

---

    Code
      elic_cat_add_data(x, data_source = y, mechanism = "mechanism_1")
    Condition
      Error in `elic_cat_add_data()`:
      ! Malformatted dataset:
      x The column containing the expert names is not formatted as expected.
      i See Data format in `elicitr::elic_cat_add_data()`.

---

    Code
      elic_cat_add_data(x, data_source = y, mechanism = "mechanism_1")
    Condition
      Error in `elic_cat_add_data()`:
      ! Malformatted dataset:
      x The column containing the levels is not formatted as expected.
      i See Data format in `elicitr::elic_cat_add_data()`.

---

    Code
      elic_cat_add_data(x, data_source = y, mechanism = "mechanism_1")
    Condition
      Error in `elic_cat_add_data()`:
      ! Malformatted dataset:
      x The column containing the sites is not formatted as expected.
      i See Data format in `elicitr::elic_cat_add_data()`.

---

    Code
      elic_cat_add_data(x, data_source = y, mechanism = "mechanism_1")
    Condition
      Error in `elic_cat_add_data()`:
      ! Malformatted dataset:
      x The column containing the confidence values is not formatted as expected.
      i See Data format in `elicitr::elic_cat_add_data()`.

---

    Code
      elic_cat_add_data(x, data_source = y, mechanism = "mechanism_1")
    Condition
      Error in `elic_cat_add_data()`:
      ! Invalid value for `estimate`:
      x Estimates of one expert and one site don't sum to 1.
      * Check id "5ac97e0" for site "site_1": sum 1.91

---

    Code
      elic_cat_add_data(x, data_source = y, mechanism = "mechanism_1")
    Condition
      Error in `elic_cat_add_data()`:
      ! Invalid value for `estimate`:
      x Estimates of one/some experts for one/some sites don't sum to 1.
      * Check id "5ac97e0" for site "site_1": sum 1.91
      * Check id "5ac97e0" for site "site_4": sum 1.94
      * Check id "3d32ab9" for site "site_4": sum 1.96

# Info

    Code
      out <- elic_cat_add_data(x, data_source = mechanism_1, mechanism = "mechanism_1")
    Message
      v Data added to Mechanism "mechanism_1" from "data.frame"

---

    Code
      out <- elic_cat_add_data(x, data_source = files[[1]], mechanism = "mechanism_1")
    Message
      v Data added to Mechanism "mechanism_1" from "csv file"

---

    Code
      out <- elic_cat_add_data(x, data_source = file, mechanism = "mechanism_1")
    Message
      v Data added to Mechanism "mechanism_1" from "xlsx file"

---

    Code
      out <- elic_cat_add_data(x, data_source = gs, mechanism = "mechanism_1")
    Message
      v Data added to Mechanism "mechanism_1" from "Google Sheets"

