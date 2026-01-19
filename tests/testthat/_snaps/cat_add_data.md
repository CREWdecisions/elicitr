# Errors

    Code
      cat_add_data("abc", data_source = topic_1, topic = "topic_1")
    Condition
      Error in `cat_add_data()`:
      ! Invalid value for `x`:
      x Argument `x` must be an object of class <elic_cat> and not of class <character>.
      See `elicitr::cat_add_data()`.

---

    Code
      cat_add_data(x, data_source = "test.csv", topic = "topic_1")
    Condition
      Error in `cat_add_data()`:
      x File 'test.csv' doesn't exist!

---

    Code
      cat_add_data(x, data_source = file, topic = "topic_1")
    Condition
      Error in `cat_add_data()`:
      ! Unsupported file extension:
      x The extension of the provided file is ".txt", supported are ".csv" or ".xlsx".
      i See `elicitr::cat_add_data()`.

---

    Code
      cat_add_data(x, data_source = topic_1, topic = 1)
    Condition
      Error in `cat_add_data()`:
      ! Invalid value for `topic`:
      x Argument `topic` must be <character> not <numeric>.
      i See `elicitr::cat_start()`.

---

    Code
      cat_add_data(x, data_source = topic_1, topic = c("topic_1", "topic_2"))
    Condition
      Error in `cat_add_data()`:
      ! Incorrect value for `topic`:
      x Argument `topic` must have length 1 not 2.
      i See `elicitr::cat_add_data()`.

---

    Code
      cat_add_data(x, data_source = topic_1, topic = "topic_3")
    Condition
      Error in `cat_add_data()`:
      ! Invalid value for `topic`:
      x "topic_3" not present in the <elic_cat> object.
      i Available topics: "topic_1" and "topic_2".

---

    Code
      cat_add_data(x, data_source = topic_1[, 1:4], topic = "topic_1")
    Condition
      Error in `cat_add_data()`:
      ! Unexpected number of columns:
      x The imported dataset has 4 columns but 5 are expected.
      i See Data format in `elicitr::cat_add_data()`.

---

    Code
      cat_add_data(x, data_source = y, topic = "topic_1")
    Condition
      Error in `cat_add_data()`:
      ! Unexpected column type:
      x The column "confidence" is not of type "numeric" or "integer" but of type "character".
      i See Data format in `elicitr::cat_add_data()`.

---

    Code
      cat_add_data(x, data_source = y, topic = "topic_1")
    Condition
      Error in `cat_add_data()`:
      ! Unexpected column types:
      x The columns "option" and "category" are not of type "character" but of type "factor".
      i See Data format in `elicitr::cat_add_data()`.

---

    Code
      cat_add_data(x, data_source = y, topic = "topic_1")
    Condition
      Error in `cat_add_data()`:
      ! The number of unique names is greater than the expected number of experts:
      x There are 7 unique names but they should be no more than 6.
      i Check the metadata in the <elic_cat> object.

---

    Code
      cat_add_data(x, data_source = y, topic = "topic_1")
    Condition
      Error in `cat_add_data()`:
      ! The column with the name of the categories contains unexpected values:
      x The value "category_6" is not valid.
      i Check the metadata in the <elic_cat> object.

---

    Code
      cat_add_data(x, data_source = y, topic = "topic_1")
    Condition
      Error in `cat_add_data()`:
      ! The column with the name of the categories contains unexpected values:
      x The values "category_6" and "category_7" are not valid.
      i Check the metadata in the <elic_cat> object.

---

    Code
      cat_add_data(x, data_source = y, topic = "topic_1")
    Condition
      Error in `cat_add_data()`:
      ! The column with the name of the options contains unexpected values:
      x The value "option_5" is not valid.
      i Check the metadata in the <elic_cat> object.

---

    Code
      cat_add_data(x, data_source = y, topic = "topic_1")
    Condition
      Error in `cat_add_data()`:
      ! The column with the name of the options contains unexpected values:
      x The values "option_5" and "option_6" are not valid.
      i Check the metadata in the <elic_cat> object.

---

    Code
      cat_add_data(x, data_source = y, topic = "topic_1")
    Condition
      Error in `cat_add_data()`:
      ! Malformatted dataset:
      x The column containing the expert names is not formatted as expected.
      i See Data format in `elicitr::cat_add_data()`.

---

    Code
      cat_add_data(x, data_source = y, topic = "topic_1")
    Condition
      Error in `cat_add_data()`:
      ! Malformatted dataset:
      x The column containing the categories is not formatted as expected.
      i See Data format in `elicitr::cat_add_data()`.

---

    Code
      cat_add_data(x, data_source = y, topic = "topic_1")
    Condition
      Error in `cat_add_data()`:
      ! Malformatted dataset:
      x The column containing the options is not formatted as expected.
      i See Data format in `elicitr::cat_add_data()`.

---

    Code
      cat_add_data(x, data_source = y, topic = "topic_1")
    Condition
      Error in `cat_add_data()`:
      ! Malformatted dataset:
      x The column containing the confidence values is not formatted as expected.
      i See Data format in `elicitr::cat_add_data()`.

---

    Code
      cat_add_data(x, data_source = y, topic = "topic_1")
    Condition
      Error in `cat_add_data()`:
      ! Invalid value for `estimate`:
      x Estimates of one expert and one option don't sum to 1 or 100.
      * Check id "5ac97e0" for option "option_1": sum 1.91

---

    Code
      cat_add_data(x, data_source = y, topic = "topic_1")
    Condition
      Error in `cat_add_data()`:
      ! Invalid value for `estimate`:
      x Estimates of one/some experts for one/some options don't sum to 1 or 100.
      * Check id "5ac97e0" for option "option_1": sum 1.91
      * Check id "5ac97e0" for option "option_4": sum 1.94
      * Check id "3d32ab9" for option "option_4": sum 1.97

# Accepts all estimates summing to 100

    Code
      out <- cat_add_data(x, data_source = y, topic = "topic_1")
    Message
      v Data added to Topic "topic_1" from "data.frame"

# Accepts some estimates summing to 1 and some to 100

    Code
      out <- cat_add_data(x, data_source = y, topic = "topic_1")
    Message
      i Estimates sum to 100 for some experts/options, and to 1 for others. Rescaling the 1-sums to 100.
      v Data added to Topic "topic_1" from "data.frame"

# Accepts all estimates summing to 1

    Code
      out <- cat_add_data(x, data_source = y, topic = "topic_1")
    Message
      i Estimates sum to 1. Rescaling to 100.
      v Data added to Topic "topic_1" from "data.frame"

# Info

    Code
      out <- cat_add_data(x, data_source = topic_1, topic = "topic_1")
    Message
      i Estimates sum to 1. Rescaling to 100.
      v Data added to Topic "topic_1" from "data.frame"

---

    Code
      out <- cat_add_data(x, data_source = files[[1]], topic = "topic_1")
    Message
      i Estimates sum to 1. Rescaling to 100.
      v Data added to Topic "topic_1" from "csv file"

---

    Code
      out <- cat_add_data(x, data_source = file, topic = "topic_1")
    Message
      i Estimates sum to 1. Rescaling to 100.
      v Data added to Topic "topic_1" from "xlsx file"

---

    Code
      out <- cat_add_data(x, data_source = gs, topic = "topic_1")
    Message
      i Estimates sum to 1. Rescaling to 100.
      v Data added to Topic "topic_1" from "Google Sheets"

