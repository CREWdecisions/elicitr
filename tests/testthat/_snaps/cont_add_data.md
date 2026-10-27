# Errors 

    Code
      cont_add_data(x, data_source = "test.csv", round = 1)
    Condition
      Error in `cont_add_data()`:
      x File 'test.csv' doesn't exist!
      i See `elicitr::cont_add_data()` for more information.

---

    Code
      cont_add_data(x, data_source = file, round = 1)
    Condition
      Error in `cont_add_data()`:
      ! Unsupported file extension:
      x The extension of the provided file is ".txt", supported are ".csv" or ".xlsx".
      i See `elicitr::cont_add_data()`.

---

    Code
      cont_add_data(x, data_source = round_1, round = 2)
    Condition
      Error in `cont_add_data()`:
      ! "Round 1" is empty.
      x Wrong value provided for `round`.
        Data can only be loaded in "Round 2" after data was loaded in "Round 1".
      i See `elicitr::cont_add_data()` for more information.

---

    Code
      cont_add_data(y, data_source = round_1, round = 1)
    Condition
      Error in `cont_add_data()`:
      ! Data for "Round 1" already present:
      x Set `overwrite = TRUE` if you want to overwrite them.
      i See `elicitr::cont_add_data()` for more information.

---

    Code
      cont_add_data(x, data_source = round_1[, -2], round = 1)
    Condition
      Error in `cont_add_data()`:
      ! Unexpected number of columns:
      x The imported dataset has 8 columns but 9 are expected.
        The first column in the data should be the expert name, followed by the elicited variables as:
      One-point elicitation variables should each have 1 column, three-point elicitation variables should each have 2 columns and four-point elicitation variables should each have 4 columns.
      i See Data format in `elicitr::cont_add_data()`.

---

    Code
      cont_add_data(x, data_source = rbind(round_1, round_1), round = 1, verbose = FALSE)
    Condition
      Error in `cont_add_data()`:
      ! Incorrect number of rows in dataset:
      x The dataset for "Round 1" contains 12 rows but expects estimates from 6 experts.
      i Check raw data or modify the <elic_cont> object by setting the number of experts to 12 in `elicitr::cont_start()` and load the data again.
        See `elicitr::cont_add_data()` for more information.

---

    Code
      cont_add_data(y, data_source = rbind(round_2, round_2), round = 2, verbose = FALSE)
    Condition
      Error in `cont_add_data()`:
      ! Incorrect number of rows in dataset:
      x The dataset for "Round 2" contains 12 rows but expects estimates from 6 experts.
      i Check raw data or modify the <elic_cont> object by setting the number of experts to 12 in `elicitr::cont_start()` and load the data for "Round 1" and "Round 2" again.
        See `elicitr::cont_add_data()` for more information.

---

    Code
      cont_add_data("abc", data_source = round_1, round = 1)
    Condition
      Error in `cont_add_data()`:
      ! Invalid value for `x`:
      x Argument `x` must be a `?elicitr::elic_cont()` object created with `elicitr::cont_start()`.
        `x` should be of class <elic_cont> and not of class <character>.
      i See `elicitr::cont_add_data()`.

---

    Code
      cont_add_data(x, data_source = round_1, round = 3)
    Condition
      Error in `cont_add_data()`:
      ! Incorrect value for `round`:
      x Argument `round` can only be equal to 1 (for "Round 1") or to 2 (for "Round 2").
      i See `elicitr::cont_add_data()`.

---

    Code
      cont_add_data(x, data_source = round_1, round = 0)
    Condition
      Error in `cont_add_data()`:
      ! Incorrect value for `round`:
      x Argument `round` can only be equal to 1 (for "Round 1") or to 2 (for "Round 2").
      i See `elicitr::cont_add_data()`.

---

    Code
      cont_add_data(x, data_source = round_1, round = round_1)
    Condition
      Error in `cont_add_data()`:
      ! Incorrect value for `round`:
      x Argument `round` can only be equal to 1 (for "Round 1") or to 2 (for "Round 2").
      i See `elicitr::cont_add_data()`.

---

    Code
      out <- cont_add_data(y, data_source = z, round = 2, verbose = FALSE)
    Condition
      Error in `cont_add_data()`:
      ! Automatic match between the dataset in "Round 1" and "Round 2" is not possible:
      x "Round 2" has 2 <id> not present in "Round 1".
        The <id> not present in "Round 1" are "06d2130" and "3b842bc".
      i Check raw data. See `elicitr::cont_add_data()` for more information.

---

    Code
      out <- cont_add_data(y, data_source = z, round = 2, verbose = FALSE)
    Condition
      Warning:
      ! The dataset for "Round 2" has 4 rows but expects 6 experts. NAs added to missing <id>.
      i Check raw data and if you want to update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.
      Error in `cont_add_data()`:
      ! Automatic match between the dataset in "Round 1" and "Round 2" is not possible:
      x "Round 2" has 1 <id> not present in "Round 1" and 2 entries with NAs.
        The <id> not present in "Round 1" is "06d2130".
      i Check raw data. See `elicitr::cont_add_data()` for more information.

---

    Code
      out <- cont_add_data(y, data_source = z, round = 2, verbose = FALSE)
    Condition
      Warning:
      ! The dataset for "Round 2" has 4 rows but expects 6 experts. NAs added to missing <id>.
      i Check raw data and if you want to update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.
      Error in `cont_add_data()`:
      ! Automatic match between the dataset in "Round 1" and "Round 2" is not possible:
      x "Round 2" has 2 <id> not present in "Round 1".
        The <id> not present in "Round 1" are "06d2130" and "3b842bc".
      i Check raw data. See `elicitr::cont_add_data()` for more information.

---

    Code
      out <- cont_add_data(y, data_source = z, round = 2, verbose = FALSE)
    Condition
      Warning:
      ! The dataset for "Round 2" has 4 rows but expects 6 experts. NAs added to missing <id>.
      i Check raw data and if you want to update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.
      Error in `cont_add_data()`:
      ! Automatic match between the dataset in "Round 1" and "Round 2" is not possible:
      x "Round 2" has 4 <id> not present in "Round 1".
        The <id> not present in "Round 1" are "06d2130", "6c074fa", "7aa9fc1", and "3b842bc".
      i Check raw data. See `elicitr::cont_add_data()` for more information.

---

    Code
      out <- cont_add_data(y, data_source = z, round = 2, verbose = FALSE)
    Condition
      Warning:
      ! The dataset for "Round 2" has 4 rows but expects 6 experts. NAs added to missing <id>.
      i Check raw data and if you want to update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.
      Error in `cont_add_data()`:
      ! Impossible to combine "Round 1" and "Round 2" datasets:
      x "Round 2" has 4 <id> not present in "Round 1".
        This brings the total number of expert to 8 while `experts` was set to 6 in `elicitr::cont_start()`.
      i Check raw data. Either modify the metadata using `elicitr::cont_start()` with `experts` = 8 and reupload the data for both rounds,
      or modify the dataset manually and upload it again with `elicitr::cont_add_data()`.

---

    Code
      out <- cont_add_data(z, data_source = y, round = 1)
    Condition
      Warning:
      ! Expert "5ac97e0" did not reply to variable "var2".
      i If this is a mistake and you want to change and update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.
    Message
      v Data added to "Round 1" from "data.frame"

---

    Code
      cont_add_data(z, data_source = y, round = 1)
    Condition
      Error in `cont_add_data()`:
      ! Invalid raw data:
      x Expert "5ac97e0" replied to only one part of the variable "var2".
      i Experts cannot report estimates for only part of a variable.
        Fix raw data. For more information, see `cont_add_data()`.

---

    Code
      cont_add_data(x, data_source = y, round = 1)
    Condition
      Error in `cont_add_data()`:
      ! Incorrect `var_types`:
      x The argument `var_types` of "var1" was set to "Z" in the metadata but variable "var1" contains estimates that are not round numbers.
      i Check the raw data. If this is a mistake, recreate the metadata with `elicitr::cont_start()` (make sure to change the argument `var_types` accordingly), and reload the data.

---

    Code
      out <- cont_add_data(x, data_source = y, round = 1)
    Condition
      Warning:
      ! Expert "5ac97e0" did not reply to variable "var1".
      i If this is a mistake and you want to change and update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.
    Message
      v Data added to "Round 1" from "data.frame"

---

    Code
      cont_add_data(x, data_source = y, round = 1)
    Condition
      Error in `cont_add_data()`:
      ! Incorrect `var_types`:
      x The argument `var_types` of "var2" was set to "N" in the metadata but variable "var2" contains estimates that are not positive round numbers or 0.
      i Check the raw data. If this is a mistake, recreate the metadata with `elicitr::cont_start()` (make sure to change the argument `var_types` accordingly), and reload the data.

---

    Code
      out <- cont_add_data(x, data_source = y, round = 1)
    Condition
      Warning:
      ! Expert "5ac97e0" did not reply to variable "var2".
      i If this is a mistake and you want to change and update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.
    Message
      v Data added to "Round 1" from "data.frame"

---

    Code
      cont_add_data(y, data_source = round_1, round = 1)
    Condition
      Error in `cont_add_data()`:
      ! Incorrect `var_types`:
      x The argument `var_types` of "var1" was set to "z" in the metadata but variable "var1" contains estimates that are not negative round numbers.
      i Check the raw data. If this is a mistake, recreate the metadata with `elicitr::cont_start()` (make sure to change the argument `var_types` accordingly), and reload the data.

---

    Code
      cont_add_data(y, data_source = z, round = 1)
    Condition
      Error in `cont_add_data()`:
      ! Incorrect `var_types`:
      x The argument `var_types` of "var3" was set to "s" in the metadata but variable "var3" contains estimates that are not positive round numbers or 0.
      i Check the raw data. If this is a mistake, recreate the metadata with `elicitr::cont_start()` (make sure to change the argument `var_types` accordingly), and reload the data.

---

    Code
      out <- cont_add_data(y, data_source = z, round = 1)
    Condition
      Warning:
      ! Expert "5ac97e0" did not reply to variable "var2".
      i If this is a mistake and you want to change and update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.
    Message
      v Data added to "Round 1" from "data.frame"

---

    Code
      cont_add_data(y, data_source = round_1, round = 1)
    Condition
      Error in `cont_add_data()`:
      ! Incorrect `var_types`:
      x The argument `var_types` of "var3" was set to "r" in the metadata but variable "var3" contains estimates that are not not strictly negative numbers.
      i Check the raw data. If this is a mistake, recreate the metadata with `elicitr::cont_start()` (make sure to change the argument `var_types` accordingly), and reload the data.

---

    Code
      out <- cont_add_data(y, data_source = z, round = 1)
    Condition
      Warning:
      ! Expert "5ac97e0" did not reply to variable "var2".
      i If this is a mistake and you want to change and update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.
      Warning:
      ! var2 of <id> "e51202e", "e78cbf4", "9fafbee", "3cc9c29", and "3d32ab9" reordered following the order "min-max-best".
    Message
      v Data added to "Round 1" from "data.frame"

---

    Code
      cont_add_data(x, data_source = y, round = 1)
    Condition
      Error in `cont_add_data()`:
      ! Incorrect `var_types`:
      x The argument `var_types` of "var3" was set to "p" in the metadata but variable "var3" contains estimates that are not in the range [0,1].
      i Check the raw data. If this is a mistake, recreate the metadata with `elicitr::cont_start()` (make sure to change the argument `var_types` accordingly), and reload the data.

---

    Code
      cont_add_data(x, data_source = y, round = 1)
    Condition
      Error in `cont_add_data()`:
      ! Incorrect `var_types`:
      x The argument `var_types` of "var3" was set to "p" in the metadata but variable "var3" contains estimates that are not in the range [0,1].
      i Check the raw data. If this is a mistake, recreate the metadata with `elicitr::cont_start()` (make sure to change the argument `var_types` accordingly), and reload the data.

---

    Code
      out <- cont_add_data(x, data_source = y, round = 1)
    Condition
      Warning:
      ! Expert "5ac97e0" did not reply to variable "var3".
      i If this is a mistake and you want to change and update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.
    Message
      v Data added to "Round 1" from "data.frame"

---

    Code
      cont_add_data(x, data_source = y, round = 1)
    Condition
      Error in `cont_add_data()`:
      ! Incorrect confidence values:
      x The variable "var3" contains confidence estimates not in the range (50, 100].
      i Check the raw data. For more information, see `cont_add_data()`.

---

    Code
      cont_add_data(x, data_source = y, round = 1)
    Condition
      Error in `cont_add_data()`:
      ! Incorrect confidence values:
      x The variable "var3" contains confidence estimates not in the range (50, 100].
      i Check the raw data. For more information, see `cont_add_data()`.

---

    Code
      cont_add_data(x, data_source = y, round = 1)
    Condition
      Error in `cont_add_data()`:
      ! Invalid raw data:
      x Expert "5ac97e0" replied to only one part of the variable "var3".
      i Experts cannot report estimates for only part of a variable.
        Fix raw data. For more information, see `cont_add_data()`.

---

    Code
      cont_add_data(x, data_source = y, round = 1)
    Condition
      Error in `cont_add_data()`:
      ! Data is empty.
      x Please add data
      i To test whether the data is correctly formed, add a test response.

---

    Code
      cont_add_data(x, data_source = y, round = 1)
    Condition
      Error in `cont_add_data()`:
      ! Malformated data.
      x The first column does not contain the expert IDs.
      i See Data format in `elicitr::cont_add_data()`

---

    Code
      cont_add_data(x, data_source = y, round = 1)
    Condition
      Error in `cont_add_data()`:
      ! Malformated data.
      x The first column does not contain the expert IDs.
      i See Data format in `elicitr::cont_add_data()`

---

    Code
      out <- cont_add_data(z, data_source = y, round = 1)
    Condition
      Warning:
      ! Expert "5ac97e0" did not reply to variable "var1".
      ! Expert "5ac97e0" did not reply to variable "var2".
      i If this is a mistake and you want to change and update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.
      Error in `cont_add_data()`:
      ! Incorrect `var_types`:
      x The argument `var_types` of "var1" was set to "N" in the metadata but variable "var1" contains estimates that are not positive round numbers or 0.
      x The argument `var_types` of "var2" was set to "p" in the metadata but variable "var2" contains estimates that are not in the range [0,1].
      x The argument `var_types` of "var3" was set to "r" in the metadata but variable "var3" contains estimates that are not not strictly negative numbers.
      i Check the raw data. If this is a mistake, recreate the metadata with `elicitr::cont_start()` (make sure to change the argument `var_types` accordingly), and reload the data.

---

    Code
      out <- cont_add_data(z, data_source = y, round = 1)
    Condition
      Warning:
      ! Expert "5ac97e0" did not reply to variable "var1".
      ! Expert "5ac97e0" did not reply to variable "var2".
      i If this is a mistake and you want to change and update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.
      Error in `cont_add_data()`:
      ! Incorrect confidence values:
      x The variable "var1" contains confidence estimates not in the range (50, 100].
      x The variable "var3" contains confidence estimates not in the range (50, 100].
      i Check the raw data. For more information, see `cont_add_data()`.

---

    Code
      out <- cont_add_data(z, data_source = y, round = 1)
    Condition
      Warning:
      ! Expert "5ac97e0" did not reply to variable "var1".
      i If this is a mistake and you want to change and update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.
      Error in `cont_add_data()`:
      ! Invalid raw data:
      x Experts "e78cbf4" and "9fafbee" replied to only one part of the variable "var2".
      x Experts "e78cbf4" and "3cc9c29" replied to only one part of the variable "var3".
      i Experts cannot report estimates for only part of a variable.
        Fix raw data. For more information, see `cont_add_data()`.

# Warnings

    Code
      y <- cont_add_data(x, data_source = round_1[1:4, ], round = 1, verbose = FALSE)
    Condition
      Warning:
      ! The dataset for "Round 1" has 4 rows but expects 6 experts. NAs added to missing <id>.
      i Check raw data and if you want to update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.

---

    Code
      out <- cont_add_data(y, data_source = z, round = 2, verbose = FALSE)
    Condition
      Warning:
      ! Dataset for "Round 2" has 1 <id> not present in "Round 1". This is considered a typo by expert "3cc9c29" in "Round 2" and the <id> has been replaced.
      i Check raw data and if you want to update the dataset in "Round 2" use `elicitr::cont_add_data()` with `overwrite = TRUE`.

---

    Code
      out <- cont_add_data(y, data_source = z, round = 2, verbose = FALSE)
    Condition
      Warning:
      ! The dataset for "Round 2" has 4 rows but expects 6 experts. NAs added to missing <id>.
      i Check raw data and if you want to update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.

---

    Code
      out <- cont_add_data(y, data_source = round_2[1:5, ], round = 2, verbose = FALSE)
    Condition
      Warning:
      ! The dataset for "Round 2" has 5 rows but expects 6 experts. NAs added to missing <id>.
      i Check raw data and if you want to update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.
    Message
      i The dataset in "Round 2" has 2 <id> not present in "Round 1". These <id> have been added to "Round 1" with NA values.

---

    Code
      out <- cont_add_data(y, data_source = round_2[1:4, ], round = 2, verbose = FALSE)
    Condition
      Warning:
      ! The dataset for "Round 2" has 4 rows but expects 6 experts. NAs added to missing <id>.
      i Check raw data and if you want to update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.
      Warning:
      The dataset in "Round 2" has 2 <id> not present in "Round 1". These <id> have been added to "Round 1" with "NA" values but could be typos in the raw data.
      i Check raw data and if you want to update the dataset in "Round 2" use `elicitr::cont_add_data()` with `overwrite = TRUE`.

---

    Code
      out <- cont_add_data(x, data_source = z, round = 1, verbose = FALSE)
    Condition
      Warning:
      ! var2 of <id> "5ac97e0" and "e78cbf4" reordered following the order "min-max-best".
      Warning:
      ! var3 of <id> "3d32ab9" reordered following the order "min-max-best".

# Info

    Code
      out <- cont_add_data(y, data_source = round_2, round = 2, verbose = FALSE)
    Message
      i The dataset in "Round 2" has 1 <id> not present in "Round 1". This <id> has been added to "Round 1" with NA values.

---

    Code
      out <- cont_add_data(y, data_source = round_2, round = 2, verbose = FALSE)
    Message
      i The dataset in "Round 2" has 3 <id> not present in "Round 1". These <id> have been added to "Round 1" with NA values.

---

    Code
      out <- cont_add_data(x, data_source = round_1, round = 1)
    Message
      v Data added to "Round 1" from "data.frame"

---

    Code
      out <- cont_add_data(x, data_source = round_1, round = 1, anonymise = FALSE)
    Message
      v Data added to "Round 1" from "data.frame"

---

    Code
      out <- cont_add_data(x, data_source = files[[1]], round = 1)
    Message
      v Data added to "Round 1" from "csv file"

---

    Code
      out <- cont_add_data(x, data_source = files[[1]], round = 1, anonymise = FALSE)
    Message
      v Data added to "Round 1" from "csv file"

---

    Code
      out <- cont_add_data(x, data_source = file, round = 1)
    Message
      v Data added to "Round 1" from "xlsx file"

---

    Code
      out <- cont_add_data(x, data_source = file, round = 1, anonymise = FALSE)
    Message
      v Data added to "Round 1" from "xlsx file"

---

    Code
      out <- googlesheets4::with_gs4_quiet(cont_add_data(x, data_source = gs, round = 1))
    Message
      v Data added to "Round 1" from "Google Sheets"

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

# Round harmonisation preserves missing estimates

    Code
      out <- cont_add_data(obj, y, round = 1, anonymise = FALSE, verbose = FALSE)
    Condition
      Warning:
      ! The dataset for "Round 1" has 5 rows but expects 6 experts. NAs added to missing <id>.
      i Check raw data and if you want to update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.

---

    Code
      out <- cont_add_data(out, z, round = 2, anonymise = FALSE, verbose = FALSE)
    Condition
      Warning:
      ! Expert "Mar'Quasa Banks" did not reply to variable "var2".
      i If this is a mistake and you want to change and update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.
    Message
      i The dataset in "Round 2" has 1 <id> not present in "Round 1". This <id> has been added to "Round 1" with NA values.

---

    Code
      out <- cont_add_data(obj, y, round = 1, anonymise = FALSE, verbose = FALSE)
    Condition
      Warning:
      ! The dataset for "Round 1" has 5 rows but expects 6 experts. NAs added to missing <id>.
      i Check raw data and if you want to update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.

---

    Code
      out <- cont_add_data(out, z, round = 2, anonymise = FALSE, verbose = FALSE)
    Condition
      Warning:
      ! Expert "Mar'Quasa Banks" did not reply to variable "var2".
      i If this is a mistake and you want to change and update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.
      Warning:
      ! The dataset for "Round 2" has 5 rows but expects 6 experts. NAs added to missing <id>.
      i Check raw data and if you want to update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.
      Warning:
      The dataset in "Round 2" has 1 <id> not present in "Round 1". This <id> has been added to "Round 1" with "NA" values but could be typo in the raw data.
      i Check raw data and if you want to update the dataset in "Round 2" use `elicitr::cont_add_data()` with `overwrite = TRUE`.

# All experts absent from Round 2 receive missing estimates

    Code
      out <- cont_add_data(obj, y, round = 1, anonymise = FALSE, verbose = FALSE)
    Condition
      Warning:
      ! The dataset for "Round 1" has 5 rows but expects 6 experts. NAs added to missing <id>.
      i Check raw data and if you want to update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.

---

    Code
      out <- cont_add_data(out, z, round = 2, anonymise = FALSE, verbose = FALSE)
    Condition
      Warning:
      ! Expert "Mar'Quasa Banks" did not reply to variable "var2".
      i If this is a mistake and you want to change and update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.
      Warning:
      ! The dataset for "Round 2" has 4 rows but expects 6 experts. NAs added to missing <id>.
      i Check raw data and if you want to update the dataset use `elicitr::cont_add_data()` with `overwrite = TRUE`.
      Warning:
      The dataset in "Round 2" has 1 <id> not present in "Round 1". This <id> has been added to "Round 1" with "NA" values but could be typo in the raw data.
      i Check raw data and if you want to update the dataset in "Round 2" use `elicitr::cont_add_data()` with `overwrite = TRUE`.

