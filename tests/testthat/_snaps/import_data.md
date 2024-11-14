# Raises errors 

    Code
      import_data("test.csv", var_names = "var1", var_types = "p", elic_type = "1")
    Condition
      Error in `import_data()`:
      x File 'test.csv' doesn't exist!

---

    Code
      import_data(file, var_names = "var1", var_types = "p", elic_type = "1")
    Condition
      Error in `import_data()`:
      ! Unsupported file extension:
      i See `elicitr::import_data()`.
      x The file extension is .txt, supported are .csv or .xlsx

---

    Code
      import_data("abc", var_names = c("var1", "var2", "var3"), var_types = "pqR",
      elic_type = "1")
    Condition
      Error in `import_data()`:
      ! Incorrect value for `var_types`:
      i See `elicitr::import_data()`.
      x The incorrect short code is q.

---

    Code
      import_data("abc", var_names = c("var1", "var2", "var3"), var_types = "pqG",
      elic_type = "1")
    Condition
      Error in `import_data()`:
      ! Incorrect value for `var_types`:
      i See `elicitr::import_data()`.
      x The incorrect short codes are q and G.

---

    Code
      import_data("abc", var_names = c("var1", "var2", "var3"), var_types = "p",
      elic_type = "123")
    Condition
      Error in `import_data()`:
      ! Incorrect value for `elic_types`:
      i See `elicitr::import_data()`.
      x The incorrect short code is 2.

---

    Code
      import_data("abc", var_names = c("var1", "var2", "var3"), var_types = "p",
      elic_type = "1237")
    Condition
      Error in `import_data()`:
      ! Incorrect value for `elic_types`:
      i See `elicitr::import_data()`.
      x The incorrect short codes are 2 and 7.

---

    Code
      import_data("abs", var_names = c("var1"), var_types = c("pR"), elic_type = c(
        "1"))
    Condition
      Error in `import_data()`:
      ! Mismatch between function arguments:
      i See `elicitr::import_data()`.
      x You provided 1 value for `var_names` and 2 short codes for `var_types`

---

    Code
      import_data("abs", var_names = c("var1"), var_types = c("p"), elic_type = c(
        "13"))
    Condition
      Error in `import_data()`:
      ! Mismatch between function arguments:
      i See `elicitr::import_data()`.
      x You provided 1 value for `var_names` and 2 short codes for `elic_types`

---

    Code
      import_data("abs", var_names = c("var1"), var_types = c("pR"), elic_type = c(
        "13"))
    Condition
      Error in `import_data()`:
      ! Mismatch between function arguments:
      i See `elicitr::import_data()`.
      x You provided 1 value for `var_names`, 2 short codes for `var_types`, and 2 short codes for `elic_types`

---

    Code
      import_data("abs", var_names = c("var1", "var2", "var3"), var_types = c("pN"),
      elic_type = c("1"))
    Condition
      Error in `import_data()`:
      ! Mismatch between function arguments:
      i See `elicitr::import_data()`.
      x You provided 3 values for `var_names` and 2 short codes for `var_types`

---

    Code
      import_data("abs", var_names = c("var1", "var2", "var3"), var_types = c("p"),
      elic_type = c("13"))
    Condition
      Error in `import_data()`:
      ! Mismatch between function arguments:
      i See `elicitr::import_data()`.
      x You provided 3 values for `var_names` and 2 short codes for `elic_types`

---

    Code
      import_data("abs", var_names = c("var1", "var2", "var3"), var_types = c("pN"),
      elic_type = c("13"))
    Condition
      Error in `import_data()`:
      ! Mismatch between function arguments:
      i See `elicitr::import_data()`.
      x You provided 3 values for `var_names`, 2 short codes for `var_types`, and 2 short codes for `elic_types`

---

    Code
      import_data("abs", var_names = c("var1", "var2", "var3"), var_types = c("pN"),
      elic_type = c("1344"))
    Condition
      Error in `import_data()`:
      ! Mismatch between function arguments:
      i See `elicitr::import_data()`.
      x You provided 3 values for `var_names`, 2 short codes for `var_types`, and 4 short codes for `elic_types`

---

    Code
      import_data(dplyr::select(elicit, -1), var_names = c("var1", "var2", "var3"),
      var_types = "p", elic_types = "134")
    Message
      v Function arguments are correct
    Condition
      Error in `import_data()`:
      ! Unexpected number of columns:
      i See Data Format in `elicitr::import_data()`.
      x The imported dataset has 8 columns but are expected to be 9.

---

    Code
      import_data(elicit, var_names = c("var1", "var2", "var5"), var_types = "p",
      elic_types = "134")
    Message
      v Function arguments are correct
    Condition
      Error in `import_data()`:
      ! Incorrect column names:
      i See Data Format in `elicitr::import_data()`.
      x The imported dataset has id, var1_best, var2_min, var2_max, var2_best, var3_min, var3_max, var3_best, and var3_conf but it is expected id, var1_best, var2_min, var2_max, var2_best, var5_min, var5_max, var5_best, and var5_conf.

