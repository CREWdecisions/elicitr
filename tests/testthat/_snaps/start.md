# Errors

    Code
      elic_start(var_names = "var1", var_types = c("p", "N"), elic_types = "3")
    Condition
      Error in `elic_start()`:
      ! Incorrect value for `var_types`:
      x The value provided for `var_types` is a character vector of length 2 but should be a single string with short codes.
      i See "Variable Types" in `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = "var1", var_types = "p", elic_types = c("4", "3"))
    Condition
      Error in `elic_start()`:
      ! Incorrect value for `elic_types`:
      x The value provided for `elic_types` is a character vector of length 2 but should be a single string with short codes.
      i See "Elicitation types" in `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = c("var1", "var2", "var3"), var_types = "pqR", elic_type = "1")
    Condition
      Error in `elic_start()`:
      ! Incorrect value for `var_types`:
      x The incorrect short code is "q".
      i See "Variable Types" in `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = c("var1", "var2", "var3"), var_types = "apa", elic_type = "1")
    Condition
      Error in `elic_start()`:
      ! Incorrect value for `var_types`:
      x The incorrect short code is "a".
      i See "Variable Types" in `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = c("var1", "var2", "var3"), var_types = "pqG", elic_type = "1")
    Condition
      Error in `elic_start()`:
      ! Incorrect value for `var_types`:
      x The incorrect short codes are "q" and "G".
      i See "Variable Types" in `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = c("var1", "var2", "var3"), var_types = "p", elic_type = "123")
    Condition
      Error in `elic_start()`:
      ! Incorrect value for `elic_types`:
      x The incorrect short code is "2".
      i See "Elicitation types" in `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = c("var1", "var2", "var3"), var_types = "p", elic_type = "232")
    Condition
      Error in `elic_start()`:
      ! Incorrect value for `elic_types`:
      x The incorrect short code is "2".
      i See "Elicitation types" in `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = c("var1", "var2", "var3"), var_types = "p", elic_type = "1237")
    Condition
      Error in `elic_start()`:
      ! Incorrect value for `elic_types`:
      x The incorrect short codes are "2" and "7".
      i See "Elicitation types" in `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = c("var1"), var_types = c("pR"), elic_type = c("1"))
    Condition
      Error in `elic_start()`:
      ! Mismatch between function arguments:
      x You provided 1 value for `var_names` and 2 short codes for `var_types`.
      i See `elicitr::read_data()`.

---

    Code
      elic_start(var_names = c("var1"), var_types = c("p"), elic_type = c("13"))
    Condition
      Error in `elic_start()`:
      ! Mismatch between function arguments:
      x You provided 1 value for `var_names` and 2 short codes for `elic_types`.
      i See `elicitr::read_data()`.

---

    Code
      elic_start(var_names = c("var1"), var_types = c("pR"), elic_type = c("13"))
    Condition
      Error in `elic_start()`:
      ! Mismatch between function arguments:
      x You provided 1 value for `var_names`, 2 short codes for `var_types`, and 2 short codes for `elic_types`.
      i See `elicitr::read_data()`.

---

    Code
      elic_start(var_names = c("var1", "var2", "var3"), var_types = c("pN"),
      elic_type = c("1"))
    Condition
      Error in `elic_start()`:
      ! Mismatch between function arguments:
      x You provided 3 values for `var_names` and 2 short codes for `var_types`.
      i See `elicitr::read_data()`.

---

    Code
      elic_start(var_names = c("var1", "var2", "var3"), var_types = c("p"),
      elic_type = c("13"))
    Condition
      Error in `elic_start()`:
      ! Mismatch between function arguments:
      x You provided 3 values for `var_names` and 2 short codes for `elic_types`.
      i See `elicitr::read_data()`.

---

    Code
      elic_start(var_names = c("var1", "var2", "var3"), var_types = c("pN"),
      elic_type = c("13"))
    Condition
      Error in `elic_start()`:
      ! Mismatch between function arguments:
      x You provided 3 values for `var_names`, 2 short codes for `var_types`, and 2 short codes for `elic_types`.
      i See `elicitr::read_data()`.

---

    Code
      elic_start(var_names = c("var1", "var2", "var3"), var_types = c("pN"),
      elic_type = c("1344"))
    Condition
      Error in `elic_start()`:
      ! Mismatch between function arguments:
      x You provided 3 values for `var_names`, 2 short codes for `var_types`, and 4 short codes for `elic_types`.
      i See `elicitr::read_data()`.

# Output format

    Code
      x <- elic_start(var_names = c("var1", "var2"), var_types = "pR", elic_types = "43")
    Message
      v `elicit` object correctly initialised

---

    structure(list(var_names = c("var1", "var2"), var_types = c("p", 
    "R"), elic_types = c("4p", "3p"), data = list(round_1 = NULL, 
        round_2 = NULL)), class = "elicit", title = "Elicitation")

