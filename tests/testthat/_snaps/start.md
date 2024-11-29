# Errors

    Code
      elic_start(var_names = "var1", var_types = c("p", "N"), elic_types = "3",
      experts = 3)
    Condition
      Error in `elic_start()`:
      ! Incorrect value for `var_types`:
      x The value provided for `var_types` is a character vector of length 2 but should be a single string with short codes.
      i See "Variable Types" in `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = "var1", var_types = "p", elic_types = c("4", "3"),
      experts = 3)
    Condition
      Error in `elic_start()`:
      ! Incorrect value for `elic_types`:
      x The value provided for `elic_types` is a character vector of length 2 but should be a single string with short codes.
      i See "Elicitation types" in `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = c("var1", "var2", "var3"), var_types = "pqR",
      elic_types = "1", experts = 3)
    Condition
      Error in `elic_start()`:
      ! Incorrect value for `var_types`:
      x The incorrect short code is "q".
      i See "Variable Types" in `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = c("var1", "var2", "var3"), var_types = "apa",
      elic_types = "1", experts = 3)
    Condition
      Error in `elic_start()`:
      ! Incorrect value for `var_types`:
      x The incorrect short code is "a".
      i See "Variable Types" in `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = c("var1", "var2", "var3"), var_types = "pqG",
      elic_types = "1", experts = 3)
    Condition
      Error in `elic_start()`:
      ! Incorrect value for `var_types`:
      x The incorrect short codes are "q" and "G".
      i See "Variable Types" in `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = c("var1", "var2", "var3"), var_types = "p", elic_types = "123",
      experts = 3)
    Condition
      Error in `elic_start()`:
      ! Incorrect value for `elic_types`:
      x The incorrect short code is "2".
      i See "Elicitation types" in `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = c("var1", "var2", "var3"), var_types = "p", elic_types = "232",
      experts = 3)
    Condition
      Error in `elic_start()`:
      ! Incorrect value for `elic_types`:
      x The incorrect short code is "2".
      i See "Elicitation types" in `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = c("var1", "var2", "var3"), var_types = "p", elic_types = "1237",
      experts = 3)
    Condition
      Error in `elic_start()`:
      ! Incorrect value for `elic_types`:
      x The incorrect short codes are "2" and "7".
      i See "Elicitation types" in `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = c("var1"), var_types = c("pR"), elic_types = c("1"),
      experts = 3)
    Condition
      Error in `elic_start()`:
      ! Mismatch between function arguments:
      x You provided 1 value for `var_names` and 2 short codes for `var_types`.
      i See `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = c("var1"), var_types = c("p"), elic_types = c("13"),
      experts = 3)
    Condition
      Error in `elic_start()`:
      ! Mismatch between function arguments:
      x You provided 1 value for `var_names` and 2 short codes for `elic_types`.
      i See `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = c("var1"), var_types = c("pR"), elic_types = c("13"),
      experts = 3)
    Condition
      Error in `elic_start()`:
      ! Mismatch between function arguments:
      x You provided 1 value for `var_names`, 2 short codes for `var_types`, and 2 short codes for `elic_types`.
      i See `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = c("var1", "var2", "var3"), var_types = c("pN"),
      elic_types = c("1"), experts = 3)
    Condition
      Error in `elic_start()`:
      ! Mismatch between function arguments:
      x You provided 3 values for `var_names` and 2 short codes for `var_types`.
      i See `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = c("var1", "var2", "var3"), var_types = c("p"),
      elic_types = c("13"), experts = 3)
    Condition
      Error in `elic_start()`:
      ! Mismatch between function arguments:
      x You provided 3 values for `var_names` and 2 short codes for `elic_types`.
      i See `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = c("var1", "var2", "var3"), var_types = c("pN"),
      elic_types = c("13"), experts = 3)
    Condition
      Error in `elic_start()`:
      ! Mismatch between function arguments:
      x You provided 3 values for `var_names`, 2 short codes for `var_types`, and 2 short codes for `elic_types`.
      i See `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = c("var1", "var2", "var3"), var_types = c("pN"),
      elic_types = c("1344"), experts = 3)
    Condition
      Error in `elic_start()`:
      ! Mismatch between function arguments:
      x You provided 3 values for `var_names`, 2 short codes for `var_types`, and 4 short codes for `elic_types`.
      i See `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = "var1", var_types = "p", elic_types = "1", experts = "3")
    Condition
      Error in `elic_start()`:
      ! Incorrect value for `experts`:
      x The value provided for `experts` is a character, it should be numeric.
      See `elicitr::elic_start()`.

---

    Code
      elic_start(var_names = "var1", var_types = "p", elic_types = "1", experts = 1:2)
    Condition
      Error in `elic_start()`:
      ! Incorrect value for `experts`:
      x The value provided for `experts` has length 2, it should be a single number.
      See `elicitr::elic_start()`.

# Output format

    Code
      x <- elic_start(var_names = c("var1", "var2"), var_types = "pR", elic_types = "43",
      experts = 3)
    Message
      v `elicit` object for "Elicitation" correctly initialised

---

    structure(list(var_names = c("var1", "var2"), var_types = c("p", 
    "R"), elic_types = c("4p", "3p"), experts = 3, data = list(round_1 = NULL, 
        round_2 = NULL)), class = "elicit", title = "Elicitation")

