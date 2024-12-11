# Errors

    Code
      elic_cont_start(var_names = "var1", var_types = c("p", "N"), elic_types = "3",
      experts = 3)
    Condition
      Error in `elic_cont_start()`:
      ! Incorrect value for `var_types`:
      x The value provided for `var_types` should be a character string of short codes, i.e. "pN" and not `c("p", "N")`.
      i See "Variable types" in `elicitr::elic_cont_start()`.

---

    Code
      elic_cont_start(var_names = "var1", var_types = "p", elic_types = c("4", "3"),
      experts = 3)
    Condition
      Error in `elic_cont_start()`:
      ! Incorrect value for `elic_types`:
      x The value provided for `elic_types` should be a character string of short codes, i.e. "43" and not `c("4", "3")`.
      i See "Elicitation types" in `elicitr::elic_cont_start()`.

---

    Code
      elic_cont_start(var_names = c("var1", "var2", "var3"), var_types = "pqR",
      elic_types = "1", experts = 3)
    Condition
      Error in `elic_cont_start()`:
      ! Incorrect value for `var_types`:
      x "q" is not in the list of available short codes.
      i See "Variable types" in `elicitr::elic_cont_start()`.

---

    Code
      elic_cont_start(var_names = c("var1", "var2", "var3"), var_types = "apa",
      elic_types = "1", experts = 3)
    Condition
      Error in `elic_cont_start()`:
      ! Incorrect value for `var_types`:
      x "a" is not in the list of available short codes.
      i See "Variable types" in `elicitr::elic_cont_start()`.

---

    Code
      elic_cont_start(var_names = c("var1", "var2", "var3"), var_types = "pqG",
      elic_types = "1", experts = 3)
    Condition
      Error in `elic_cont_start()`:
      ! Incorrect value for `var_types`:
      x "q" and "G" are not in the list of available short codes.
      i See "Variable types" in `elicitr::elic_cont_start()`.

---

    Code
      elic_cont_start(var_names = c("var1", "var2", "var3"), var_types = "p",
      elic_types = "123", experts = 3)
    Condition
      Error in `elic_cont_start()`:
      ! Incorrect value for `elic_types`:
      x "2" is not in the list of available short codes.
      i See "Elicitation types" in `elicitr::elic_cont_start()`.

---

    Code
      elic_cont_start(var_names = c("var1", "var2", "var3"), var_types = "p",
      elic_types = "232", experts = 3)
    Condition
      Error in `elic_cont_start()`:
      ! Incorrect value for `elic_types`:
      x "2" is not in the list of available short codes.
      i See "Elicitation types" in `elicitr::elic_cont_start()`.

---

    Code
      elic_cont_start(var_names = c("var1", "var2", "var3"), var_types = "p",
      elic_types = "1237", experts = 3)
    Condition
      Error in `elic_cont_start()`:
      ! Incorrect value for `elic_types`:
      x "2" and "7" are not in the list of available short codes.
      i See "Elicitation types" in `elicitr::elic_cont_start()`.

---

    Code
      elic_cont_start(var_names = "var1", var_types = "pR", elic_types = "1",
        experts = 3)
    Condition
      Error in `elic_cont_start()`:
      ! Mismatch between function arguments:
      x The number of short codes in `var_types` should be either 1 or equal to the number of elements in `var_names`.
      i See `elicitr::elic_cont_start()`.

---

    Code
      elic_cont_start(var_names = "var1", var_types = "p", elic_types = "13",
        experts = 3)
    Condition
      Error in `elic_cont_start()`:
      ! Mismatch between function arguments:
      x The number of short codes in`elic_types`should be either 1 or equal to the number of elements in `var_names`.
      i See `elicitr::elic_cont_start()`.

---

    Code
      elic_cont_start(var_names = "var1", var_types = "pR", elic_types = "13",
        experts = 3)
    Condition
      Error in `elic_cont_start()`:
      ! Mismatch between function arguments:
      x The number of short codes in `var_types` and `elic_types` should be either 1 or equal to the number of elements in `var_names`.
      i See `elicitr::elic_cont_start()`.

---

    Code
      elic_cont_start(var_names = c("var1", "var2", "var3"), var_types = "pN",
      elic_types = "1", experts = 3)
    Condition
      Error in `elic_cont_start()`:
      ! Mismatch between function arguments:
      x The number of short codes in `var_types` should be either 1 or equal to the number of elements in `var_names`.
      i See `elicitr::elic_cont_start()`.

---

    Code
      elic_cont_start(var_names = c("var1", "var2", "var3"), var_types = "p",
      elic_types = "13", experts = 3)
    Condition
      Error in `elic_cont_start()`:
      ! Mismatch between function arguments:
      x The number of short codes in`elic_types`should be either 1 or equal to the number of elements in `var_names`.
      i See `elicitr::elic_cont_start()`.

---

    Code
      elic_cont_start(var_names = c("var1", "var2", "var3"), var_types = "pN",
      elic_types = "13", experts = 3)
    Condition
      Error in `elic_cont_start()`:
      ! Mismatch between function arguments:
      x The number of short codes in `var_types` and `elic_types` should be either 1 or equal to the number of elements in `var_names`.
      i See `elicitr::elic_cont_start()`.

---

    Code
      elic_cont_start(var_names = c("var1", "var2", "var3"), var_types = "pN",
      elic_types = "1344", experts = 3)
    Condition
      Error in `elic_cont_start()`:
      ! Mismatch between function arguments:
      x The number of short codes in `var_types` and `elic_types` should be either 1 or equal to the number of elements in `var_names`.
      i See `elicitr::elic_cont_start()`.

---

    Code
      elic_cont_start(var_names = "var1", var_types = "p", elic_types = "1", experts = "3")
    Condition
      Error in `elic_cont_start()`:
      ! Incorrect value for `experts`:
      x Argument `experts` must be <numeric> not <character>.
      See `elicitr::elic_cont_start()`.

---

    Code
      elic_cont_start(var_names = "var1", var_types = "p", elic_types = "1", experts = 1:
        2)
    Condition
      Error in `elic_cont_start()`:
      ! Incorrect value for `experts`:
      x Argument `experts` must be a single number not a vector of length 2.
      See `elicitr::elic_cont_start()`.

# Info

    Code
      x <- elic_cont_start(var_names = c("var1", "var2"), var_types = "pR",
      elic_types = "43", experts = 3)
    Message
      v <elic_cont> object for "Elicitation" correctly initialised

# Output

    structure(list(var_names = c("var1", "var2"), var_types = c("p", 
    "R"), elic_types = c("4p", "3p"), experts = 3, data = list(round_1 = NULL, 
        round_2 = NULL)), class = "elic_cont", title = "Elicitation")

