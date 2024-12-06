# Errors

    Code
      elic_cont_get_data("abc", round = 1)
    Condition
      Error in `elic_cont_get_data()`:
      ! Argument `x` must be an object of class <elic_cont>:
      x An object of class <character> is invalid.
      See `elicitr::elic_cont_get_data()`.

---

    Code
      elic_cont_get_data(obj, round = 3)
    Condition
      Error in `elic_cont_get_data()`:
      ! Incorrect value for `round`:
      x `round` can only be 1 or 2.
      i See `elicitr::elic_cont_get_data()`.

---

    Code
      elic_cont_get_data(obj, round = 1, var = "var5")
    Condition
      Error in `elic_cont_get_data()`:
      ! Argument `var` can be only a vector with a combination of "var1", "var2", and "var3" or "all":
      x The value "var5" is invalid.
      i See `elicitr::elic_cont_get_data()`.

---

    Code
      elic_cont_get_data(obj, round = 1, var = c("var1", "var5", "var7"))
    Condition
      Error in `elic_cont_get_data()`:
      ! Argument `var` can be only a vector with a combination of "var1", "var2", and "var3" or "all":
      x The values "var5" and "var7" are invalid.
      i See `elicitr::elic_cont_get_data()`.

---

    Code
      elic_cont_get_data(obj, round = 1, var_types = c("Z", "N"))
    Condition
      Error in `elic_cont_get_data()`:
      ! Incorrect value for `var_types`:
      x The value provided for `var_types` should be a character string of short codes, i.e. "ZN" and not `c(Z, N)`.
      i See "Variable Types" in `elicitr::elic_cont_get_data()`.

---

    Code
      elic_cont_get_data(obj, round = 1, elic_types = c("1", "4"))
    Condition
      Error in `elic_cont_get_data()`:
      ! Incorrect value for `elic_types`:
      x The value provided for `elic_types` should be a character string of short codes, i.e. "14" and not `c(1, 4)`.
      i See "Elicitation types" in `elicitr::elic_cont_get_data()`.

---

    Code
      elic_cont_get_data(obj, round = 1, var_types = "pqR")
    Condition
      Error in `elic_cont_get_data()`:
      ! Incorrect value for `var_types`:
      x "q" is not in the list of available short codes.
      i See "Variable Types" in `elicitr::elic_cont_get_data()`.

---

    Code
      elic_cont_get_data(obj, round = 1, elic_types = "123")
    Condition
      Error in `elic_cont_get_data()`:
      ! Incorrect value for `elic_types`:
      x "2" is not in the list of available short codes.
      i See "Elicitation types" in `elicitr::elic_cont_get_data()`.

---

    Code
      elic_cont_get_data(obj, round = 1, var_types = "rN")
    Condition
      Error in `elic_cont_get_data()`:
      ! Invalid value for `var_types`:
      x Variable type "r" not present in the <elic_cont> object.
      i Available variable types are "Z", "N", and "p"

---

    Code
      elic_cont_get_data(obj, round = 1, var_types = "ZrR")
    Condition
      Error in `elic_cont_get_data()`:
      ! Invalid value for `var_types`:
      x Variable types "r" and "R" not present in the <elic_cont> object.
      i Available variable types are "Z", "N", and "p"

---

    Code
      elic_cont_get_data(obj, round = 1, elic_types = "3")
    Condition
      Error in `elic_cont_get_data()`:
      ! Invalid value for `elic_types`:
      x Elicitation type "3p" not present in the <elic_cont> object.
      i Available elicitation types are "1p" and "4p"

---

    Code
      elic_cont_get_data(obj, round = 1, elic_types = "34")
    Condition
      Error in `elic_cont_get_data()`:
      ! Invalid value for `elic_types`:
      x Elicitation types "3p" and "4p" not present in the <elic_cont> object.
      i Available elicitation type is "1p"

# Warnings

    Code
      out <- elic_cont_get_data(obj, round = 1, var = "var1", elic_types = "4")
    Condition
      Warning:
      Only one optional argument can be specified, used the first one: `var`
      i See Details in `elicitr::elic_cont_get_data()`.

---

    Code
      out <- elic_cont_get_data(obj, round = 1, var = "var2", var_types = "ZN")
    Condition
      Warning:
      Only one optional argument can be specified, used the first one: `var`
      i See Details in `elicitr::elic_cont_get_data()`.

---

    Code
      out <- elic_cont_get_data(obj, round = 1, var_types = "Zp", elic_types = "4")
    Condition
      Warning:
      Only one optional argument can be specified, used the first one: `var_types`
      i See Details in `elicitr::elic_cont_get_data()`.

