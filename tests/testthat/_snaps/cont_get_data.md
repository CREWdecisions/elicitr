# Errors

    Code
      cont_get_data("abc", round = 1)
    Condition
      Error in `cont_get_data()`:
      ! Invalid value for `x`:
      x Argument `x` must be an object of class <elic_cont> and not of class <character>.
      See `elicitr::cont_get_data()`.

---

    Code
      cont_get_data(obj, round = 3)
    Condition
      Error in `cont_get_data()`:
      ! Incorrect value for `round`:
      x `round` can only be 1 or 2.
      i See `elicitr::cont_get_data()`.

---

    Code
      cont_get_data(obj, round = 1, var = "var5")
    Condition
      Error in `cont_get_data()`:
      ! Invalid value for `var`:
      x Variable "var5" not present in the <elic_cont> object.
      i Available variables are "var1", "var2", and "var3".

---

    Code
      cont_get_data(obj, round = 1, var = c("var1", "var5", "var7"))
    Condition
      Error in `cont_get_data()`:
      ! Invalid value for `var`:
      x Variables "var5" and "var7" not present in the <elic_cont> object.
      i Available variables are "var1", "var2", and "var3".

---

    Code
      cont_get_data(obj, round = 1, var_types = c("Z", "N"))
    Condition
      Error in `cont_get_data()`:
      ! Incorrect value for `var_types`:
      x The value provided for `var_types` should be a character string of short codes, i.e. "ZN" and not `c("Z", "N")`.
      i See "Variable types" in `elicitr::cont_get_data()`.

---

    Code
      cont_get_data(obj, round = 1, elic_types = c("1", "4"))
    Condition
      Error in `cont_get_data()`:
      ! Incorrect value for `elic_types`:
      x The value provided for `elic_types` should be a character string of short codes, i.e. "14" and not `c("1", "4")`.
      i See "Elicitation types" in `elicitr::cont_get_data()`.

---

    Code
      cont_get_data(obj, round = 1, var_types = "pqR")
    Condition
      Error in `cont_get_data()`:
      ! Incorrect value for `var_types`:
      x "q" is not in the list of available short codes.
      i See "Variable types" in `elicitr::cont_get_data()`.

---

    Code
      cont_get_data(obj, round = 1, elic_types = "123")
    Condition
      Error in `cont_get_data()`:
      ! Incorrect value for `elic_types`:
      x "2" is not in the list of available short codes.
      i See "Elicitation types" in `elicitr::cont_get_data()`.

---

    Code
      cont_get_data(obj, round = 1, var_types = "rN")
    Condition
      Error in `cont_get_data()`:
      ! Invalid value for `var_types`:
      x "r" not present in the <elic_cont> object.
      i Available var types: "Z", "N", and "p".

---

    Code
      cont_get_data(obj, round = 1, var_types = "ZrR")
    Condition
      Error in `cont_get_data()`:
      ! Invalid value for `var_types`:
      x "r" and "R" not present in the <elic_cont> object.
      i Available var types: "Z", "N", and "p".

---

    Code
      cont_get_data(obj, round = 1, elic_types = "3")
    Condition
      Error in `cont_get_data()`:
      ! Invalid value for `elic_types`:
      x "3p" not present in the <elic_cont> object.
      i Available elic types: "1p" and "4p".

---

    Code
      cont_get_data(obj, round = 1, elic_types = "34")
    Condition
      Error in `cont_get_data()`:
      ! Invalid value for `elic_types`:
      x "3p" and "4p" not present in the <elic_cont> object.
      i Available elic types: "1p".

# Warnings

    Code
      out <- cont_get_data(obj, round = 1, var = "var1", elic_types = "4")
    Condition
      Warning:
      Only one optional argument can be specified, used the first one provided: `var`
      i See Details in `elicitr::cont_get_data()`.

---

    Code
      out <- cont_get_data(obj, round = 1, var = "var2", var_types = "ZN")
    Condition
      Warning:
      Only one optional argument can be specified, used the first one provided: `var`
      i See Details in `elicitr::cont_get_data()`.

---

    Code
      out <- cont_get_data(obj, round = 1, var_types = "Zp", elic_types = "4")
    Condition
      Warning:
      Only one optional argument can be specified, used the first one provided: `var_types`
      i See Details in `elicitr::cont_get_data()`.

