# Raises errors

    Code
      elic_get_data("abc", round = 1)
    Condition
      Error in `elic_get_data()`:
      ! Argument `x` must be an object of class <elicit>:
      x An object of class <character> is invalid.
      See `elicitr::elic_get_data()`.

---

    Code
      elic_get_data(obj, round = 3)
    Condition
      Error in `elic_get_data()`:
      ! Argument `round` can be only 1 or 2:
      x The value 3 is invalid.
      i See `elicitr::elic_get_data()`.

---

    Code
      elic_get_data(obj, round = 1, var = "var5")
    Condition
      Error in `elic_get_data()`:
      ! Argument `var` can be only a vector with a combination of "var1", "var2", and "var3" or "all":
      x The value "var5" is invalid.
      i See `elicitr::elic_get_data()`.

---

    Code
      elic_get_data(obj, round = 1, var = c("var1", "var5", "var7"))
    Condition
      Error in `elic_get_data()`:
      ! Argument `var` can be only a vector with a combination of "var1", "var2", and "var3" or "all":
      x The values "var5" and "var7" are invalid.
      i See `elicitr::elic_get_data()`.

