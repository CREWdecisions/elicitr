# Errors

    Code
      cont_sample_data("abc", round = 1, var = "var1")
    Condition
      Error in `cont_sample_data()`:
      ! Invalid value for `x`:
      x Argument `x` must be an object of class <elic_cont> and not of class <character>.
      See `elicitr::cont_sample_data()`.

---

    Code
      cont_sample_data(obj, round = 0, var = "var1")
    Condition
      Error in `cont_sample_data()`:
      ! Incorrect value for `round`:
      x `round` can only be 1 or 2.
      i See `elicitr::cont_sample_data()`.

---

    Code
      cont_sample_data(obj, round = 3, var = "var1")
    Condition
      Error in `cont_sample_data()`:
      ! Incorrect value for `round`:
      x `round` can only be 1 or 2.
      i See `elicitr::cont_sample_data()`.

---

    Code
      cont_sample_data(obj, round = 1, var = c("var1", "var2"))
    Condition
      Error in `cont_sample_data()`:
      ! Incorrect value for `var`:
      x Argument `var` must have length 1 not 2.
      i See `elicitr::cont_sample_data()`.

---

    Code
      cont_sample_data(obj, round = 1, var = "var4")
    Condition
      Error in `cont_sample_data()`:
      ! Invalid value for `var`:
      x Variable "var4" not found in the <elic_cont> object.
      i Available variables are "var1", "var2", and "var3".

---

    Code
      cont_sample_data(obj, round = 1, var = "var1", weights = 2)
    Condition
      Error in `cont_sample_data()`:
      ! Invalid value for argument `weights:`
      x Argument `weights` must be 1 or a vector of length 6, same as the number of experts.
      i See `elicitr::cont_sample_data()` for more information.

---

    Code
      cont_sample_data(obj, round = 1, var = "var1", weights = c(1, 2))
    Condition
      Error in `cont_sample_data()`:
      ! Invalid value for argument `weights:`
      x Argument `weights` must be 1 or a vector of length 6, same as the number of experts.
      i See `elicitr::cont_sample_data()` for more information.

