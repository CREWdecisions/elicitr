# Errors

    Code
      cont_sample_data("abc", round = 1)
    Condition
      Error in `cont_sample_data()`:
      ! Invalid value for `x`:
      x Argument `x` must be an object of class <elic_cont> and not of class <character>.
      See `elicitr::cont_sample_data()`.

---

    Code
      cont_sample_data(obj, round = 0)
    Condition
      Error in `cont_sample_data()`:
      ! Incorrect value for `round`:
      x `round` can only be 1 or 2.
      i See `elicitr::cont_sample_data()`.

---

    Code
      cont_sample_data(obj, round = 3)
    Condition
      Error in `cont_sample_data()`:
      ! Incorrect value for `round`:
      x `round` can only be 1 or 2.
      i See `elicitr::cont_sample_data()`.

---

    Code
      cont_sample_data(obj, round = 1, method = "method_3")
    Condition
      Error in `cont_sample_data()`:
      ! Invalid value for `method`:
      x The method "method_3" is not available for continuous data.
      i See Methods in `elicitr::cont_sample_data()`.

---

    Code
      cont_sample_data(obj, round = 1, var = "var4")
    Condition
      Error in `cont_sample_data()`:
      ! Invalid value for `var`:
      x Variable "var4" not present in the <elic_cont> object.
      i Available variables are "var1", "var2", and "var3".

---

    Code
      cont_sample_data(obj, round = 1, var = c("var4", "var5"))
    Condition
      Error in `cont_sample_data()`:
      ! Invalid value for `var`:
      x Variables "var4" and "var5" not present in the <elic_cont> object.
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

# Warnings

    Code
      out <- cont_sample_data(obj, round = 1, var = "var3", verbose = FALSE)
    Condition
      Warning:
      ! Some values have been constrained to be between 0 and 1.

# Info

    Code
      out <- cont_sample_data(obj, round = 1, var = "var1", n_votes = 50)
    Message
      v Data for 1 variable sampled successfully using the "basic" method.

---

    Code
      out <- cont_sample_data(obj, round = 2, var = c("var1", "var2"), n_votes = 100)
    Message
      v Data for 2 variables sampled successfully using the "basic" method.

---

    Code
      out <- cont_sample_data(obj, round = 2)
    Message
      v Rescaled min and max for variable "var3".
      v Data for 3 variables sampled successfully using the "basic" method.

---

    Code
      out <- cont_sample_data(obj, round = 2, var = "var3", weights = w)
    Message
      i Provided weights used instead of confidence estimates
      v Rescaled min and max for variable "var3".
      v Data for 1 variable sampled successfully using the "basic" method.

