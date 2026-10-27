# Errors

    Code
      cont_sample_data("abc", round = 1)
    Condition
      Error in `cont_sample_data()`:
      ! Invalid value for `x`:
      x Argument `x` must be a `?elicitr::elic_cont()` object created with `elicitr::cont_start()` and with data added with `elicitr::cont_add_data()`.
        `x` should be of class <elic_cont> and not of class <character>.
      i See `elicitr::cont_sample_data()`.

---

    Code
      cont_sample_data(x, round = 1)
    Condition
      Error in `cont_sample_data()`:
      ! Invalid value for `x`:
      x Argument `x` must be a `?elicitr::elic_cont()` object created with `elicitr::cont_start()` and with data added with `elicitr::cont_add_data()`.
      i See `elicitr::cont_sample_data()`.

---

    Code
      cont_sample_data(x, round = 2)
    Condition
      Error in `cont_sample_data()`:
      ! Invalid value for `x`:
      x Argument `x` must be a `?elicitr::elic_cont()` object created with `elicitr::cont_start()` and with data added with `elicitr::cont_add_data()`.
      i See `elicitr::cont_sample_data()`.

---

    Code
      cont_sample_data(obj, round = 0)
    Condition
      Error in `cont_sample_data()`:
      ! Incorrect value for `round`:
      x Argument `round` can only be equal to 1 (for "Round 1") or to 2 (for "Round 2").
      i See `elicitr::cont_sample_data()`.

---

    Code
      cont_sample_data(obj, round = 3)
    Condition
      Error in `cont_sample_data()`:
      ! Incorrect value for `round`:
      x Argument `round` can only be equal to 1 (for "Round 1") or to 2 (for "Round 2").
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
      i Available `var_names` are "var1", "var2", and "var3".
        See `elicitr::cont_sample_data()` for more information.

---

    Code
      cont_sample_data(obj, round = 1, var = c("var4", "var5"))
    Condition
      Error in `cont_sample_data()`:
      ! Invalid value for `var`:
      x Variables "var4" and "var5" not present in the <elic_cont> object.
      i Available `var_names` are "var1", "var2", and "var3".
        See `elicitr::cont_sample_data()` for more information.

---

    Code
      cont_sample_data(obj, round = 1, var = "var1", weights = c(1, 2))
    Condition
      Error in `cont_sample_data()`:
      ! Invalid value for argument `weights:`
      x Argument `weights` must be 1 or a vector of length 6, same as the number of experts.
      i See `elicitr::cont_sample_data()` for more information.

---

    Code
      cont_sample_data(obj, round = 1, var = "var1", weights = rep(0, 6))
    Condition
      Error in `cont_sample_data()`:
      ! Invalid value for argument `weights:`
      x Argument `weights` cannot be 0 for all experts.
      i See `elicitr::cont_sample_data()` for more information.

---

    Code
      cont_sample_data(obj, round = 1, var = "var1", weights = c(rep(0, 5), NA))
    Condition
      Error in `cont_sample_data()`:
      ! Invalid value for argument `weights:`
      x Weights cannot be "NA". Set weights to default (NULL) to avoid using this argument.
      i See `elicitr::cont_sample_data()` for more information.

---

    Code
      cont_sample_data(obj, round = 1, var = "var1", weights = c(rep(0, 5), -1))
    Condition
      Error in `cont_sample_data()`:
      ! Invalid value for argument `weights:`
      x Weights cannot be negative.
      i See `elicitr::cont_sample_data()` for more information.

---

    Code
      cont_sample_data(obj, round = 1, var = "var1", n_votes = -1)
    Condition
      Error in `cont_sample_data()`:
      ! Invalid value for `n_votes`:
      x Argument `n_votes` must be a single positive integer.
      See `elicitr::cont_sample_data()`.

---

    Code
      cont_sample_data(obj, round = 1, var = "var1", n_votes = c(1, 2, 4, 5, 3, 6))
    Condition
      Error in `cont_sample_data()`:
      ! Invalid value for `n_votes`:
      x Argument `n_votes` must be a single positive integer.
      See `elicitr::cont_sample_data()`.

---

    Code
      cont_sample_data(obj, round = 1, var = "var1", n_votes = NA)
    Condition
      Error in `cont_sample_data()`:
      ! Invalid value for `n_votes`:
      x Argument `n_votes` must be a single positive integer.
      See `elicitr::cont_sample_data()`.

# Warnings

    Code
      out <- cont_sample_data(obj, round = 1, var = "var3", verbose = FALSE)
    Condition
      Warning:
      ! Some values have been constrained to be between 0 and 1.
        See the scale_conf section in the help page for `elicitr::cont_sample_data()` for more information.

# Accepts NAs from one expert

    Code
      out <- cont_sample_data(obj, round = 1, var = c("var3", "var2"), verbose = FALSE)
    Condition
      Warning:
      ! Some values have been constrained to be between 0 and 1.
        See the scale_conf section in the help page for `elicitr::cont_sample_data()` for more information.

# Accepts NAs from all experts for one variable

    Code
      out <- cont_sample_data(obj, round = 1, var = "var3", verbose = FALSE)

# Info

    Code
      out <- cont_sample_data(obj, round = 1, var = "var1", n_votes = 50)
    Message
      v Data for "var1" sampled successfully using the "PERT" method.

---

    Code
      out <- cont_sample_data(obj, round = 2, var = c("var1", "var2"), n_votes = 100)
    Message
      v Data for "var1" and "var2" sampled successfully using the "PERT" method.

---

    Code
      out <- cont_sample_data(obj, round = 2)
    Message
      v Rescaled min and max based on confidence estimates for variable "var3".
      v Data for "var1", "var2", and "var3" sampled successfully using the "PERT" method.

---

    Code
      out <- cont_sample_data(obj, round = 2, var = "var3", weights = w)
    Message
      i Weights provided in argument `weights` used instead of confidence estimates to calculate the number of vote attributed to each expert in variable "var3".
      v Rescaled min and max based on confidence estimates for variable "var3".
      v Data for "var3" sampled successfully using the "PERT" method.

# cont_sample_data ignores completely empty placeholder rows

    Code
      expected <- cont_sample_data(obj, round = 1, verbose = FALSE)
    Condition
      Warning:
      ! Some values have been constrained to be between 0 and 1.
        See the scale_conf section in the help page for `elicitr::cont_sample_data()` for more information.

---

    Code
      actual <- cont_sample_data(x_placeholders, round = 1, verbose = FALSE)
    Condition
      Warning:
      ! Some values have been constrained to be between 0 and 1.
        See the scale_conf section in the help page for `elicitr::cont_sample_data()` for more information.

