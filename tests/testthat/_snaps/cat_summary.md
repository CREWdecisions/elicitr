# Errors

    Code
      summary(samp, option = "option_7")
    Condition
      Error in `summary()`:
      ! Invalid value for argument `option`:
      x Option "option_7" not available in the sampled data.
        Available options: "option_1", "option_2", "option_3", "option_4", and "all".
      i See `elicitr::summary.cat_sample()` for more information.

---

    Code
      summary(samp, option = "option_1")
    Condition
      Error in `summary()`:
      ! The data in "option_1" only holds NAs.
      x Use `elicitr::cont_get_data()` to see the data in the `?elicitr::elic_cat()` object.
      i If the resulting data is also only NA, review raw data. Otherwise review the object parsed in `object`.

---

    Code
      summary(samp)
    Condition
      Error in `summary()`:
      ! The data in "option_1", "option_2", "option_3", and "option_4" only holds NAs.
      x Use `elicitr::cont_get_data()` to see the data in the `?elicitr::elic_cat()` object.
      i If the resulting data is also only NA, review raw data. Otherwise review the object parsed in `object`.

# With NA

    Code
      out <- summary(samp, option = "all")
    Message
      > Results were dropped for "option_1" as no estimate was provided.

---

    Code
      out <- summary(samp, option = "all")
    Message
      > Results were dropped for "option_1" and "option_2" as no estimate was provided.

