# Errors

    Code
      summary(samp, var = "var4")
    Condition
      Error in `summary()`:
      ! Invalid value for argument `var`:
      x Variable "var4" is not available in the sampled data.
      i Available variables are "var1", "var2", and "var3".
        See `elicitr::summary.cont_sample()` for more information.

---

    Code
      summary(samp, var = "var1")
    Condition
      Error in `summary()`:
      ! "var1" only hold NAs.
      x Use `elicitr::cont_get_data()` with `var` = "var1" to see the data in the `?elicitr::elic_cont()` object.
      i If the resulting data is also only NA, review raw data. Otherwise review the object samp.

---

    Code
      summary(samp, var = c("var2", "var1"))
    Condition
      Error in `summary()`:
      ! "var1" and "var2" only holds NAs.
      x Use `elicitr::cont_get_data()` with `var` = "var1" and "var2" to see the data in the `?elicitr::elic_cont()` object.
      i If the resulting data is also only NA, review raw data. Otherwise review the object samp.

# Missing variables

    Code
      out <- summary(samp)
    Message
      > Results were dropped for "var1" as no estimate was provided.

---

    Code
      out <- summary(samp)
    Message
      > Results were dropped for "var1" and "var2" as no estimate was provided.

