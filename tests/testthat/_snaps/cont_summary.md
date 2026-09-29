# Errors

    Code
      summary(samp, var = "var4")
    Condition
      Error in `check_var_in_sample()`:
      ! Invalid value for argument `var`:
      x Variable "var4" is not available in the sampled data.
      i Available variables are "var1", "var2", and "var3".

---

    Code
      summary(samp, var = "var1")
    Condition
      Error in `summary()`:
      ! No estimate provided
      x the provided data only holds NAs
      i No data provided in "var1".

---

    Code
      summary(samp, var = c("var2", "var1"))
    Condition
      Error in `summary()`:
      ! No estimate provided
      x the provided data only holds NAs
      i No data provided in "var1" and "var2".

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

