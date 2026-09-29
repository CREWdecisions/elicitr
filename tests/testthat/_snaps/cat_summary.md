# Errors

    Code
      summary(samp, option = "option_7")
    Condition
      Error in `summary()`:
      ! Invalid value for argument `option`:
      x Option "option_7" not available in the sampled data.
      i Available options: "option_1", "option_2", "option_3", "option_4", and "all".

---

    Code
      summary(samp, option = "option_1")
    Condition
      Error in `summary()`:
      ! No estimate provided
      x the provided data only holds NAs
      i No data provided in "option_1".

---

    Code
      summary(samp)
    Condition
      Error in `summary()`:
      ! No estimate provided
      x the provided data only holds NAs
      i No data provided in "option_1", "option_2", "option_3", and "option_4".

