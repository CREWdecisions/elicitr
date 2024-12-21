# Errors

    Code
      plot(samp, var = c("var1", "var2"))
    Condition
      Error in `plot()`:
      ! Incorrect value for `var`:
      x Argument `var` must have length 1 not 2.
      i See `elicitr::plot.cont_sample()`.

---

    Code
      plot(samp, var = "var5")
    Condition
      Error in `check_var_in_sample()`:
      ! Invalid value for argument `var`:
      x Variable "var5" is not available in the sampled data.
      i Available variables are "var1", "var2", and "var3".

---

    Code
      plot(samp, var = "var1", type = "boxplot")
    Condition
      Error in `plot()`:
      ! Invalid value for argument `type`:
      x Type "boxplot" is not implemented.
      i Available types are "violin" and "density".

---

    Code
      plot(samp, var = "var1", colours = c("red", "blue"))
    Condition
      Error in `plot()`:
      ! Invalid value for argument `colours`:
      x The number of colours provided does not match the number of experts.
      i Please provide a vector with 6 colours.

---

    Code
      plot(samp, var = "var1", colours = c("red", "blue"), group = TRUE)
    Condition
      Error in `plot()`:
      ! Invalid value for argument `colours`:
      x The number of colours provided is more than 1.
      i Please provide only 1 colour when `group = TRUE`.

