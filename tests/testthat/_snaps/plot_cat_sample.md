# Errors

    Code
      plot(samp, option = "option_7")
    Condition
      Error in `plot()`:
      ! Invalid value for argument `option`:
      x Option "option_7" not available in the sampled data.
      i Available options: "option_1", "option_2", "option_3", and "option_4".

---

    Code
      plot(samp, colours = c("red", "blue"))
    Condition
      Error in `plot()`:
      ! Invalid value for argument `colours`:
      x The number of colours provided does not match the number of categories.
      i Please provide a vector with 5 colours.

---

    Code
      plot(samp, type = "boxplot")
    Condition
      Error in `plot()`:
      ! Invalid value for argument `type`:
      x Type "boxplot" is not implemented.
      i Available types are "beeswarm" and "violin".

