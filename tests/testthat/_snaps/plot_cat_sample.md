# Errors

    Code
      plot(samp, option = "option_7")
    Condition
      Error in `plot()`:
      ! Invalid value for argument `option`:
      x Option "option_7" not available in the sampled data.
        Available options: "option_1", "option_2", "option_3", "option_4", and "all".
      i See `elicitr::plot.cat_sample()` for more information.

---

    Code
      plot(samp, colours = c("red", "blue"))
    Condition
      Error in `plot()`:
      ! Invalid value for argument `colours`:
      x The number of colours provided does not match the number of categories.
      i Please provide a vector with 5 colours.
        See `elicitr::plot.cat_sample()` for more information.

---

    Code
      plot(samp, type = "boxplot")
    Condition
      Error in `plot()`:
      ! Invalid value for argument `type`:
      x Type "boxplot" is not implemented.
      i Available types are "violin" and "beeswarm".
        See `elicitr::plot.cat_sample()` for more information.

---

    Code
      plot(samp, type = "density")
    Condition
      Error in `plot()`:
      ! Invalid value for argument `type`:
      x Type "density" is not implemented.
      i Available types are "violin" and "beeswarm".
        See `elicitr::plot.cat_sample()` for more information.

---

    Code
      plot(samp, option = "option_1", type = "beeswarm")
    Condition
      Error in `plot()`:
      ! The data in "option_1" only holds NAs.
      x Use `elicitr::cont_get_data()` to see the data in the `?elicitr::elic_cat()` object.
      i If the resulting data is also only NA, review raw data. Otherwise review the object parsed in `x`

# Warning when only NA for an option

    Code
      p <- plot(samp, type = "violin")
    Message
      i No data rendered for "option_1" as no estimate was provided.

---

    Code
      p <- plot(samp, type = "violin")
    Message
      i No data rendered for "option_1", "option_2", and "option_4" as no estimate was provided.

