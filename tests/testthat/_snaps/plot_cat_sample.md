# Errors

    Code
      plot(samp, site = "site_7")
    Condition
      Error in `plot()`:
      ! Invalid value for argument `site`:
      x Site "site_7" not available in the sampled data.
      i Available sites: "site_1", "site_2", "site_3", and "site_4".

---

    Code
      plot(samp, colours = c("red", "blue"))
    Condition
      Error in `plot()`:
      ! Invalid value for argument `colours`:
      x The number of colours provided does not match the number of categories.
      i Please provide a vector with 5 colours.

