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
      Error in `plot()`:
      ! Invalid value for argument `var`:
      x Variable "var5" is not available in the sampled data.
      i Available variables are "var1", "var2", and "var3".
        See `elicitr::plot.cont_sample()` for more information.

---

    Code
      plot(samp, var = "var1", type = "boxplot")
    Condition
      Error in `plot()`:
      ! Invalid value for argument `type`:
      x Type "boxplot" is not implemented.
      i Available types are "violin", "beeswarm", and "density".
        See `elicitr::plot.cont_sample()` for more information.

---

    Code
      plot(samp, var = "var1", colours = c("red", "blue"))
    Condition
      Error in `plot()`:
      ! Invalid value for argument `colours`:
      x The number of colours provided does not match the number of experts.
      i Please provide a vector with 6 colours.
        See `elicitr::plot.cont_sample()` for more information.

---

    Code
      plot(samp, var = "var1", colours = c("red", "blue"), group = TRUE)
    Condition
      Error in `plot()`:
      ! Invalid value for argument `colours`:
      x The number of colours provided is more than 1.
      i Please provide only 1 colour when `group = TRUE`.
        See `elicitr::plot.cont_sample()` for more information.

---

    Code
      plot(obj, round = 1, var = "var1", expert_names = paste0("E", 1:obj[["experts"]])[
        -1])
    Condition
      Error in `plot()`:
      ! Incorrect length of `expert_names`:
      x You provided 5 expert names but the elicitation process has 6 experts.
      i Provide a vector of length 6.
        See `elicitr::plot.elic_cont()` for more information.

---

    Code
      plot(samp, var = "var1", expert_names = rep("Same", obj[["experts"]]))
    Condition
      Error in `plot()`:
      ! Invalid value for `expert_names`:
      x Multiple experts have the same name in `expert_names`.
      i Please provide unique names for each expert.
        See `elicitr::plot.cont_sample()` for more information.

---

    Code
      plot(samp, var = "var1", expert_names = c("Group", paste0("E", 1:obj[[
        "experts"]])[-1]))
    Condition
      Error in `plot()`:
      ! Invalid value for `expert_names`:
      x The names "Group" and "Truth" are reserved and cannot be used in `expert_names`.
      i Please provide different names for the experts.
        See `elicitr::plot.cont_sample()` for more information.

---

    Code
      plot(samp, var = "var1", type = "beeswarm")
    Condition
      Error in `plot()`:
      ! Experts did not provide any estimate for variable "var1".
      x Use `elicitr::cont_get_data()` with `var` = "var1" to see the data in the `?elicitr::elic_cont()` object.
      i If the resulting data is also only NA, review raw data. Otherwise review the object parsed in `x`}.
        See `elicitr::plot.cont_sample()` for more information.

---

    Code
      plot(samp, type = "beeswarm")
    Condition
      Error in `plot()`:
      ! Experts did not provide any estimate for variable "var1".
      x Use `elicitr::cont_get_data()` with `var` = "var1" to see the data in the `?elicitr::elic_cont()` object.
      i If the resulting data is also only NA, review raw data. Otherwise review the object parsed in `x`}.
        See `elicitr::plot.cont_sample()` for more information.

---

    Code
      plot(samp, var = "var2", type = "beeswarm")
    Condition
      Error in `plot()`:
      ! Experts did not provide any estimate for variable "var2".
      x Use `elicitr::cont_get_data()` with `var` = "var2" to see the data in the `?elicitr::elic_cont()` object.
      i If the resulting data is also only NA, review raw data. Otherwise review the object parsed in `x`}.
        See `elicitr::plot.cont_sample()` for more information.

---

    Code
      plot(samp, var = "var3", type = "beeswarm")
    Condition
      Error in `plot()`:
      ! Experts did not provide any estimate for variable "var3".
      x Use `elicitr::cont_get_data()` with `var` = "var3" to see the data in the `?elicitr::elic_cont()` object.
      i If the resulting data is also only NA, review raw data. Otherwise review the object parsed in `x`}.
        See `elicitr::plot.cont_sample()` for more information.

# violin plot rendered if type is not violin and elic_type = 1p

    Code
      p <- plot(samp, var = "var1", type = "beeswarm")
    Message
      i Replacing `type` with "violin" as "var1" is a one-point variable.

---

    Code
      p <- plot(samp, var = "var1", type = "density")
    Message
      i Replacing `type` with "violin" as "var1" is a one-point variable.

