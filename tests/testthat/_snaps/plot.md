# Errors

    Code
      elic_cont_plot("abc", round = 1, var = "var1")
    Condition
      Error in `elic_cont_plot()`:
      ! Argument `x` must be an object of class <elic_cont>:
      x An object of class <character> is invalid.
      See `elicitr::elic_cont_plot()`.

---

    Code
      elic_cont_plot(obj, round = 3, var = "var1")
    Condition
      Error in `elic_cont_plot()`:
      ! Incorrect value for `round`:
      x `round` can only be 1 or 2.
      i See `elicitr::elic_cont_plot()`.

---

    Code
      elic_cont_plot(obj, round = 1, var = "var5")
    Condition
      Error in `elic_cont_plot()`:
      ! Invalid value for `var`:
      x Variable "var5" not found in the <elic_cont_plot> object.
      i Available variables are "var1", "var2", and "var3".

---

    Code
      elic_cont_plot(obj, round = 1, var = c("var1", "var5", "var7"))
    Condition
      Error in `elic_cont_plot()`:
      ! Incorrect value for `var`:
      x Only one variable can be plotted at a time, you passed 3 variables.
      i See `elicitr::plot.elic_cont()`.

# Warnings

    Code
      p <- elic_cont_plot(obj, round = 1, var = "var3", verbose = FALSE)
    Condition
      Warning:
      ! Some values have been constrained to be between 0 and 1.

# Info

    Code
      p <- elic_cont_plot(obj, round = 2, var = "var3")
    Message
      v Rescaled min and max

