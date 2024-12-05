# Errors

    Code
      elic_plot("abc", round = 1, var = "var1")
    Condition
      Error in `elic_plot()`:
      ! Argument `x` must be an object of class <elicit>:
      x An object of class <character> is invalid.
      See `elicitr::elic_plot()`.

---

    Code
      elic_plot(obj, round = 3, var = "var1")
    Condition
      Error in `elic_plot()`:
      ! Argument `round` can be only 1 or 2:
      x The value 3 is invalid.
      i See `elicitr::elic_plot()`.

---

    Code
      elic_plot(obj, round = 1, var = "var5")
    Condition
      Error in `elic_plot()`:
      ! Invalid value for `var`:
      x Variable "var5" not found in the <elicit> object.
      i Available variables are "var1", "var2", and "var3".

---

    Code
      elic_plot(obj, round = 1, var = c("var1", "var5", "var7"))
    Condition
      Error in `elic_plot()`:
      ! Incorrect value for `var`:
      x Only one variable can be plotted at a time, you passed 3 variables.
      i See `elicitr::plot.elicit()`.

# Warnings

    Code
      p <- elic_plot(obj, round = 1, var = "var3", verbose = FALSE)
    Condition
      Warning:
      ! Some values have been constrained to be between 0 and 1.

# Info

    Code
      p <- elic_plot(obj, round = 2, var = "var3")
    Message
      v Rescaled min and max
