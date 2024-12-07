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

---

    Code
      elic_cont_plot(obj, round = 1, var = "var1", truth = 0.8)
    Condition
      Error in `elic_cont_plot()`:
      ! Incorrect value for `truth`:
      x Argument `truth` is of class <numeric> but it should be a named <list>.
      i See `elicitr::elic_cont_plot()`.

---

    Code
      elic_cont_plot(obj, round = 1, var = "var1", truth = list(min = 0.7, max = 0.9))
    Condition
      Error in `elic_cont_plot()`:
      ! Incorrect value for `truth`:
      x Argument `truth` is a list with 2 elements but it should have only 1 element named "best".
      i See Details in `elicitr::elic_cont_plot()`.

---

    Code
      elic_cont_plot(obj, round = 1, var = "var1", truth = list(beast = 0.8))
    Condition
      Error in `elic_cont_plot()`:
      ! Incorrect value for `truth`:
      x The name of the element in `truth` should be "best" and not "beast".
      i See Details in `elicitr::elic_cont_plot()`.

---

    Code
      elic_cont_plot(obj, round = 2, var = "var2", truth = list(min = 0.7, max = 0.9))
    Condition
      Error in `elic_cont_plot()`:
      ! Incorrect value for `truth`:
      x Argument `truth` is a list with 2 elements but should have 3 elements named "min", "max" and "best".
      i See Details in `elicitr::elic_cont_plot()`.

---

    Code
      elic_cont_plot(obj, round = 2, var = "var2", truth = list(min = 0.7, beast = 0.8,
        max = 0.9))
    Condition
      Error in `elic_cont_plot()`:
      ! Incorrect value for `truth`:
      x The name of the element in `truth` should be "min", "max", and "best" and not "min", "beast", and "max".
      i See Details in `elicitr::elic_cont_plot()`.

---

    Code
      elic_cont_plot(obj, round = 2, var = "var3", truth = list(min = 0.7, max = 0.9))
    Condition
      Error in `elic_cont_plot()`:
      ! Incorrect value for `truth`:
      x Argument `truth` is a list with 2 elements but should have 4 elements named "min", "max", "best" and "conf".
      i See Details in `elicitr::elic_cont_plot()`.

---

    Code
      elic_cont_plot(obj, round = 2, var = "var3", truth = list(min = 0.7, beast = 0.8,
        max = 0.9, conf = 100))
    Condition
      Error in `elic_cont_plot()`:
      ! Incorrect value for `truth`:
      x The name of the element in `truth` should be "min", "max", "best", and "conf" and not "min", "beast", "max", and "conf".
      i See Details in `elicitr::elic_cont_plot()`.

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

