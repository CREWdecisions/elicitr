# Errors

    Code
      plot(obj, round = 3, var = "var1")
    Condition
      Error in `plot()`:
      ! Incorrect value for `round`:
      x `round` can only be 1 or 2.
      i See `elicitr::plot.elic_cont()`.

---

    Code
      plot(obj, round = 1, var = "var5")
    Condition
      Error in `plot()`:
      ! Invalid value for `var`:
      x Variable "var5" not found in the <elic_cont> object.
      i Available variables are "var1", "var2", and "var3".

---

    Code
      plot(obj, round = 1, var = c("var1", "var5", "var7"))
    Condition
      Error in `plot()`:
      ! Incorrect value for `var`:
      x Only one variable can be plotted at a time, you passed 3 variables.
      i See `elicitr::plot.elic_cont()`.

---

    Code
      plot(obj, round = 1, var = "var1", truth = 0.8)
    Condition
      Error in `plot()`:
      ! Incorrect value for `truth`:
      x Argument `truth` is of class <numeric> but it should be a named <list>.
      i See `elicitr::plot.elic_cont()`.

---

    Code
      plot(obj, round = 1, var = "var1", truth = list(min = 0.7, max = 0.9))
    Condition
      Error in `plot()`:
      ! Incorrect value for `truth`:
      x Argument `truth` is a list with 2 elements but it should have only 1 element named "best".
      i See Details in `elicitr::plot.elic_cont()`.

---

    Code
      plot(obj, round = 1, var = "var1", truth = list(beast = 0.8))
    Condition
      Error in `plot()`:
      ! Incorrect value for `truth`:
      x The name of the element in `truth` should be "best" and not "beast".
      i See Details in `elicitr::plot.elic_cont()`.

---

    Code
      plot(obj, round = 2, var = "var2", truth = list(min = 0.7, max = 0.9))
    Condition
      Error in `plot()`:
      ! Incorrect value for `truth`:
      x Argument `truth` is a list with 2 elements but should have 3 elements named "min", "max" and "best".
      i See Details in `elicitr::plot.elic_cont()`.

---

    Code
      plot(obj, round = 2, var = "var2", truth = list(min = 0.7, beast = 0.8, max = 0.9))
    Condition
      Error in `plot()`:
      ! Incorrect value for `truth`:
      x The name of the element in `truth` should be "min", "max", and "best" and not "min", "beast", and "max".
      i See Details in `elicitr::plot.elic_cont()`.

---

    Code
      plot(obj, round = 2, var = "var3", truth = list(min = 0.7, max = 0.9))
    Condition
      Error in `plot()`:
      ! Incorrect value for `truth`:
      x Argument `truth` is a list with 2 elements but should have 4 elements named "min", "max", "best" and "conf".
      i See Details in `elicitr::plot.elic_cont()`.

---

    Code
      plot(obj, round = 2, var = "var3", truth = list(min = 0.7, beast = 0.8, max = 0.9,
        conf = 100))
    Condition
      Error in `plot()`:
      ! Incorrect value for `truth`:
      x The name of the element in `truth` should be "min", "max", "best", and "conf" and not "min", "beast", "max", and "conf".
      i See Details in `elicitr::plot.elic_cont()`.

---

    Code
      plot(obj, round = 1, var = "var1", expert_names = paste0("E", 1:obj[["experts"]])[
        -1])
    Condition
      Error in `plot()`:
      ! Incorrect length of `expert_names`:
      x You provided 5 expert names but the elicitation process has 6 experts.
      i Provide a vector of length 6.

---

    Code
      plot(obj, round = 2, var = "var1", expert_names = rep("Same", obj[["experts"]]))
    Condition
      Error in `plot()`:
      ! Invalid value for `expert_names`:
      x Multiple experts have the same name in `expert_names`.
      i Please provide unique names for each expert.

---

    Code
      plot(obj, round = 2, var = "var1", expert_names = c("Group", paste0("E", 1:obj[[
        "experts"]])[-1]))
    Condition
      Error in `plot()`:
      ! Invalid value for `expert_names`:
      x The names "Group" and "Truth" are reserved and cannot be used in `expert_names`.
      i Please provide different names for the experts.

---

    Code
      plot(obj, round = 2, var = "var1", expert_names = c("Truth", paste0("E", 1:obj[[
        "experts"]])[-1]))
    Condition
      Error in `plot()`:
      ! Invalid value for `expert_names`:
      x The names "Group" and "Truth" are reserved and cannot be used in `expert_names`.
      i Please provide different names for the experts.

# Warnings

    Code
      p <- plot(obj, round = 1, var = "var3", verbose = FALSE)
    Condition
      Warning:
      ! Some values have been constrained to be between 0 and 1.

# Info

    Code
      p <- plot(obj, round = 2, var = "var3")
    Message
      v Rescaled min and max

# NAs are discarded correctly

    Code
      p <- plot(obj, round = 2, var = "var1", verbose = FALSE)

---

    Code
      p <- plot(obj, round = 2, var = "var2", verbose = FALSE)

---

    Code
      p <- plot(obj, round = 1, var = "var3", verbose = FALSE)
    Condition
      Warning:
      ! Some values have been constrained to be between 0 and 1.

