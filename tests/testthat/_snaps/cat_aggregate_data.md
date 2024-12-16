# Errors

    Code
      cat_aggregate_data("abc", method = "basic")
    Condition
      Error in `cat_aggregate_data()`:
      ! Invalid value for `x`:
      x Argument `x` must be an object of class <elic_cat> and not of class <character>.
      See `elicitr::cat_aggregate_data()`.

---

    Code
      cat_aggregate_data(obj, method = "new_method")
    Condition
      Error in `cat_aggregate_data()`:
      ! Invalid value for `method`:
      x The method "new_method" is not available for categorical data.
      i See Methods in `elicitr::cat_aggregate_data()`.

