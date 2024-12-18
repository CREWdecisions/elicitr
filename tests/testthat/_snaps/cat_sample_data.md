# Errors

    Code
      cat_sample_data("abc", method = "basic")
    Condition
      Error in `cat_sample_data()`:
      ! Invalid value for `x`:
      x Argument `x` must be an object of class <elic_cat> and not of class <character>.
      See `elicitr::cat_sample_data()`.

---

    Code
      cat_sample_data(obj, method = c("basic", "bootstrap"))
    Condition
      Error in `cat_sample_data()`:
      ! Incorrect value for `method`:
      x Argument `method` must have length 1 not 2.
      i See `elicitr::cat_sample_data()`.

---

    Code
      cat_sample_data(obj, method = "new_method")
    Condition
      Error in `cat_sample_data()`:
      ! Invalid value for `method`:
      x The method "new_method" is not available for categorical data.
      i See Methods in `elicitr::cat_sample_data()`.

