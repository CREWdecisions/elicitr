# Errors

    Code
      elic_cat_get_data("abc", mechanism = "mechanism_1")
    Condition
      Error in `elic_cat_get_data()`:
      ! Invalid value for `x`:
      x Argument `x` must be an object of class <elic_cat> and not of class <character>.
      See `elicitr::elic_cat_get_data()`.

---

    Code
      elic_cat_get_data(obj, mechanism = 1)
    Condition
      Error in `elic_cat_get_data()`:
      ! Invalid value for `mechanism`:
      x Argument `mechanism` must be <character> not <numeric>.
      i See `elicitr::cat_start()`.

---

    Code
      elic_cat_get_data(obj, mechanism = c("mechanism_1", "mechanism_2"))
    Condition
      Error in `elic_cat_get_data()`:
      ! Incorrect value for `mechanism`:
      x Argument `mechanism` must have length 1 not 2.
      i See `elicitr::elic_cat_get_data()`.

---

    Code
      elic_cat_get_data(obj, mechanism = "mechanism_4")
    Condition
      Error in `elic_cat_get_data()`:
      ! Invalid value for `mechanism`:
      x "mechanism_4" not present in the <elic_cat> object.
      i Available mechanisms: "mechanism_1", "mechanism_2", and "mechanism_3".

---

    Code
      elic_cat_get_data(obj, mechanism = "mechanism_1", site = "site_5")
    Condition
      Error in `elic_cat_get_data()`:
      ! Invalid value for `sites`:
      x "site_5" not present in the <elic_cat> object.
      i Available sites: "site_1", "site_2", "site_3", and "site_4".

---

    Code
      elic_cat_get_data(obj, mechanism = "mechanism_3", site = "site_4")
    Condition
      Error in `elic_cat_get_data()`:
      ! Invalid value for `site_4`:
      x Site "site_4" not available in mechanism "mechanism_3".
      i Available sites: "site_1", "site_2", and "site_3".

