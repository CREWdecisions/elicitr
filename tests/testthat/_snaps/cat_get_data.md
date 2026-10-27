# Errors

    Code
      cat_get_data("abc", topic = "topic_1")
    Condition
      Error in `cat_get_data()`:
      ! Invalid value for `x`:
      x Argument `x` must be a `?elicitr::elic_cat()` object created with `elicitr::cat_start()` and with data added with `elicitr::cat_add_data()`.
        `x` should be of class <elic_cat> and not of class <character>.
      i See `elicitr::cat_get_data()`.

---

    Code
      cat_get_data(x, method = "unweighted", topic = "topic_1")
    Condition
      Error in `cat_get_data()`:
      ! Invalid value for `x`:
      x Argument `x` must be a `?elicitr::elic_cat()` object created with `elicitr::cat_start()` and with data added with `elicitr::cat_add_data()`.
      i See `elicitr::cat_get_data()`.

---

    Code
      cat_get_data(x, method = "unweighted", topic = "topic_2")
    Condition
      Error in `cat_get_data()`:
      ! Invalid value for `x`:
      x Argument `x` must be a `?elicitr::elic_cat()` object created with `elicitr::cat_start()` and with data added with `elicitr::cat_add_data()`.
      i See `elicitr::cat_get_data()`.

---

    Code
      cat_get_data(x, method = "unweighted", topic = "topic_3")
    Condition
      Error in `cat_get_data()`:
      ! Invalid value for `x`:
      x Argument `x` must be a `?elicitr::elic_cat()` object created with `elicitr::cat_start()` and with data added with `elicitr::cat_add_data()`.
      i See `elicitr::cat_get_data()`.

---

    Code
      cat_get_data(obj, topic = 1)
    Condition
      Error in `cat_get_data()`:
      ! Invalid value for `topic`:
      x Argument `topic` must be <character> not <numeric>.
      i See `elicitr::cat_start()`.

---

    Code
      cat_get_data(obj, topic = c("topic_1", "topic_2"))
    Condition
      Error in `cat_get_data()`:
      ! Incorrect value for `topic`:
      x Argument `topic` must have length 1 not 2.
      i See `elicitr::cat_get_data()`.

---

    Code
      cat_get_data(obj, topic = "topic_4")
    Condition
      Error in `cat_get_data()`:
      ! Invalid value for `topic`:
      x "topic_4" not present in the <elic_cat> object.
      i Available topics: "topic_1", "topic_2", and "topic_3".
        See `elicitr::cat_get_data()` for more information.

---

    Code
      cat_get_data(obj, topic = "topic_1", option = "option_5")
    Condition
      Error in `cat_get_data()`:
      ! Invalid value for `options`:
      x "option_5" not present in the <elic_cat> object.
      i Available options: "option_1", "option_2", "option_3", and "option_4".
        See `elicitr::cat_get_data()` for more information.

---

    Code
      cat_get_data(obj, topic = "topic_3", option = "option_4")
    Condition
      Error in `cat_get_data()`:
      ! Invalid value for argument `option`:
      x Option "option_4" not available in topic "topic_3".
      i Available options: "option_1", "option_2", and "option_3".

