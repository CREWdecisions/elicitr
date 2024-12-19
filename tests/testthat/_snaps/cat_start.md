# Errors

    Code
      cat_start(categories = 1:3, options = c("option_1", "option_2", "option_3"),
      experts = 8, topics = c("topic_1", "topic_2"))
    Condition
      Error in `cat_start()`:
      ! Invalid value for `categories`:
      x Argument `categories` must be <character> not <integer>.
      i See `elicitr::cat_start()`.

---

    Code
      cat_start(categories = cats, options = 1:3, experts = 8, topics = c("topic_1",
        "topic_2"))
    Condition
      Error in `cat_start()`:
      ! Invalid value for `options`:
      x Argument `options` must be <character> not <integer>.
      i See `elicitr::cat_start()`.

---

    Code
      cat_start(categories = cats, options = c("option_1", "option_2", "option_3"),
      experts = 8, topics = 1:3)
    Condition
      Error in `cat_start()`:
      ! Invalid value for `topics`:
      x Argument `topics` must be <character> not <integer>.
      i See `elicitr::cat_start()`.

---

    Code
      cat_start(categories = cats, options = c("option_1", "option_2", "option_3"),
      experts = "8", topics = c("topic_1", "topic_2"))
    Condition
      Error in `cat_start()`:
      ! Incorrect value for `experts`:
      x Argument `experts` must be <numeric> not <character>.
      See `elicitr::cat_start()`.

---

    Code
      cat_start(categories = cats, options = c("option_1", "option_2", "option_3"),
      experts = 1:3, topics = c("topic_1", "topic_2"))
    Condition
      Error in `cat_start()`:
      ! Incorrect value for `experts`:
      x Argument `experts` must be a single number not a vector of length 3.
      See `elicitr::cat_start()`.

# Info

    Code
      x <- cat_start(categories = c("category_1", "category_2"), options = opt,
      experts = 8, topics = c("topic_1", "topic_2"))
    Message
      v <elic_cat> object for "Elicitation" correctly initialised

# Output

    structure(list(categories = c("category_1", "category_2"), options = c("option_1", 
    "option_2", "option_3"), experts = 8, data = list(topic_1 = NULL, 
        topic_2 = NULL)), class = "elic_cat", title = "Elicitation")

