# Errors

    Code
      cat_start(levels = 1:3, sites = c("site_1", "site_2", "site_3"), experts = 8,
      topics = c("topic_1", "topic_2"))
    Condition
      Error in `cat_start()`:
      ! Invalid value for `levels`:
      x Argument `levels` must be <character> not <integer>.
      i See `elicitr::cat_start()`.

---

    Code
      cat_start(levels = c("level_1", "level_2", "level_3"), sites = 1:3, experts = 8,
      topics = c("topic_1", "topic_2"))
    Condition
      Error in `cat_start()`:
      ! Invalid value for `sites`:
      x Argument `sites` must be <character> not <integer>.
      i See `elicitr::cat_start()`.

---

    Code
      cat_start(levels = c("level_1", "level_2", "level_3"), sites = c("site_1",
        "site_2", "site_3"), experts = 8, topics = 1:3)
    Condition
      Error in `cat_start()`:
      ! Invalid value for `topics`:
      x Argument `topics` must be <character> not <integer>.
      i See `elicitr::cat_start()`.

---

    Code
      cat_start(levels = c("level_1", "level_2", "level_3"), sites = c("site_1",
        "site_2", "site_3"), experts = "8", topics = c("topic_1", "topic_2"))
    Condition
      Error in `cat_start()`:
      ! Incorrect value for `experts`:
      x Argument `experts` must be <numeric> not <character>.
      See `elicitr::cat_start()`.

---

    Code
      cat_start(levels = c("level_1", "level_2", "level_3"), sites = c("site_1",
        "site_2", "site_3"), experts = 1:3, topics = c("topic_1", "topic_2"))
    Condition
      Error in `cat_start()`:
      ! Incorrect value for `experts`:
      x Argument `experts` must be a single number not a vector of length 3.
      See `elicitr::cat_start()`.

# Info

    Code
      x <- cat_start(levels = c("level_1", "level_2"), sites = c("site_1", "site_2",
        "site_3"), experts = 8, topics = c("topic_1", "topic_2"))
    Message
      v <elic_cat> object for "Elicitation" correctly initialised

# Output

    structure(list(levels = c("level_1", "level_2"), sites = c("site_1", 
    "site_2", "site_3"), experts = 8, data = list(topic_1 = NULL, 
        topic_2 = NULL)), class = "elic_cat", title = "Elicitation")

