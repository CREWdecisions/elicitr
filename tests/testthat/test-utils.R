test_that("split_short_codes() works", {
  # For variable types
  x <- split_short_codes("abc")
  expect_length(x, 3)
  expect_type(x, "character")
  expect_identical(x, c("a", "b", "c"))
  # For elicitation types (the character "p" is added to the short codes)
  x <- split_short_codes("134", add_p = TRUE)
  expect_length(x, 3)
  expect_type(x, "character")
  expect_identical(x, c("1p", "3p", "4p"))
})

#####bootstrap
test_that("error", {
  #negative weights
  w <- c(-1, 3)
  expect_snapshot(get_boostrap_n_sample(experts = c("A", "B"),
                                        n_votes = 10,
                                        weights = w),
                  error = TRUE)
})

test_that("Output", {
  experts <- c("A", "B", "C")
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = 10,
                                         weights = c(1, 1, 1)),
                   c(10, 10, 10))

  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = 10,
                                         weights = c(1, 2, 3)),
                   c(5, 10, 15))

  # Multiplying every weight by the same number changes nothing
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = 10,
                                         weights = c(10, 20, 30)),
                   c(5, 10, 15))

  #rounding
  out <- get_boostrap_n_sample(experts = c("A", "B", "C"),
                               n_votes = 10,
                               weights = c(1, 1, 2))
  expect_identical(sum(out), 30)
  expect_identical(sort(out), c(7, 8, 15))

  #continuous
  data_1p <- tibble::tibble(id = experts,
                            best = c(1, 1, 1))
  data_3p <- tibble::tibble(id = experts,
                            min = c(0, 0, 0),
                            max = c(2, 2, 2),
                            best = c(1, 1, 1))
  data_4p <- tibble::tibble(id = experts,
                            min = c(0, 0, 0),
                            max = c(2, 2, 2),
                            best = c(1, 1, 1),
                            conf = c(60, 80, 100))
  #1p, 3p
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = 10,
                                         weights = NULL,
                                         elic_type = "1p",
                                         data = data_1p),
                   c(10, 10, 10))
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = 10,
                                         weights = NULL,
                                         elic_type = "3p",
                                         data = data_3p),
                   c(10, 10, 10))

  #1p, 3p with weights
  w <- c(2, 3, 4)
  n <- 10
  samp_expected <- (length(experts) * n * w / sum(w)) |>
    miceadds::sumpreserving.rounding(digits = 0, preserve = TRUE)
  expect_identical(get_boostrap_n_sample(experts,
                                         n_vote = n,
                                         weights = c(2, 3, 4),
                                         elic_type = "1p",
                                         data = data_1p),
                   samp_expected)
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = 10,
                                         weights = c(2, 3, 4),
                                         elic_type = "3p",
                                         data = data_3p),
                   samp_expected)

  #4p
  conf <- data_4p[["conf"]]
  n <- 10
  samp_expected <- (length(experts) * n * conf / sum(conf)) |>
    miceadds::sumpreserving.rounding(digits = 0, preserve = TRUE)
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = n,
                                         weights = NULL,
                                         elic_type = "4p",
                                         data = data_4p),
                   samp_expected)

  #4p with weights
  w <- c(1, 1, 1)
  n <- 10
  samp_expected <- (length(experts) * n * w / sum(w)) |>
    miceadds::sumpreserving.rounding(digits = 0, preserve = TRUE)
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = n,
                                         weights = w,
                                         elic_type = "4p",
                                         data = data_4p),
                   samp_expected)

  #categorical unweighted
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = 10,
                                         weights = c(60, 80, 100),
                                         elic_type = "unweighted"),
                   c(10, 10, 10))

  #categorical weighted
  w <- c(60, 80, 100)
  n <- 10
  samp_expected <- (length(experts) * n * w / sum(w)) |>
    miceadds::sumpreserving.rounding(digits = 0, preserve = TRUE)
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = n,
                                         weights = w,
                                         elic_type = "weighted"),
                   samp_expected)
})

test_that("Handles some NAs correctly", {
  experts <- c("A", "B", "C")
  #continuous
  data_1p <- tibble::tibble(id = experts,
                            best = c(NA, 1, 1))
  data_3p <- tibble::tibble(id = experts,
                            min = c(NA, 0, 0),
                            max = c(NA, 2, 2),
                            best = c(NA, 1, 1))
  data_4p <- tibble::tibble(id = experts,
                            min = c(NA, 0, 0),
                            max = c(NA, 2, 2),
                            best = c(NA, 1, 1),
                            conf = c(NA, 80, 100))

  #1p, 3p
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = 10,
                                         weights = NULL,
                                         elic_type = "1p",
                                         data = data_1p),
                   c(1, 15, 15))
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = 10,
                                         weights = NULL,
                                         elic_type = "3p",
                                         data = data_3p),
                   c(1, 15, 15))

  #1p, 3p with weights
  w <- c(2, 3, 4)
  w_NA <- c(0, 3, 4)
  n <- 10
  samp_expected <- (length(experts) * n * w_NA / sum(w_NA)) |>
    miceadds::sumpreserving.rounding(digits = 0, preserve = TRUE)
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = n,
                                         weights = w,
                                         elic_type = "1p",
                                         data = data_1p),
                   c(1, samp_expected[2:3]))
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = 10,
                                         weights = w,
                                         elic_type = "3p",
                                         data = data_3p),
                   c(1, samp_expected[2:3]))

  #4p
  conf <- data_4p[["conf"]]
  n <- 10
  conf_NA <- c(1, data_4p[["conf"]][2:3])
  samp_expected <- (length(experts) * n * conf_NA / sum(conf_NA)) |>
    miceadds::sumpreserving.rounding(digits = 0, preserve = TRUE)
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = n,
                                         weights = NULL,
                                         elic_type = "4p",
                                         data = data_4p),
                   c(1, samp_expected[2:3]))

  #4p with weights
  w <- c(2, 3, 4)
  w_NA <- c(0, 3, 4)
  n <- 10
  samp_expected <- (length(experts) * n * w_NA / sum(w_NA)) |>
    miceadds::sumpreserving.rounding(digits = 0, preserve = TRUE)
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = n,
                                         weights = w,
                                         elic_type = "4p",
                                         data = data_4p),
                   c(1, samp_expected[2:3]))

  #categorical unweighted with NA
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = 10,
                                         weights = c(NA, 60, 90),
                                         elic_type = "unweighted"),
                   c(1, 10, 10))

  #categorical weighted with NA
  w <- c(NA, 60, 90)
  n <- 10
  samp_expected <- (length(experts) * n * w / sum(w)) |>
    miceadds::sumpreserving.rounding(digits = 0, preserve = TRUE)
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = n,
                                         weights = w,
                                         elic_type = "weighted"),
                   c(1, 12, 18))
})

test_that("Handles all NAs correctly", {
  experts <- c("A", "B", "C")
  #continuous
  data_1p <- tibble::tibble(id = experts,
                            best = rep(NA, 3))
  data_3p <- tibble::tibble(id = experts,
                            min = rep(NA, 3),
                            max = rep(NA, 3),
                            best = rep(NA, 3))
  data_4p <- tibble::tibble(id = experts,
                            min = rep(NA, 3),
                            max = rep(NA, 3),
                            best = rep(NA, 3),
                            conf = rep(NA, 3))

  #1p, 3p
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = 10,
                                         weights = NULL,
                                         elic_type = "1p",
                                         data = data_1p),
                   c(1, 1, 1))
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = 10,
                                         weights = NULL,
                                         elic_type = "3p",
                                         data = data_3p),
                   c(1, 1, 1))

  #1p, 3p with weights
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = 10,
                                         weights = c(2, 3, 4),
                                         elic_type = "1p",
                                         data = data_1p),
                   c(1, 1, 1))
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = 10,
                                         weights = c(2, 3, 4),
                                         elic_type = "3p",
                                         data = data_3p),
                   c(1, 1, 1))

  #4p
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = 10,
                                         weights = NULL,
                                         elic_type = "4p",
                                         data = data_4p),
                   c(1, 1, 1))

  #4p with weights
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = 10,
                                         weights = c(10, 20, 30),
                                         elic_type = "4p",
                                         data = data_4p),
                   c(1, 1, 1))

  #categorical unweighted full NA
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = 10,
                                         weights = c(NA, NA, NA),
                                         elic_type = "unweighted"),
                   c(1, 1, 1))

  #categorical weighted full NA
  expect_identical(get_boostrap_n_sample(experts,
                                         n_votes = 10,
                                         weights = c(NA, NA, NA),
                                         elic_type = "weighted"),
                   c(1, 1, 1))
})
