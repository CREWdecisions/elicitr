test_that("Errors", {
  obj <- create_cont_obj()

  # When x is not an elic_cont object
  expect_snapshot(cont_sample_data("abc", round = 1),
                  error = TRUE)

  # When round is neither 1 nor 2
  expect_snapshot(cont_sample_data(obj, round = 0),
                  error = TRUE)
  expect_snapshot(cont_sample_data(obj, round = 3),
                  error = TRUE)

  # When method is not among the available methods
  expect_snapshot(cont_sample_data(obj, round = 1, method = "method_3"),
                  error = TRUE)

  # When one var is not among the available variables
  expect_snapshot(cont_sample_data(obj, round = 1, var = "var4"),
                  error = TRUE)

  # When two var is not among the available variables
  expect_snapshot(cont_sample_data(obj, round = 1, var = c("var4", "var5")),
                  error = TRUE)

  # When weights is a vector of the wrong length
  expect_snapshot(cont_sample_data(obj, round = 1, var = "var1",
                                   weights = c(1, 2)),
                  error = TRUE)
})

test_that("Warnings", {
  obj <- create_cont_obj()

  # One variable with 3p without weights
  expect_snapshot(out <- cont_sample_data(obj, round = 1,
                                          var = "var3",
                                          verbose = FALSE))
  expect_identical(nrow(out), 6000L)
  experts <- unique(obj[["data"]][["round_1"]][["id"]])
  n_samp_actual <- table(factor(out[["id"]], levels = unique(out[["id"]]))) |>
    as.vector()
  data <- obj[["data"]][["round_1"]][c(1, 6:9)]
  n_samp_expected <- get_boostrap_n_sample(experts,
                                           n_votes = 1000,
                                           weights = NULL,
                                           elic_type = "4p",
                                           data = data) |>
    as.integer()
  expect_identical(n_samp_actual, n_samp_expected)
})

test_that("Accepts NAs from one expert", {
  #1p variable
  obj <- create_cont_obj()
  obj[["data"]][["round_1"]][1, 2] <- NA
  out <- cont_sample_data(obj,
                          round = 1,
                          var = c("var1", "var2"),
                          verbose = FALSE)
  expect_length(which(out[["id"]][out[["var"]] == "var1"] ==
                        unique(out[["id"]])[1]),
                1)
  expect_length(which(out[["id"]][out[["var"]] != "var1"] ==
                        unique(out[["id"]])[1]),
                1000)
  expect_true(is.na(out[["value"]][out[["var"]] == "var1"][1]))

  #3p variable
  obj <- create_cont_obj()
  obj[["data"]][["round_1"]][1, 3:5] <- NA
  out <- cont_sample_data(obj,
                          round = 1,
                          var = c("var2", "var1"),
                          verbose = FALSE)
  expect_length(which(out[["id"]][out[["var"]] == "var2"] ==
                        unique(out[["id"]])[1]),
                1)
  expect_length(which(out[["id"]][out[["var"]] != "var2"] ==
                        unique(out[["id"]])[1]),
                1000)
  expect_true(is.na(out[["value"]][out[["var"]] == "var2"][1]))

  #4p variable
  obj <- create_cont_obj()
  obj[["data"]][["round_1"]][1, 6:9] <- NA
  expect_snapshot(out <- cont_sample_data(obj,
                                          round = 1,
                                          var = c("var3", "var2"),
                                          verbose = FALSE))
  expect_length(which(out[["id"]][out[["var"]] == "var3"] ==
                        unique(out[["id"]])[1]),
                1)
  expect_length(which(out[["id"]][out[["var"]] != "var3"] ==
                        unique(out[["id"]])[1]),
                1000)
  expect_true(is.na(out[["value"]][out[["var"]] == "var3"][1]))
})

test_that("Accepts NAs from all experts for one variable", {
  #1p variable
  obj <- create_cont_obj()
  experts <- obj[["data"]][["round_1"]][["id"]]
  obj[["data"]][["round_1"]][, 2] <- NA
  out <- cont_sample_data(obj,
                          round = 1,
                          var = "var1",
                          verbose = FALSE)
  expect_length(which(out[["id"]] %in% unique(out[["id"]])), length(experts))
  expect_identical(nrow(out), length(experts))
  expect_true(all(is.na(out[["value"]])))
  expect_length(out[["value"]], length(experts))

  #3p variable
  obj <- create_cont_obj()
  obj[["data"]][["round_1"]][, 3:5] <- NA
  out <- cont_sample_data(obj,
                          round = 1,
                          var = "var2",
                          verbose = FALSE)
  expect_length(which(out[["id"]] %in% unique(out[["id"]])), length(experts))
  expect_identical(nrow(out), length(experts))
  expect_true(all(is.na(out[["value"]])))
  expect_length(out[["value"]], length(experts))

  #4p variable
  obj <- create_cont_obj()
  obj[["data"]][["round_1"]][, 6:9] <- NA
  expect_snapshot(out <- cont_sample_data(obj,
                                          round = 1,
                                          var = "var3",
                                          verbose = FALSE))
  expect_length(which(out[["id"]] %in% unique(out[["id"]])), length(experts))
  expect_identical(nrow(out), length(experts))
  expect_true(all(is.na(out[["value"]])))
  expect_length(out[["value"]], length(experts))
})

test_that("Info", {
  obj <- create_cont_obj()

  # One variable
  expect_snapshot(out <- cont_sample_data(obj, round = 1, var = "var1",
                                          n_votes = 50))
  expect_s3_class(out, class = "cont_sample")
  expect_identical(attr(out, "round"), 1)
  expect_identical(nrow(out), as.integer(obj[["experts"]] * 50))
  expect_identical(as.vector(table(out[["id"]])), rep(50L, 6))
  # Each sampled value must match the estimate of its expert
  original <- obj[["data"]][["round_1"]]
  expected <- original[["var1_best"]][match(out[["id"]], original[["id"]])]
  expect_identical(out[["value"]], expected)

  # Two variable
  expect_snapshot(out <- cont_sample_data(obj,
                                          round = 2,
                                          var = c("var1", "var2"),
                                          n_votes = 100))
  expect_s3_class(out, class = "cont_sample")
  expect_identical(attr(out, "round"), 2)
  expect_identical(nrow(out), as.integer(obj[["experts"]] * 100 * 2))
  expect_identical(as.vector(table(out[["id"]])), rep(200L, 6))

  # Three variables
  expect_snapshot(out <- cont_sample_data(obj,
                                          round = 2))
  expect_s3_class(out, class = "cont_sample")
  expect_identical(attr(out, "round"), 2)
  expect_identical(nrow(out), as.integer(obj[["experts"]] * 1000 * 3))
  experts <- unique(obj[["data"]][["round_2"]][["id"]])
  n_samp_actual <- table(factor(out[["id"]], levels = unique(out[["id"]]))) |>
    as.vector()
  data <- obj[["data"]][["round_2"]][c(1, 6:9)]
  n_samp_expected <- get_boostrap_n_sample(experts,
                                           n_votes = 1000,
                                           weights = NULL,
                                           elic_type = "4p",
                                           data = data) |>
    as.integer()
  expect_identical(n_samp_actual, n_samp_expected + 2000L)

  # One variable with 4p and weights
  w <- c(0.8, 0.7, 0.9, 0.7, 0.6, 0.9)
  expect_snapshot(out <- cont_sample_data(obj, round = 2,
                                          var = "var3",
                                          weights = w))
  expect_identical(nrow(out), 6000L)
  experts <- unique(obj[["data"]][["round_1"]][["id"]])
  n_samp_actual <- table(factor(out[["id"]], levels = unique(out[["id"]]))) |>
    as.vector()
  data <- obj[["data"]][["round_2"]][c(1, 6:9)]
  n_samp_expected <- get_boostrap_n_sample(experts,
                                           n_votes = 1000,
                                           weights = w,
                                           elic_type = "4p",
                                           data = data) |>
    as.integer()
  expect_identical(n_samp_actual, n_samp_expected)
})

test_that("Output", {
  obj <- create_cont_obj()

  # One variable with weights
  w <- c(0.8, 0.7, 0.9, 0.7, 0.6, 0.9)
  out <- cont_sample_data(obj, round = 1,
                          var = "var2",
                          weights = w,
                          verbose = FALSE)
  expect_identical(nrow(out), 6000L)
  experts <- unique(obj[["data"]][["round_1"]][["id"]])
  n_samp_actual <- table(factor(out[["id"]], levels = unique(out[["id"]]))) |>
    as.vector()
  data <- obj[["data"]][["round_1"]][c(1, 3:5)]
  n_samp_expected <- get_boostrap_n_sample(experts,
                                           n_votes = 1000,
                                           weights = w,
                                           elic_type = "3p",
                                           data = data) |>
    as.integer()
  expect_identical(n_samp_actual, n_samp_expected)

  # One variable with 4p without weights
  out <- cont_sample_data(obj, round = 2,
                          var = "var3",
                          verbose = FALSE)
  expect_identical(nrow(out), 6000L)
  experts <- unique(obj[["data"]][["round_2"]][["id"]])
  n_samp_actual <- table(factor(out[["id"]], levels = unique(out[["id"]]))) |>
    as.vector()
  data <- obj[["data"]][["round_2"]][c(1, 6:9)]
  n_samp_expected <- get_boostrap_n_sample(experts,
                                           n_votes = 1000,
                                           weights = NULL,
                                           elic_type = "4p",
                                           data = data) |>
    as.integer()
  expect_identical(n_samp_actual, n_samp_expected)
})

test_that("one weight supplied for all experts", {
  obj <- create_cont_obj()

  out <- cont_sample_data(obj, round = 2,
                          var = "var1",
                          weights = 10,
                          verbose = FALSE)
  expect_identical(nrow(out), 6000L)
  experts <- unique(obj[["data"]][["round_1"]][["id"]])
  n_samp_actual <- table(factor(out[["id"]], levels = unique(out[["id"]]))) |>
    as.vector()
  data <- obj[["data"]][["round_1"]][1:2]
  n_samp_expected <- get_boostrap_n_sample(experts,
                                           n_votes = 1000,
                                           weights = rep(10, 6),
                                           elic_type = "1p",
                                           data = data) |>
    as.integer()
  expect_identical(n_samp_actual, n_samp_expected)

})

test_that("one-point sampling preserves a single estimate", {
  out <- get_sample(estimates = 10,
                    n_samp = 20,
                    e = 1,
                    elic_type = "1p")

  expect_identical(out, rep(10, 20))
})
