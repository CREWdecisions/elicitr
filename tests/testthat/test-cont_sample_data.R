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

  # When one var is not among the available variables
  expect_snapshot(cont_sample_data(obj, round = 1, var = "var4"),
                  error = TRUE)

  # When two var is not among the available variables
  expect_snapshot(cont_sample_data(obj, round = 1, var = c("var4", "var5")),
                  error = TRUE)

  # When weights is not 1 and not a vector
  expect_snapshot(cont_sample_data(obj, round = 1, var = "var1", weights = 2),
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
  conf <- obj[["data"]][["round_1"]][, 9, drop = TRUE] / 100
  n_samp_expected <- get_boostrap_n_sample(experts, 1000, conf) |>
    as.integer()
  expect_identical(n_samp_actual, n_samp_expected)
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

  # Two variable
  expect_snapshot(out <- cont_sample_data(obj,
                                          round = 2,
                                          var = c("var1", "var2"),
                                          n_votes = 100))
  expect_s3_class(out, class = "cont_sample")
  expect_identical(attr(out, "round"), 2)
  expect_identical(nrow(out), as.integer(obj[["experts"]] * 100 * 2))
  expect_identical(as.vector(table(out[["id"]])), rep(200L, 6))

  # One variable with 3p and weights
  w <- c(0.8, 0.7, 0.9, 0.7, 0.6, 0.9)
  expect_snapshot(out <- cont_sample_data(obj, round = 2,
                                          var = "var3",
                                          weights = w))
  expect_identical(nrow(out), 6000L)
  experts <- unique(obj[["data"]][["round_1"]][["id"]])
  n_samp_actual <- table(factor(out[["id"]], levels = unique(out[["id"]]))) |>
    as.vector()
  n_samp_expected <- get_boostrap_n_sample(experts, 1000, w) |>
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
  n_samp_expected <- get_boostrap_n_sample(experts, 1000, w) |>
    as.integer()
  expect_identical(n_samp_actual, n_samp_expected)

  # One variable with 3p without weights
  out <- cont_sample_data(obj, round = 2,
                          var = "var3",
                          verbose = FALSE)
  expect_identical(nrow(out), 6000L)
  experts <- unique(obj[["data"]][["round_2"]][["id"]])
  n_samp_actual <- table(factor(out[["id"]], levels = unique(out[["id"]]))) |>
    as.vector()
  conf <- obj[["data"]][["round_2"]][, 9, drop = TRUE] / 100
  n_samp_expected <- get_boostrap_n_sample(experts, 1000, conf) |>
    as.integer()
  expect_identical(n_samp_actual, n_samp_expected)
})
