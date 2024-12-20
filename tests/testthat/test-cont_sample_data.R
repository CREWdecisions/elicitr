test_that("Errors", {
  obj <- create_cont_obj()

  # When x is not an elic_cont object
  expect_snapshot(cont_sample_data("abc", round = 1, var = "var1"),
                  error = TRUE)

  # When round is neither 1 nor 2
  expect_snapshot(cont_sample_data(obj, round = 0, var = "var1"),
                  error = TRUE)
  expect_snapshot(cont_sample_data(obj, round = 3, var = "var1"),
                  error = TRUE)

  # When var is not a character string of length 1
  expect_snapshot(cont_sample_data(obj, round = 1, var = c("var1", "var2")),
                  error = TRUE)
  # When var is not among the available variables
  expect_snapshot(cont_sample_data(obj, round = 1, var = "var4"),
                  error = TRUE)

  # When weights is not 1 and not a vector
  expect_snapshot(cont_sample_data(obj, round = 1, var = "var1", weights = 2),
                  error = TRUE)

  # When weights is a vector of the wrong length
  expect_snapshot(cont_sample_data(obj, round = 1, var = "var1",
                                   weights = c(1, 2)),
                  error = TRUE)
})
