test_that("Raises errors", {
  obj <- create_elicit_obj()
  # When x is not an elicit object
  expect_snapshot(elic_get_data("abc", round = 1),
                  error = TRUE)
  # When round is not 1 or 2
  expect_snapshot(elic_get_data(obj, round = 3),
                  error = TRUE)
  # When var is neither a variable in obj nor all
  expect_snapshot(elic_get_data(obj, round = 1, var = "var5"),
                  error = TRUE)
  expect_snapshot(elic_get_data(obj, round = 1,
                                var = c("var1", "var5", "var7")),
                  error = TRUE)
})

test_that("Output", {
  obj <- create_elicit_obj()
  # Get all variables
  expect_identical(elic_get_data(obj, round = 1), obj$data$round_1)
  expect_identical(elic_get_data(obj, round = 2), obj$data$round_2)
  # Get only one variable
  expect_identical(elic_get_data(obj, round = 1, var = "var1"),
                   obj$data$round_1[, 1:2])
  expect_identical(elic_get_data(obj, round = 1, var = "var2"),
                   obj$data$round_1[, c(1, 3:5)])
  expect_identical(elic_get_data(obj, round = 1, var = "var3"),
                   obj$data$round_1[, c(1, 6:9)])
})
