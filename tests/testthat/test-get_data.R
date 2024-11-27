test_that("Raises errors", {
  obj <- create_elicit_obj()

  # When x is not an elicit object
  expect_snapshot(elic_get_data("abc", round = 1),
                  error = TRUE)
  # When round is not 1 or 2
  expect_snapshot(elic_get_data(obj, round = 3),
                  error = TRUE)
  # When var is neither a variable in the elicit object nor all
  expect_snapshot(elic_get_data(obj, round = 1, var = "var5"),
                  error = TRUE)
  expect_snapshot(elic_get_data(obj, round = 1,
                                var = c("var1", "var5", "var7")),
                  error = TRUE)
  # When length variable types > 1
  expect_snapshot(elic_get_data(obj, round = 1, var_types = c("Z", "N")),
                                error = TRUE)
  # When length elicitation types > 1
  expect_snapshot(elic_get_data(obj, round = 1, elic_types = c("1", "4")),
                                error = TRUE)
  # When 1 variable type is not allowed
  expect_snapshot(elic_get_data(obj, round = 1, var_types = "pqR"),
                  error = TRUE)
  # When 1 estimate type is not allowed
  expect_snapshot(elic_get_data(obj, round = 1, elic_types = "123"),
                  error = TRUE)
  # When 1 variable type is not on the object
  expect_snapshot(elic_get_data(obj, round = 1, var_types = "rN"),
                  error = TRUE)
  # When 2 variable types are not on the object
  expect_snapshot(elic_get_data(obj, round = 1, var_types = "ZrR"),
                  error = TRUE)
  # When 1 elicitation type is not on the object
  obj$elic_types <- c("1p", "4p")
  expect_snapshot(elic_get_data(obj, round = 1, elic_types = "3"),
                  error = TRUE)
  # When 2 elicitation types are not on the object
  obj$elic_types <- "1p"
  expect_snapshot(elic_get_data(obj, round = 1, elic_types = "34"),
                  error = TRUE)
})

test_that("Output", {
  obj <- create_elicit_obj()
  # Get all variables
  expect_identical(elic_get_data(obj, round = 1), obj$data$round_1)
  expect_identical(elic_get_data(obj, round = 2), obj$data$round_2)
  # Get only 1 variable
  expect_identical(elic_get_data(obj, round = 1, var = "var1"),
                   obj$data$round_1[, 1:2])
  expect_identical(elic_get_data(obj, round = 1, var = "var2"),
                   obj$data$round_1[, c(1, 3:5)])
  expect_identical(elic_get_data(obj, round = 1, var = "var3"),
                   obj$data$round_1[, c(1, 6:9)])
  # Get 2 variables
  expect_identical(elic_get_data(obj, round = 1, var = c("var1", "var3")),
                   obj$data$round_1[, c(1:2, 6:9)])
  # Order is the one of original dataset for 2 variables
  expect_identical(elic_get_data(obj, round = 1, var = c("var3", "var1")),
                   obj$data$round_1[, c(1:2, 6:9)])

  # Get only 1 variable type
  expect_identical(elic_get_data(obj, round = 1, var_types = "Z"),
                   obj$data$round_1[, 1:2])
  # Get 2 variable types
  expect_identical(elic_get_data(obj, round = 1, var_types = "Zp"),
                   obj$data$round_1[, c(1:2, 6:9)])
  # Order is the one of original dataset for 2 variable types
  expect_identical(elic_get_data(obj, round = 1, var_types = "pZ"),
                   obj$data$round_1[, c(1:2, 6:9)])

  # Get only 1 elicitation type
  expect_identical(elic_get_data(obj, round = 1, elic_types = "3"),
                   obj$data$round_1[, c(1, 3:5)])
  # Get 2 elicitation types
  expect_identical(elic_get_data(obj, round = 1, elic_types = "14"),
                   obj$data$round_1[, c(1:2, 6:9)])
  # Order is the one of original dataset for 2 elicitation types
  expect_identical(elic_get_data(obj, round = 1, elic_types = "41"),
                   obj$data$round_1[, c(1:2, 6:9)])
})

test_that("Warnings", {
  obj <- create_elicit_obj()

  # Always the first of the optional arguments is used
  expect_snapshot(out <- elic_get_data(obj, round = 1, var = "var1",
                                       elic_types = "4"))
  expect_identical(out, obj$data$round_1[, 1:2])

  expect_snapshot(out <- elic_get_data(obj, round = 1, var = "var2",
                                       var_types = "ZN"))
  expect_identical(out, obj$data$round_1[, c(1, 3:5)])

  expect_snapshot(out <- elic_get_data(obj, round = 1, var_types = "Zp",
                                       elic_types = "4"))
  expect_identical(out, obj$data$round_1[, c(1:2, 6:9)])
})
