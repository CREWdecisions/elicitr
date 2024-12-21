test_that("Errors", {
  obj <- create_cont_obj()
  samp <- cont_sample_data(obj, round = 2, method = "basic", verbose = FALSE)

  # When var is of length > 1
  expect_snapshot(plot(samp, var = c("var1", "var2")),
                  error = TRUE)

  # When var is not on the object
  expect_snapshot(plot(samp, var = "var5"),
                  error = TRUE)

  # When plot type is not available
  expect_snapshot(plot(samp, var = "var1", type = "boxplot"),
                  error = TRUE)

  # When colours has length != number of experts
  expect_snapshot(plot(samp, var = "var1", colours = c("red", "blue")),
                  error = TRUE)

  # When colours has length != 1 and group is TRUE
  expect_snapshot(plot(samp, var = "var1", colours = c("red", "blue"),
                       group = TRUE),
                  error = TRUE)
})
