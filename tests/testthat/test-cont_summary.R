test_that("Errors", {
  obj <- create_cont_obj()
  samp <- cont_sample_data(obj, round = 2, method = "basic", verbose = FALSE)

  # When var is not on the object
  expect_snapshot(summary(samp, var = "var4"),
                  error = TRUE)
})

test_that("Output", {
  obj <- create_cont_obj()
  samp <- cont_sample_data(obj, round = 2, method = "basic", verbose = FALSE)

  # When no variable is specified
  out <- summary(samp)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("Var", "Min", "Q1", "Median", "Mean", "Q3", "Max"))
  expect_identical(nrow(out), 3L)
  expect_identical(out[["Var"]], c("var1", "var2", "var3"))

  # When one variable is specified
  out <- summary(samp, var = "var1")
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("Var", "Min", "Q1", "Median", "Mean", "Q3", "Max"))
  expect_identical(nrow(out), 1L)
  expect_identical(out[["Var"]], "var1")
})

test_that("accepts NAs", {
  obj <- create_cont_obj()
  samp <- cont_sample_data(obj, round = 2, method = "basic", verbose = FALSE)

  # Introduce NAs
  samp_na <- samp
  samp_na$value[1:10] <- NA

  samp <- samp[-(1:10), , drop = FALSE]

  out <- summary(samp, var = "var1")
  out_na <- summary(samp_na, var = "var1")
  expect_s3_class(out_na, "tbl_df")
  expect_named(out_na, c("Var", "Min", "Q1", "Median", "Mean", "Q3", "Max"))
  expect_identical(out, out_na)
})
