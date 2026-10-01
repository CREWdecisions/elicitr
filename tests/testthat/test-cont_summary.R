test_that("Errors", {
  obj <- create_cont_obj()
  samp <- cont_sample_data(obj, round = 2, method = "PERT", verbose = FALSE)

  # When var is not on the object
  expect_snapshot(summary(samp, var = "var4"),
                  error = TRUE)

  # When all data is NA (one variable)
  obj[["data"]][["round_2"]][, 2] <- NA
  samp <- cont_sample_data(obj, round = 2, method = "PERT", verbose = FALSE)
  expect_snapshot(summary(samp, var = "var1"),
                  error = TRUE)

  # When all data is NA (multiple variables)
  obj[["data"]][["round_2"]][, 2:5] <- NA
  samp <- cont_sample_data(obj, round = 2, method = "PERT", verbose = FALSE)
  expect_snapshot(summary(samp, var = c("var2", "var1")),
                  error = TRUE)
})

test_that("Output", {
  obj <- create_cont_obj()
  samp <- cont_sample_data(obj, round = 2, method = "PERT", verbose = FALSE)

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

test_that("Missing variables", {
  obj <- create_cont_obj()
  # 1 missing variable
  obj[["data"]][["round_2"]][1, 2] <- NA
  samp <- cont_sample_data(obj, round = 2, method = "PERT", verbose = FALSE)
  out <- summary(samp, var = "var1")
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("Var", "Min", "Q1", "Median", "Mean", "Q3", "Max"))
  expect_identical(nrow(out), 1L)
  expect_identical(out[["Var"]], "var1")
  expect_false(anyNA(out))

  # all missing variable for one variable
  obj[["data"]][["round_2"]][, 2] <- NA
  samp <- cont_sample_data(obj, round = 2, method = "PERT", verbose = FALSE)
  expect_snapshot(out <- summary(samp))
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("Var", "Min", "Q1", "Median", "Mean", "Q3", "Max"))
  expect_identical(nrow(out), 2L)
  expect_identical(out[["Var"]], c("var2", "var3"))
  expect_false(anyNA(out))

  # all missing variable for multiple variables
  obj[["data"]][["round_2"]][, 2] <- NA
  obj[["data"]][["round_2"]][, 3:5] <- NA
  samp <- cont_sample_data(obj, round = 2, method = "PERT", verbose = FALSE)
  expect_snapshot(out <- summary(samp))
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("Var", "Min", "Q1", "Median", "Mean", "Q3", "Max"))
  expect_identical(nrow(out), 1L)
  expect_identical(out[["Var"]], "var3")
  expect_false(anyNA(out))
})
