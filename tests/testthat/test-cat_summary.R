test_that("Errors", {
  obj <- create_cat_obj()
  samp <- cat_sample_data(obj, method = "basic", topic = "topic_1",
                          verbose = FALSE)

  # When option is not available in the object
  expect_snapshot(summary(samp, option = "option_7"),
                  error = TRUE)
})

test_that("Output", {
  obj <- create_cat_obj()
  samp <- cat_sample_data(obj, method = "basic", topic = "topic_1",
                          verbose = FALSE)
  out <- summary(samp, option = "option_1")
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("Category", "Min", "Q1", "Median", "Mean", "Q3", "Max"))
  expect_identical(nrow(out), 5L)
  expect_identical(out[["Category"]],
                   c("category_1", "category_2", "category_3",
                     "category_4", "category_5"))
})
