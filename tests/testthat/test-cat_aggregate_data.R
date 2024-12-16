test_that("Errors", {
  obj <- create_elic_cat_obj()

  # When x is not an elic_cat object
  expect_snapshot(elic_cat_aggregate_data("abc",
                                          method = "basic"),
                  error = TRUE)
})
