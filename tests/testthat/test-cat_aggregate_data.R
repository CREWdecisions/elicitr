test_that("Errors", {
  obj <- create_elic_cat_obj()

  # When x is not an elic_cat object
  expect_snapshot(cat_aggregate_data("abc", method = "basic"),
                  error = TRUE)

  # When method is given as character vector of length > 1
  expect_snapshot(cat_aggregate_data(obj, method = c("basic", "bootstrap")),
                  error = TRUE)

  # When method is not a available
  expect_snapshot(cat_aggregate_data(obj, method = "new_method"),
                  error = TRUE)
})
