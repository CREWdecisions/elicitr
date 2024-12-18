test_that("Errors", {
  obj <- create_cat_obj()

  # When x is not an elic_cat object
  expect_snapshot(cat_sample_data("abc",
                                  method = "basic",
                                  mechanism = "mechanism_1"),
                  error = TRUE)

  # When method is given as character vector of length > 1
  expect_snapshot(cat_sample_data(obj,
                                  method = c("basic", "bootstrap"),
                                  mechanism = "mechanism_1"),
                  error = TRUE)

  # When method is not a available
  expect_snapshot(cat_sample_data(obj,
                                  method = "new_method",
                                  mechanism = "mechanism_1"),
                  error = TRUE)
})

test_that("Info", {
  obj <- create_cat_obj()

  # Basic method
  expect_snapshot(out <- cat_sample_data(obj,
                                         method = "basic",
                                         mechanism = "mechanism_1",
                                         site = c("site_1", "site_2"),
                                         n_votes = 50))
  expect_s3_class(out, class = "cat_sample")
  expect_identical(nrow(out), as.integer(obj[["experts"]] * 2 * 50))

  # Bootstrap method
  expect_snapshot(out <- cat_sample_data(obj,
                                         method = "bootstrap",
                                         mechanism = "mechanism_1"))
  expect_s3_class(out, class = "cat_sample")
  expect_identical(nrow(out),
                   as.integer(obj[["experts"]] * length(obj[["sites"]]) * 100))
})

test_that("Output", {
  obj <- create_cat_obj()

  # Modify one expert estimate to have 100% for level 1 in site 1
  obj[["data"]][["mechanism_1"]][1, 5] <- 1
  obj[["data"]][["mechanism_1"]][2:5, 5] <- 0

  # Basic method
  out <- cat_sample_data(obj,
                         method = "basic",
                         mechanism = "mechanism_1",
                         site = "site_1",
                         verbose = FALSE)
  expect_identical(dplyr::pull(out, "level_1")[1:5], rep(1, 5))
  expect_identical(dplyr::pull(out, "level_2")[1:5], rep(0, 5))
  expect_identical(dplyr::pull(out, "level_3")[1:5], rep(0, 5))
  expect_identical(dplyr::pull(out, "level_4")[1:5], rep(0, 5))
  expect_identical(dplyr::pull(out, "level_5")[1:5], rep(0, 5))

  # Bootstrap method
  out <- cat_sample_data(obj,
                         method = "bootstrap",
                         mechanism = "mechanism_1",
                         site = "site_1",
                         verbose = FALSE)
  expect_identical(dplyr::pull(out, "level_1")[1:5], rep(1, 5))
  expect_identical(dplyr::pull(out, "level_2")[1:5], rep(0, 5))
  expect_identical(dplyr::pull(out, "level_3")[1:5], rep(0, 5))
  expect_identical(dplyr::pull(out, "level_4")[1:5], rep(0, 5))
  expect_identical(dplyr::pull(out, "level_5")[1:5], rep(0, 5))
})
