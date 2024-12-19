test_that("Errors", {
  obj <- create_cat_obj()

  # When x is not an elic_cat object
  expect_snapshot(cat_sample_data("abc",
                                  method = "basic",
                                  topic = "topic_1"),
                  error = TRUE)

  # When method is given as character vector of length > 1
  expect_snapshot(cat_sample_data(obj,
                                  method = c("basic", "bootstrap"),
                                  topic = "topic_1"),
                  error = TRUE)

  # When method is not a available
  expect_snapshot(cat_sample_data(obj,
                                  method = "new_method",
                                  topic = "topic_1"),
                  error = TRUE)
})

test_that("Info", {
  obj <- create_cat_obj()

  # Basic method
  expect_snapshot(out <- cat_sample_data(obj,
                                         method = "basic",
                                         topic = "topic_1",
                                         option = c("option_1", "option_2"),
                                         n_votes = 50))
  expect_s3_class(out, class = "cat_sample")
  expect_identical(attr(out, "topic"), "topic_1")
  expect_identical(nrow(out), as.integer(obj[["experts"]] * 2 * 50))

  # Bootstrap method
  expect_snapshot(out <- cat_sample_data(obj,
                                         method = "bootstrap",
                                         topic = "topic_1"))
  expect_s3_class(out, class = "cat_sample")
  expect_identical(attr(out, "topic"), "topic_1")
  res <- as.integer(obj[["experts"]] * length(obj[["options"]]) * 100)
  expect_identical(nrow(out), res)
})

test_that("Output", {
  obj <- create_cat_obj()

  # Modify one expert estimate to have 100% for category 1 in option 1
  obj[["data"]][["topic_1"]][1, 5] <- 1
  obj[["data"]][["topic_1"]][2:5, 5] <- 0

  # Basic method
  out <- cat_sample_data(obj,
                         method = "basic",
                         topic = "topic_1",
                         option = "option_1",
                         verbose = FALSE)
  expect_identical(dplyr::pull(out, "category_1")[1:5], rep(1, 5))
  expect_identical(dplyr::pull(out, "category_2")[1:5], rep(0, 5))
  expect_identical(dplyr::pull(out, "category_3")[1:5], rep(0, 5))
  expect_identical(dplyr::pull(out, "category_4")[1:5], rep(0, 5))
  expect_identical(dplyr::pull(out, "category_5")[1:5], rep(0, 5))

  # Bootstrap method
  out <- cat_sample_data(obj,
                         method = "bootstrap",
                         topic = "topic_1",
                         option = "option_1",
                         verbose = FALSE)
  expect_identical(dplyr::pull(out, "category_1")[1:5], rep(1, 5))
  expect_identical(dplyr::pull(out, "category_2")[1:5], rep(0, 5))
  expect_identical(dplyr::pull(out, "category_3")[1:5], rep(0, 5))
  expect_identical(dplyr::pull(out, "category_4")[1:5], rep(0, 5))
  expect_identical(dplyr::pull(out, "category_5")[1:5], rep(0, 5))
})
