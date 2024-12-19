test_that("Errors", {

  cats <- c("category_1", "category_2", "category_3")

  # When categories is not a character vector
  expect_snapshot(cat_start(categories = 1:3,
                            options = c("option_1", "option_2", "option_3"),
                            experts = 8,
                            topics = c("topic_1", "topic_2")),
                  error = TRUE)
  # When options is not a character vector
  expect_snapshot(cat_start(categories = cats,
                            options = 1:3,
                            experts = 8,
                            topics = c("topic_1", "topic_2")),
                  error = TRUE)
  # When topics is not a character vector
  expect_snapshot(cat_start(categories = cats,
                            options = c("option_1", "option_2", "option_3"),
                            experts = 8,
                            topics = 1:3),
                  error = TRUE)
  # When experts is not numeric
  expect_snapshot(cat_start(categories = cats,
                            options = c("option_1", "option_2", "option_3"),
                            experts = "8",
                            topics = c("topic_1", "topic_2")),
                  error = TRUE)
  # When experts is a numeric vector
  expect_snapshot(cat_start(categories = cats,
                            options = c("option_1", "option_2", "option_3"),
                            experts = 1:3,
                            topics = c("topic_1", "topic_2")),
                  error = TRUE)
})

test_that("Info", {
  opt <- c("option_1", "option_2", "option_3")
  expect_snapshot(x <- cat_start(categories = c("category_1", "category_2"),
                                 options = opt,
                                 experts = 8,
                                 topics = c("topic_1", "topic_2")))
})

test_that("Output", {
  # The argument verbose is used
  opt <- c("option_1", "option_2", "option_3")
  expect_no_message(x <- cat_start(categories = c("category_1", "category_2"),
                                   options = opt,
                                   experts = 8,
                                   topics = c("topic_1", "topic_2"),
                                   verbose = FALSE))
  expect_s3_class(x, class = "elic_cat", exact = TRUE)
  # categories are recorded in the object
  expect_identical(x[["categories"]], c("category_1", "category_2"))
  expect_type(x[["categories"]], "character")
  # options are recorded in the object
  expect_identical(x[["options"]], c("option_1", "option_2", "option_3"))
  expect_type(x[["options"]], "character")
  # Number of experts are recorded in the object
  expect_identical(x[["experts"]], 8)
  # Data is present and empty
  expect_type(x[["data"]], "list")
  expect_length(x[["data"]], 2)
  expect_null(x[["data"]][["topic_1"]])
  expect_null(x[["data"]][["topic_1"]])
  expect_named(x[["data"]], c("topic_1", "topic_2"))
  # Title attribute is created
  expect_false(is.null(attr(x, "title")))
  # Object structure
  expect_snapshot_value(x, style = "deparse")
})
