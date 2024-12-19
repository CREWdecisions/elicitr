test_that("Errors", {
  # When levels is not a character vector
  expect_snapshot(cat_start(levels = 1:3,
                            sites = c("site_1", "site_2", "site_3"),
                            experts = 8,
                            topics = c("topic_1", "topic_2")),
                  error = TRUE)
  # When sites is not a character vector
  expect_snapshot(cat_start(levels = c("level_1", "level_2", "level_3"),
                            sites = 1:3,
                            experts = 8,
                            topics = c("topic_1", "topic_2")),
                  error = TRUE)
  # When topics is not a character vector
  expect_snapshot(cat_start(levels = c("level_1", "level_2", "level_3"),
                            sites = c("site_1", "site_2", "site_3"),
                            experts = 8,
                            topics = 1:3),
                  error = TRUE)
  # When experts is not numeric
  expect_snapshot(cat_start(levels = c("level_1", "level_2", "level_3"),
                            sites = c("site_1", "site_2", "site_3"),
                            experts = "8",
                            topics = c("topic_1", "topic_2")),
                  error = TRUE)
  # When experts is a numeric vector
  expect_snapshot(cat_start(levels = c("level_1", "level_2", "level_3"),
                            sites = c("site_1", "site_2", "site_3"),
                            experts = 1:3,
                            topics = c("topic_1", "topic_2")),
                  error = TRUE)
})

test_that("Info", {
  expect_snapshot(x <- cat_start(levels = c("level_1", "level_2"),
                                 sites = c("site_1", "site_2", "site_3"),
                                 experts = 8,
                                 topics = c("topic_1", "topic_2")))
})

test_that("Output", {
  # The argument verbose is used
  expect_no_message(x <- cat_start(levels = c("level_1", "level_2"),
                                   sites = c("site_1", "site_2", "site_3"),
                                   experts = 8,
                                   topics = c("topic_1", "topic_2"),
                                   verbose = FALSE))
  expect_s3_class(x, class = "elic_cat", exact = TRUE)
  # Levels are recorded in the object
  expect_identical(x[["levels"]], c("level_1", "level_2"))
  expect_type(x[["levels"]], "character")
  # Sites are recorded in the object
  expect_identical(x[["sites"]], c("site_1", "site_2", "site_3"))
  expect_type(x[["sites"]], "character")
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
