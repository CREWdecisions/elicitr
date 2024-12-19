test_that("elic_cat object", {
  x <- new_elic_cat(categories = c("category_1", "category_2"),
                    options = c("option_1", "option_2", "option_3"),
                    experts = 8,
                    topics = c("topic_1", "topic_2"),
                    title = "Title")
  expect_s3_class(x, class = "elic_cat", exact = TRUE)
  # categories are recorded in the object as character vector
  expect_vector(x[["categories"]], ptype = "character", size = 2)
  expect_identical(x[["categories"]], c("category_1", "category_2"))
  # options are recorded in the object as character vector
  expect_vector(x[["options"]], ptype = "character", size = 3)
  expect_identical(x[["options"]], c("option_1", "option_2", "option_3"))
  # Number of experts are recorded in the object
  expect_identical(x[["experts"]], 8)
  # Data is present and empty with default element names
  expect_type(x[["data"]], "list")
  expect_length(x[["data"]], 2)
  expect_null(x[["data"]][["topic_1"]])
  expect_null(x[["data"]][["topic_2"]])
  expect_named(x[["data"]], c("topic_1", "topic_2"))
  # Title attribute is created
  expect_false(is.null(attr(x, "title")))
  expect_identical(attr(x, "title"), "Title")
  # Object structure
  expect_snapshot_value(x, style = "deparse")
})

test_that("Print elicit object", {
  # Without data
  expect_snapshot(new_elic_cat(categories = c("category_1", "category_2"),
                               options = c("option_1", "option_2", "option_3"),
                               experts = 8,
                               topics = c("topic_1", "topic_2"),
                               title = "Title"))
  # With data
  expect_snapshot(create_cat_obj())
})
