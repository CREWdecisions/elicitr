test_that("elic_cat object", {
  x <- new_elic_cat(levels = c("level_1", "level_2"),
                    sites = c("site_1", "site_2", "site_3"),
                    experts = 8,
                    mechanisms = c("mechanism_1", "mechanism_2"),
                    title = "Title")
  expect_s3_class(x, class = "elic_cat", exact = TRUE)
  # Levels are recorded in the object as character vector
  expect_vector(x[["levels"]], ptype = "character", size = 2)
  expect_identical(x[["levels"]], c("level_1", "level_2"))
  # Sites are recorded in the object as character vector
  expect_vector(x[["sites"]], ptype = "character", size = 3)
  expect_identical(x[["sites"]], c("site_1", "site_2", "site_3"))
  # Number of experts are recorded in the object
  expect_identical(x[["experts"]], 8)
  # Data is present and empty with default element names
  expect_type(x[["data"]], "list")
  expect_length(x[["data"]], 2)
  expect_null(x[["data"]][["mechanism_1"]])
  expect_null(x[["data"]][["mechanism_2"]])
  expect_named(x[["data"]], c("mechanism_1", "mechanism_2"))
  # Title attribute is created
  expect_false(is.null(attr(x, "title")))
  expect_identical(attr(x, "title"), "Title")
  # Object structure
  expect_snapshot_value(x, style = "deparse")
})

test_that("Print elicit object", {
  # Without data
  expect_snapshot(new_elic_cat(levels = c("level_1", "level_2"),
                               sites = c("site_1", "site_2", "site_3"),
                               experts = 8,
                               mechanisms = c("mechanism_1", "mechanism_2"),
                               title = "Title"))
  # With data
  expect_snapshot(create_elic_cat_obj())
})
