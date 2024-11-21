test_that("elicit object", {
  x <- new_elicit(var_names = c("var1", "var2"),
                  var_types = c("p", "R"),
                  elic_types = c("4", "3"),
                  title = "Title")
  expect_s3_class(x, class = "elicit", exact = TRUE)
  # Variable names are recorded in the object as character vector
  expect_length(x$var_types, 2)
  expect_type(x$var_names, "character")
  # Variable type short codes are recorded in the object as character vector
  expect_length(x$var_types, 2)
  expect_type(x$var_types, "character")
  # Elicitation type short codes are recorded in the object as character vector
  expect_length(x$var_types, 2)
  expect_type(x$elic_types, "character")
  # Data is present and empty with default function arguments
  expect_type(x$data, "list")
  expect_length(x$var_types, 2)
  expect_null(x$data$round_1)
  expect_null(x$data$round_2)
  # Title attribute is created
  expect_false(is.null(attr(x, "title")))
  expect_identical(attr(x, "title"), "Title")
  # Object structure
  expect_snapshot_value(x, style = "deparse")
})
