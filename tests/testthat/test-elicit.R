test_that("elicit object", {
  x <- new_elicit(var_names = c("var1", "var2"),
                  var_types = c("p", "R"),
                  elic_types = c("4", "3"),
                  experts = 5,
                  title = "Title")
  expect_s3_class(x, class = "elicit", exact = TRUE)
  # Variable names are recorded in the object as character vector
  expect_vector(x$var_names, ptype = "character", size = 2)
  # Variable type short codes are recorded in the object as character vector
  expect_vector(x$var_types, ptype = "character", size = 2)
  # Elicitation type short codes are recorded in the object as character vector
  expect_vector(x$elic_types, ptype = "character", size = 2)
  # Number of experts are recorded in the object
  expect_type(x$experts, "double")
  # Data is present and empty with default function arguments
  expect_type(x$data, "list")
  expect_length(x$data, 2)
  expect_null(x$data$round_1)
  expect_null(x$data$round_2)
  # Title attribute is created
  expect_false(is.null(attr(x, "title")))
  expect_identical(attr(x, "title"), "Title")
  # Object structure
  expect_snapshot_value(x, style = "deparse")
})

test_that("Print elicit object", {
  # Without data
  expect_snapshot(new_elicit(var_names = c("var1", "var2"),
                             var_types = c("p", "R"),
                             elic_types = c("4", "3"),
                             experts = 4L,
                             title = "Title"))
})
