test_that("Errors", {
  # check_arg_length()----
  # When length variable types > 1
  expect_snapshot(elic_start(var_names = "var1",
                             var_types = c("p", "N"),
                             elic_types = "3"),
                  error = TRUE)
  # When length elicitation types > 1
  expect_snapshot(elic_start(var_names = "var1",
                             var_types = "p",
                             elic_types = c("4", "3")),
                  error = TRUE)
  # check_arg_types()----
  # When 1 variable type is not allowed
  expect_snapshot(elic_start(var_names = c("var1", "var2", "var3"),
                             var_types = "pqR",
                             elic_type = "1"),
                  error = TRUE)
  # When 2 variable types are not allowed but have the same short code
  expect_snapshot(elic_start(var_names = c("var1", "var2", "var3"),
                             var_types = "apa",
                             elic_type = "1"),
                  error = TRUE)
  # When 2 variable types are not allowed and have different short codes
  expect_snapshot(elic_start(var_names = c("var1", "var2", "var3"),
                             var_types = "pqG",
                             elic_type = "1"),
                  error = TRUE)
  # When 1 estimate type is not allowed
  expect_snapshot(elic_start(var_names = c("var1", "var2", "var3"),
                             var_types = "p",
                             elic_type = "123"),
                  error = TRUE)
  # When 2 estimate types are not allowed but have the same short code
  expect_snapshot(elic_start(var_names = c("var1", "var2", "var3"),
                             var_types = "p",
                             elic_type = "232"),
                  error = TRUE)
  # When 2 estimate types are not allowed and have different short codes
  expect_snapshot(elic_start(var_names = c("var1", "var2", "var3"),
                             var_types = "p",
                             elic_type = "1237"),
                  error = TRUE)
  # check_arg_mism()----
  # When there are less variables than variable types
  expect_snapshot(elic_start(var_names = c("var1"),
                             var_types = c("pR"),
                             elic_type = c("1")),
                  error = TRUE)
  # When there are less variables than estimate types
  expect_snapshot(elic_start(var_names = c("var1"),
                             var_types = c("p"),
                             elic_type = c("13")),
                  error = TRUE)
  # When there are less variables than variable types and estimate types
  expect_snapshot(elic_start(var_names = c("var1"),
                             var_types = c("pR"),
                             elic_type = c("13")),
                  error = TRUE)
  # When there are more variables than variable types (elic_types is recycled)
  expect_snapshot(elic_start(var_names = c("var1", "var2", "var3"),
                             var_types = c("pN"),
                             elic_type = c("1")),
                  error = TRUE)
  # When there are more variables than estimate types (var_types is recycled)
  expect_snapshot(elic_start(var_names = c("var1", "var2", "var3"),
                             var_types = c("p"),
                             elic_type = c("13")),
                  error = TRUE)
  # When there are more variables than variable types and estimate types
  expect_snapshot(elic_start(var_names = c("var1", "var2", "var3"),
                             var_types = c("pN"),
                             elic_type = c("13")),
                  error = TRUE)
  # When variable names,variable types and estimate types are all not compatible
  expect_snapshot(elic_start(var_names = c("var1", "var2", "var3"),
                             var_types = c("pN"),
                             elic_type = c("1344")),
                  error = TRUE)
})

test_that("Output format", {
  x <- elic_start(var_names = c("var1", "var2"),
                  var_types = "pR",
                  elic_types = "43")
  expect_s3_class(x, class = "elicit", exact = TRUE)
  # Variable names are recorded in the object
  expect_identical(x$var_names, c("var1", "var2"))
  expect_type(x$var_names, "character")
  # Variable type short codes are split
  expect_identical(x$var_types, c("p", "R"))
  expect_type(x$var_types, "character")
  # Elicitation type short codes are split
  expect_identical(x$elic_types, c("4p", "3p"))
  expect_type(x$elic_types, "character")
  # Data is present and empty
  expect_type(x$data, "list")
  expect_null(x$data$round_1)
  expect_null(x$data$round_2)
  # Title attribute is created
  expect_false(is.null(attr(x, "title")))
})
