test_that("Errors", {
  # check_arg_length()----
  # When length variable types > 1
  expect_snapshot(cont_start(var_names = "var1",
                             var_types = c("p", "N"),
                             elic_types = "3",
                             experts = 3),
                  error = TRUE)
  # When length elicitation types > 1
  expect_snapshot(cont_start(var_names = "var1",
                             var_types = "p",
                             elic_types = c("4", "3"),
                             experts = 3),
                  error = TRUE)
  # check_arg_types()----
  # When 1 short code for var_types is not allowed
  expect_snapshot(cont_start(var_names = c("var1", "var2", "var3"),
                             var_types = "pqR",
                             elic_types = "1",
                             experts = 3),
                  error = TRUE)
  # When 1 short code for var_types is not allowed and is repeated twice
  expect_snapshot(cont_start(var_names = c("var1", "var2", "var3"),
                             var_types = "apa",
                             elic_types = "1",
                             experts = 3),
                  error = TRUE)
  # When 2 short codes for var_types are not allowed
  expect_snapshot(cont_start(var_names = c("var1", "var2", "var3"),
                             var_types = "pqG",
                             elic_types = "1",
                             experts = 3),
                  error = TRUE)
  # When one short code for elic_types is not allowed
  expect_snapshot(cont_start(var_names = c("var1", "var2", "var3"),
                             var_types = "p",
                             elic_types = "123",
                             experts = 3),
                  error = TRUE)
  # When 1 short code for elic_types is not allowed and is repeated twice
  expect_snapshot(cont_start(var_names = c("var1", "var2", "var3"),
                             var_types = "p",
                             elic_types = "232",
                             experts = 3),
                  error = TRUE)
  # When 2 short codes for elic_types are not allowed
  expect_snapshot(cont_start(var_names = c("var1", "var2", "var3"),
                             var_types = "p",
                             elic_types = "1237",
                             experts = 3),
                  error = TRUE)
  # check_arg_mism()----
  # When there are less variables than variable types
  expect_snapshot(cont_start(var_names = "var1",
                             var_types = "pR",
                             elic_types = "1",
                             experts = 3),
                  error = TRUE)
  # When there are less variables than estimate types
  expect_snapshot(cont_start(var_names = "var1",
                             var_types = "p",
                             elic_types = "13",
                             experts = 3),
                  error = TRUE)
  # When there are less variables than variable types and estimate types
  expect_snapshot(cont_start(var_names = "var1",
                             var_types = "pR",
                             elic_types = "13",
                             experts = 3),
                  error = TRUE)
  # When there are more variables than variable types (elic_types is recycled)
  expect_snapshot(cont_start(var_names = c("var1", "var2", "var3"),
                             var_types = "pN",
                             elic_types = "1",
                             experts = 3),
                  error = TRUE)
  # When there are more variables than estimate types (var_types is recycled)
  expect_snapshot(cont_start(var_names = c("var1", "var2", "var3"),
                             var_types = "p",
                             elic_types = "13",
                             experts = 3),
                  error = TRUE)
  # When there are more variables than variable types and estimate types
  expect_snapshot(cont_start(var_names = c("var1", "var2", "var3"),
                             var_types = "pN",
                             elic_types = "13",
                             experts = 3),
                  error = TRUE)
  # When variable names and types and estimate types are all not compatible
  expect_snapshot(cont_start(var_names = c("var1", "var2", "var3"),
                             var_types = "pN",
                             elic_types = "1344",
                             experts = 3),
                  error = TRUE)
  # check_experts_arg()----
  # When experts is provided as character
  expect_snapshot(cont_start(var_names = "var1",
                             var_types = "p",
                             elic_types = "1",
                             experts = "3"),
                  error = TRUE)
  # When experts is provided as numeric vector
  expect_snapshot(cont_start(var_names = "var1",
                             var_types = "p",
                             elic_types = "1",
                             experts = 1:2),
                  error = TRUE)
})

test_that("Info", {
  expect_snapshot(x <- cont_start(var_names = c("var1", "var2"),
                                  var_types = "pR",
                                  elic_types = "43",
                                  experts = 3))
})

test_that("Output", {
  expect_no_message(x <- cont_start(var_names = c("var1", "var2"),
                                    var_types = "pR",
                                    elic_types = "43",
                                    experts = 3,
                                    verbose = FALSE))
  expect_s3_class(x, class = "elic_cont", exact = TRUE)
  # Variable names are recorded in the object
  expect_identical(x[["var_names"]], c("var1", "var2"))
  expect_type(x[["var_names"]], "character")
  # Variable type short codes are split
  expect_identical(x[["var_types"]], c("p", "R"))
  expect_type(x[["var_types"]], "character")
  # Elicitation type short codes are split
  expect_identical(x[["elic_types"]], c("4p", "3p"))
  expect_type(x[["elic_types"]], "character")
  # Number of experts are present in the object
  expect_identical(x[["experts"]], 3)
  # Data is present and empty
  expect_type(x[["data"]], "list")
  expect_null(x[["data"]][["round_1"]])
  expect_null(x[["data"]][["round_2"]])
  expect_named(x[["data"]], c("round_1", "round_2"))
  # Title attribute is created
  expect_false(is.null(attr(x, "title")))
  # Object structure
  expect_snapshot_value(x, style = "deparse")
})
