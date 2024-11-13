googlesheets4::gs4_deauth()
# Test import_data()----
test_that("Raises errors ", {
  file <- withr::local_file("test.txt",
                            code = {writeLines("", "test.txt")})
  # When the file doesn't exist
  expect_snapshot(import_data("test.csv",
                              var_names = "var1",
                              var_types = "p",
                              elic_type = "1"),
                  error = TRUE)
  # When the file extension is not supported
  expect_snapshot(import_data(file,
                              var_names = "var1",
                              var_types = "p",
                              elic_type = "1"),
                  error = TRUE)
  # When 1 variable type is not allowed
  expect_snapshot(import_data("abc",
                              var_names = c("var1", "var2", "var3"),
                              var_types = "pqR",
                              elic_type = "1"),
                  error = TRUE)
  # When 2 variable types are not allowed
  expect_snapshot(import_data("abc",
                              var_names = c("var1", "var2", "var3"),
                              var_types = "pqG",
                              elic_type = "1"),
                  error = TRUE)
  # When 1 estimate type is not allowed
  expect_snapshot(import_data("abc",
                              var_names = c("var1", "var2", "var3"),
                              var_types = "p",
                              elic_type = "123"),
                  error = TRUE)
  # When 2 estimate types are not allowed
  expect_snapshot(import_data("abc",
                              var_names = c("var1", "var2", "var3"),
                              var_types = "p",
                              elic_type = "1237"),
                  error = TRUE)
  # When there are less variables than variable types
  expect_snapshot(import_data("abs",
                              var_names = c("var1"),
                              var_types = c("pR"),
                              elic_type = c("1")),
                  error = TRUE)
  # When there are less variables than estimate types
  expect_snapshot(import_data("abs",
                              var_names = c("var1"),
                              var_types = c("p"),
                              elic_type = c("13")),
                  error = TRUE)
  # When there are less variables than variable types and estimate types
  expect_snapshot(import_data("abs",
                              var_names = c("var1"),
                              var_types = c("pR"),
                              elic_type = c("13")),
                  error = TRUE)
  # When there are more variables than variable types
  expect_snapshot(import_data("abs",
                              var_names = c("var1", "var2", "var3"),
                              var_types = c("pN"),
                              elic_type = c("1")),
                  error = TRUE)
  # When there are more variables than estimate types
  expect_snapshot(import_data("abs",
                              var_names = c("var1", "var2", "var3"),
                              var_types = c("p"),
                              elic_type = c("13")),
                  error = TRUE)
  # When there are more variables than variable types and estimate types
  expect_snapshot(import_data("abs",
                              var_names = c("var1", "var2", "var3"),
                              var_types = c("pN"),
                              elic_type = c("13")),
                  error = TRUE)
  # When variable names,variable types and estimate types are all not compatible
  expect_snapshot(import_data("abs",
                              var_names = c("var1", "var2", "var3"),
                              var_types = c("pN"),
                              elic_type = c("1344")),
                  error = TRUE)
  # When column names are incorrect
  file <- system.file("extdata", package = "elicitr") |>
    list.files(pattern = "xlsx",
               full.names = TRUE)
  expect_snapshot(import_data(file,
                              var_names = c("var1", "var2", "var5"),
                              var_types = "p",
                              elic_types = "134"),
                  error = TRUE)
})

# Test get_labels()----
test_that("Generates correct labels", {
  ## 1 Variable----
  # 1 point elicitation
  expect_identical(get_labels(n = 1,
                              elic_types = "1p"),
                   "best")
  # 3 point elicitation
  expect_identical(get_labels(n = 1,
                              elic_types = "3p"),
                   c("min", "max", "best"))
  # 4 point elicitation
  expect_identical(get_labels(n = 1,
                              elic_types = "4p"),
                   c("min", "max", "best", "conf"))

  ## 2 Variables----
  # Same elicitation types
  expect_identical(get_labels(n = 2,
                              elic_types = c("3p", "3p")),
                   c("min", "max", "best", "min", "max", "best"))
  # Same elicitation types recycling `elic_type`
  expect_identical(get_labels(n = 2,
                              elic_types = "3p"),
                   c("min", "max", "best", "min", "max", "best"))
  # Different elicitation types
  expect_identical(get_labels(n = 2,
                              elic_types = c("1p", "3p")),
                   c("best", "min", "max", "best"))
  # Different elicitation types recycling `elic_type`
  expect_identical(get_labels(n = 2,
                              elic_types = "3p"),
                   c("min", "max", "best", "min", "max", "best"))
})

# Test get_col_names()----
test_that("Generates correct column names", {
  expect_identical(get_col_names(c("var1", "var2", "var3"),
                                 c("1p", "3p", "4p")),
                   c("id",
                     "var1_best",
                     "var2_min", "var2_max", "var2_best",
                     "var3_min", "var3_max", "var3_best", "var3_conf"))
})
