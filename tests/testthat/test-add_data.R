# Test elic_add_data()----
test_that("Raises errors ", {
  x <- elic_start(var_names = c("var1", "var2", "var3"),
                  var_types = "Nrp",
                  elic_types = "134")

  file <- withr::local_file("test.txt",
                            code = {writeLines("", "test.txt")})
  # When the file doesn't exist
  expect_snapshot(elic_add_data(x,
                                data_source = "test.csv",
                                round = 1),
                  error = TRUE)
  # When the file extension is not supported
  expect_snapshot(elic_add_data(x,
                                data_source = file,
                                round = 1),
                  error = TRUE)
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

# Test stand_names()----
test_that("Names are standardised", {
  # Capital letters are changed to lower case
  expect_identical(stand_names(c("JaneDoe", "JohnSmith")),
                   c("janedoe", "johnsmith"))
  # White spaces are removed
  expect_identical(stand_names(c("Jane Doe", "John J Smith")),
                   c("janedoe", "johnjsmith"))
  # Punctuation is removed
  expect_identical(stand_names(c("J. Doe", "J, J_Smith")),
                   c("jdoe", "jjsmith"))
})

# Test hash_names()----
test_that("Names are converted to short sha1 hashes", {
  # The function is vectorised and returns strings with 7 characters
  x <- hash_names(c("Jane Doe", "John Smith"))
  expect_length(x, 2)
  expect_identical(nchar(x), c(7L, 7L))
})
