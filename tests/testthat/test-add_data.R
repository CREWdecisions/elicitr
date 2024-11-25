# Test elic_add_data()----
test_that("Raises errors ", {
  x <- elic_start(var_names = c("var1", "var2", "var3"),
                  var_types = "ZNp",
                  elic_types = "134",
                  experts = 6,
                  verbose = FALSE)

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
  # When data for round 2 are added before those for round 1
  expect_snapshot(elic_add_data(x, data_source = round_1, round = 2),
                  error = TRUE)
  # When trying to overwrite a dataset
  y <- elic_add_data(x, data_source = round_1, round = 1, verbose = FALSE)
  expect_snapshot(elic_add_data(y, data_source = round_1, round = 1),
                  error = TRUE)
  # When the number of columns is different from expected
  expect_snapshot(elic_add_data(x, data_source = round_1[, -1], round = 1),
                  error = TRUE)
  # When there are less experts than number of rows in dataset
  expect_snapshot(elic_add_data(x, data_source = rbind(round_1, round_1),
                                round = 1, verbose = FALSE),
                  error = TRUE)
  # When x is not an elicit object
  expect_snapshot(elic_add_data("abc", data_source = round_1, round = 1),
                  error = TRUE)
  # When round is neither 1 nor 2
  expect_snapshot(elic_add_data(x, data_source = round_1, round = 3),
                  error = TRUE)
  expect_snapshot(elic_add_data(x, data_source = round_1, round = 0),
                  error = TRUE)
  # When >=2 id are present in Round 2 but not in Round 1 and there are not NAs
  y <- elic_add_data(x, data_source = round_1, round = 1, verbose = FALSE)
  z <- round_2
  z$name[3] <- "Jane Doe"
  z$name[4] <- "John Smith"
  expect_snapshot(out <- elic_add_data(y, data_source = z,
                                       round = 2, verbose = FALSE),
                  error = TRUE)
  # When 1 id is present in Round 2 but not in Round 1 and there are NAs in
  # Round 2
  z <- round_2[1:4, ]
  z$name[3] <- "Jane Doe"
  expect_snapshot(out <- elic_add_data(y, data_source = z,
                                       round = 2, verbose = FALSE),
                  error = TRUE)
  # When >=2 id are present in Round 2 but not in Round 1 and there are NAs in
  # Round 2
  z$name[4] <- "John Smith"
  expect_snapshot(out <- elic_add_data(y, data_source = z,
                                       round = 2, verbose = FALSE),
                  error = TRUE)
})

test_that("Raises warns", {
  x <- elic_start(var_names = c("var1", "var2", "var3"),
                  var_types = "ZNp",
                  elic_types = "134",
                  experts = 6,
                  verbose = FALSE)
  # When there are less entries in dataset than experts for Round 1
  expect_snapshot(y <- elic_add_data(x, data_source = round_1[1:4, ],
                                     round = 1, verbose = FALSE))
  idx <- setdiff(seq_along(round_1$name), seq_along(round_1$name[1:4]))
  for (i in seq_along(idx)) {
    expect_identical(as.numeric(y$data$round_1[idx[i], -1]),
                     rep(NA_real_, (ncol(round_1) - 1)))
  }
  # When one id is present in Round 2 but not in Round 1 and there are not NAs
  y <- elic_add_data(x, data_source = round_1, round = 1, verbose = FALSE)
  z <- round_2
  z$name[3] <- "Jane Doe"
  expect_snapshot(out <- elic_add_data(y, data_source = z,
                                       round = 2, verbose = FALSE))
  expect_identical(out$data$round_1$id, out$data$round_2$id)
  idx <- match(round_1$name, round_2$name)
  expect_identical(out$data$round_2[, -1], round_2[idx, -1])
  # When Round 2 has less entries than Round 1 but all its ids are in Round 1
  z <- round_2[1:4, ]
  expect_snapshot(out <- elic_add_data(y, data_source = z,
                                       round = 2, verbose = FALSE))
  idx <- setdiff(seq_along(round_1$name), match(z$name, round_1$name))
  for (i in seq_along(idx)) {
    expect_identical(as.numeric(out$data$round_2[idx[i], -1]),
                     rep(NA_real_, (ncol(round_2) - 1)))
  }
})

test_that("Output format", {
  # Column names are taken from the metadata and have the correct suffix
  x <- elic_start(var_names = c("cat", "dog", "fish"),
                  var_types = "ZNp",
                  elic_types = "134",
                  experts = 6,
                  verbose = FALSE)
  y <- round_1
  colnames(y) <- letters[1:9]
  z <- elic_add_data(x, data_source = y, round = 1, verbose = FALSE) |>
    elic_add_data(data_source = round_2, round = 2, verbose = FALSE)
  cols <- c("id", "cat_best", "dog_min", "dog_max", "dog_best",
            "fish_min", "fish_max", "fish_best", "fish_conf")
  expect_identical(colnames(z$data$round_1), cols)
  expect_identical(colnames(z$data$round_2), cols)
  # Column id should have values with 7 characters
  expect_identical(nchar(z$data$round_1$id), rep(7L, nrow(z$data$round_1)))
  expect_identical(nchar(z$data$round_2$id), rep(7L, nrow(z$data$round_2)))
  # Id order should be the same in Round 1 and Round 2
  expect_identical(z$data$round_1$id, z$data$round_2$id)
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

# Test clean_gs_data()----
test_that("Data are cleaned", {
  # Column with timestamps are removed
  x <- data.frame(a = rep(Sys.time(), 3),
                  b = letters[1:3],
                  c = c("0.9", "0,8", "0.7")) |>
    clean_gs_data()
  expect_length(x, 2)
  expect_named(x, c("b", "c"))
  # First column is not converted to double (it should contain the names)
  expect_type(x$b, "character")
  expect_type(x$c, "double")

  # Columns containing lists are converted to a numeric vector
  x <- data.frame(a = letters[1:5],
                  b = 1:5) |>
    dplyr::mutate(b = as.list(b)) |>
    clean_gs_data()
  expect_type(x$a, "character")
  expect_identical(x$b, as.numeric(1:5))
  expect_vector(x$b, ptype = double(), size = 5)

  # In column containing mixed decimal separators, commas are replaced with
  # periods and then characters are converted to numeric
  x <- data.frame(a = letters[1:3],
                  b = c("0.9", "0,8", "0.7")) |>
    clean_gs_data()
  expect_type(x$a, "character")
  expect_identical(x$b, c(0.9, 0.8, 0.7))
  expect_vector(x$b, ptype = double(), size = 3)
})

# Test get_data_index()----
# omogenise_datasets("The index is correct", {
#   x <- c("a", "b", "c", "d")
#   y <- c("c", "a", "d", "b")
#   idx <- get_data_index(x, y)
#   expect_identical(x, y[idx])
# })
