test_that("Errors", {
  x <- elic_cat_start(levels = c("level_1", "level_2"),
                      sites = c("site_1", "site_2", "site_3"),
                      experts = 8,
                      mechanisms = c("mechanism_1", "mechanism_2"),
                      verbose = FALSE)

  # When x is not an elic_cat object
  expect_snapshot(elic_cat_add_data("abc",
                                    data_source = mechanism_1,
                                    mechanism = "mechanism_1"),
                  error = TRUE)

  file <- withr::local_file("test.txt",
                            code = {writeLines("", "test.txt")}) # nolint

  # When the file doesn't exist
  expect_snapshot(elic_cat_add_data(x,
                                    data_source = "test.csv",
                                    mechanism = "mechanism_1"),
                  error = TRUE)

  # When the file extension is not supported
  expect_snapshot(elic_cat_add_data(x,
                                    data_source = file,
                                    mechanism = "mechanism_1"),
                  error = TRUE)

  # When mechanism is not a character string
  expect_snapshot(elic_cat_add_data(x,
                                    data_source = mechanism_1,
                                    mechanism = 1),
                  error = TRUE)

  # When mechanism is a character string of length > 1
  expect_snapshot(elic_cat_add_data(x,
                                    data_source = mechanism_1,
                                    mechanism = c("mechanism_1",
                                                  "mechanism_2")),
                  error = TRUE)

  # When mechanism is not among the available mechanisms
  expect_snapshot(elic_cat_add_data(x,
                                    data_source = mechanism_1,
                                    mechanism = "mechanism_3"),
                  error = TRUE)

  # When data has the wrong number of columns
  expect_snapshot(elic_cat_add_data(x,
                                    data_source = mechanism_1[, 1:4],
                                    mechanism = "mechanism_1"),
                  error = TRUE)

  # When data has 1 column with the wrong column type
  y <- mechanism_1 |>
    dplyr::mutate(confidence = as.character(confidence))
  expect_snapshot(elic_cat_add_data(x,
                                    data_source = y,
                                    mechanism = "mechanism_1"),
                  error = TRUE)
  # When data has 2 columns with the wrong column types
  y <- mechanism_1 |>
    dplyr::mutate(dplyr::across(2:3, as.factor))
  expect_snapshot(elic_cat_add_data(x,
                                    data_source = y,
                                    mechanism = "mechanism_1"),
                  error = TRUE)
  # When estimates don't sum to 1 for one expert and site
  y <- mechanism_1
  y[1, 5] <- 0.99
  expect_snapshot(elic_cat_add_data(x,
                                    data_source = y,
                                    mechanism = "mechanism_1"),
                  error = TRUE)
})
