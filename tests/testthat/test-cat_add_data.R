test_that("Errors", {
  x <- cat_start(levels = paste0("level_", 1:5),
                 sites = paste0("site_", 1:4),
                 experts = 6,
                 mechanisms = c("mechanism_1", "mechanism_2"),
                 verbose = FALSE)

  # When x is not an elic_cat object
  expect_snapshot(cat_add_data("abc",
                               data_source = mechanism_1,
                               mechanism = "mechanism_1"),
                  error = TRUE)

  file <- withr::local_file("test.txt",
                            code = {writeLines("", "test.txt")}) # nolint

  # When the file doesn't exist
  expect_snapshot(cat_add_data(x,
                               data_source = "test.csv",
                               mechanism = "mechanism_1"),
                  error = TRUE)

  # When the file extension is not supported
  expect_snapshot(cat_add_data(x,
                               data_source = file,
                               mechanism = "mechanism_1"),
                  error = TRUE)

  # When mechanism is not a character string
  expect_snapshot(cat_add_data(x,
                               data_source = mechanism_1,
                               mechanism = 1),
                  error = TRUE)

  # When mechanism is a character string of length > 1
  expect_snapshot(cat_add_data(x,
                               data_source = mechanism_1,
                               mechanism = c("mechanism_1", "mechanism_2")),
                  error = TRUE)

  # When mechanism is not among the available mechanisms
  expect_snapshot(cat_add_data(x,
                               data_source = mechanism_1,
                               mechanism = "mechanism_3"),
                  error = TRUE)

  # When data has the wrong number of columns
  expect_snapshot(cat_add_data(x,
                               data_source = mechanism_1[, 1:4],
                               mechanism = "mechanism_1"),
                  error = TRUE)

  # When data has 1 column with the wrong column type
  y <- mechanism_1 |>
    dplyr::mutate(confidence = as.character(confidence))
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               mechanism = "mechanism_1"),
                  error = TRUE)
  # When data has 2 columns with the wrong column types
  y <- mechanism_1 |>
    dplyr::mutate(dplyr::across(2:3, as.factor))
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               mechanism = "mechanism_1"),
                  error = TRUE)

  # When there are more experts than expected
  y <- mechanism_1
  new_expert <- y[1:5, ]
  new_expert[["name"]] <- "new_expert"
  y <- rbind(y, new_expert)
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               mechanism = "mechanism_1"),
                  error = TRUE)

  # When one level is not on the metadata
  y <- mechanism_1
  y[1, 2] <- "level_6"
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               mechanism = "mechanism_1"),
                  error = TRUE)

  # When two levels are not on the metadata
  y <- mechanism_1
  y[1, 2] <- "level_6"
  y[2, 2] <- "level_7"
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               mechanism = "mechanism_1"),
                  error = TRUE)

  # When one site is not on the metadata
  y <- mechanism_1
  y[1:5, 3] <- "site_5"
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               mechanism = "mechanism_1"),
                  error = TRUE)

  # When two sites are not on the metadata
  y <- mechanism_1
  y[1:5, 3] <- "site_5"
  y[6:10, 3] <- "site_6"
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               mechanism = "mechanism_1"),
                  error = TRUE)

  # When the column with the names is malformed
  y <- mechanism_1[-1, ]
  y[1:4, 5] <- 0.25
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               mechanism = "mechanism_1"),
                  error = TRUE)

  # When the column with the levels is malformed
  y <- mechanism_1
  y[1:5, 2] <- "level_1"
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               mechanism = "mechanism_1"),
                  error = TRUE)

  # When the column with the sites is malformed
  y <- mechanism_1
  y[1:5, 3] <- c(paste0("site_", 1:4), "site_1")
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               mechanism = "mechanism_1"),
                  error = TRUE)

  # When the column with the confidence values is malformed
  y <- mechanism_1
  y[1:5, 4] <- 1:5
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               mechanism = "mechanism_1"),
                  error = TRUE)

  # When estimates don't sum to 1 for one expert and site
  y <- mechanism_1
  y[1, 5] <- 0.99
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               mechanism = "mechanism_1"),
                  error = TRUE)

  # When estimates don't sum to 1 for more experts and sites
  y[19, 5] <- 0.99
  y[120, 5] <- 0.99
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               mechanism = "mechanism_1"),
                  error = TRUE)
})

test_that("Info", {
  x <- cat_start(levels = paste0("level_", 1:5),
                 sites = paste0("site_", 1:4),
                 experts = 6,
                 mechanisms = c("mechanism_1", "mechanism_2"),
                 verbose = FALSE)

  # Success adding data.frame
  expect_snapshot(out <- cat_add_data(x,
                                      data_source = mechanism_1,
                                      mechanism = "mechanism_1"))
  expect_identical(out[["data"]][["mechanism_1"]][, -1], mechanism_1[, -1])
  hashed_id <- dplyr::pull(mechanism_1, "name") |>
    stand_names() |>
    hash_names()
  expect_identical(dplyr::pull(out[["data"]][["mechanism_1"]], "id"), hashed_id)

  # Success adding csv file
  files <- list.files(path = system.file("extdata", package = "elicitr"),
                      pattern = "mechanism_",
                      full.names = TRUE)
  expect_snapshot(out <- cat_add_data(x,
                                      data_source = files[[1]],
                                      mechanism = "mechanism_1"))
  # Test equal and not identical because when saved as file we loose precision
  expect_equal(out[["data"]][["mechanism_1"]][, -1], mechanism_1[, -1],
               tolerance = testthat_tolerance())
  hashed_id <- dplyr::pull(mechanism_1, "name") |>
    stand_names() |>
    hash_names()
  expect_identical(dplyr::pull(out[["data"]][["mechanism_1"]], "id"), hashed_id)

  # Success adding xlsx file
  file <- list.files(path = system.file("extdata", package = "elicitr"),
                     pattern = "mechanisms",
                     full.names = TRUE)
  expect_snapshot(out <- cat_add_data(x,
                                      data_source = file,
                                      mechanism = "mechanism_1"))
  # Test equal and not identical because when saved as file we loose precision
  expect_equal(out[["data"]][["mechanism_1"]][, -1], mechanism_1[, -1],
               tolerance = testthat_tolerance())
  hashed_id <- dplyr::pull(mechanism_1, "name") |>
    stand_names() |>
    hash_names()
  expect_identical(dplyr::pull(out[["data"]][["mechanism_1"]], "id"), hashed_id)

  # Data imported from Google Sheets
  googlesheets4::gs4_deauth()
  # Google Sheet used for testing
  gs <- "18VHeHB89P1s-6banaVoqOP-ggFmQZYx-z_31nMffAb8"
  x <- cat_start(levels = paste0("level_", 1:5),
                 sites = paste0("site_", 1:4),
                 experts = 6,
                 mechanisms = c("mechanism_1", "mechanism_2"),
                 verbose = FALSE)
  expect_snapshot(out <- cat_add_data(x,
                                      data_source = gs,
                                      mechanism = "mechanism_1"))
  # Test equal and not identical because when saved as file we loose precision
  expect_equal(out[["data"]][["mechanism_1"]][, -1], mechanism_1[, -1],
               tolerance = testthat_tolerance())
  hashed_id <- dplyr::pull(mechanism_1, "name") |>
    stand_names() |>
    hash_names()
  expect_identical(dplyr::pull(out[["data"]][["mechanism_1"]], "id"), hashed_id)
})
