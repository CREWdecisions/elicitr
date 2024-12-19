test_that("Errors", {
  x <- cat_start(categories = paste0("category_", 1:5),
                 sites = paste0("site_", 1:4),
                 experts = 6,
                 topics = c("topic_1", "topic_2"),
                 verbose = FALSE)

  # When x is not an elic_cat object
  expect_snapshot(cat_add_data("abc",
                               data_source = topic_1,
                               topic = "topic_1"),
                  error = TRUE)

  file <- withr::local_file("test.txt",
                            code = {writeLines("", "test.txt")}) # nolint

  # When the file doesn't exist
  expect_snapshot(cat_add_data(x,
                               data_source = "test.csv",
                               topic = "topic_1"),
                  error = TRUE)

  # When the file extension is not supported
  expect_snapshot(cat_add_data(x,
                               data_source = file,
                               topic = "topic_1"),
                  error = TRUE)

  # When topic is not a character string
  expect_snapshot(cat_add_data(x,
                               data_source = topic_1,
                               topic = 1),
                  error = TRUE)

  # When topic is a character string of length > 1
  expect_snapshot(cat_add_data(x,
                               data_source = topic_1,
                               topic = c("topic_1", "topic_2")),
                  error = TRUE)

  # When topic is not among the available topics
  expect_snapshot(cat_add_data(x,
                               data_source = topic_1,
                               topic = "topic_3"),
                  error = TRUE)

  # When data has the wrong number of columns
  expect_snapshot(cat_add_data(x,
                               data_source = topic_1[, 1:4],
                               topic = "topic_1"),
                  error = TRUE)

  # When data has 1 column with the wrong column type
  y <- topic_1 |>
    dplyr::mutate(confidence = as.character(confidence))
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               topic = "topic_1"),
                  error = TRUE)
  # When data has 2 columns with the wrong column types
  y <- topic_1 |>
    dplyr::mutate(dplyr::across(2:3, as.factor))
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               topic = "topic_1"),
                  error = TRUE)

  # When there are more experts than expected
  y <- topic_1
  new_expert <- y[1:5, ]
  new_expert[["name"]] <- "new_expert"
  y <- rbind(y, new_expert)
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               topic = "topic_1"),
                  error = TRUE)

  # When one category is not on the metadata
  y <- topic_1
  y[1, 2] <- "category_6"
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               topic = "topic_1"),
                  error = TRUE)

  # When two categories are not on the metadata
  y <- topic_1
  y[1, 2] <- "category_6"
  y[2, 2] <- "category_7"
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               topic = "topic_1"),
                  error = TRUE)

  # When one site is not on the metadata
  y <- topic_1
  y[1:5, 3] <- "site_5"
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               topic = "topic_1"),
                  error = TRUE)

  # When two sites are not on the metadata
  y <- topic_1
  y[1:5, 3] <- "site_5"
  y[6:10, 3] <- "site_6"
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               topic = "topic_1"),
                  error = TRUE)

  # When the column with the names is malformed
  y <- topic_1[-1, ]
  y[1:4, 5] <- 0.25
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               topic = "topic_1"),
                  error = TRUE)

  # When the column with the categories is malformed
  y <- topic_1
  y[1:5, 2] <- "category_1"
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               topic = "topic_1"),
                  error = TRUE)

  # When the column with the sites is malformed
  y <- topic_1
  y[1:5, 3] <- c(paste0("site_", 1:4), "site_1")
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               topic = "topic_1"),
                  error = TRUE)

  # When the column with the confidence values is malformed
  y <- topic_1
  y[1:5, 4] <- 1:5
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               topic = "topic_1"),
                  error = TRUE)

  # When estimates don't sum to 1 for one expert and site
  y <- topic_1
  y[1, 5] <- 0.99
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               topic = "topic_1"),
                  error = TRUE)

  # When estimates don't sum to 1 for more experts and sites
  y[19, 5] <- 0.99
  y[120, 5] <- 0.99
  expect_snapshot(cat_add_data(x,
                               data_source = y,
                               topic = "topic_1"),
                  error = TRUE)
})

test_that("Info", {
  x <- cat_start(categories = paste0("category_", 1:5),
                 sites = paste0("site_", 1:4),
                 experts = 6,
                 topics = c("topic_1", "topic_2"),
                 verbose = FALSE)

  # Success adding data.frame
  expect_snapshot(out <- cat_add_data(x,
                                      data_source = topic_1,
                                      topic = "topic_1"))
  expect_identical(out[["data"]][["topic_1"]][, -1], topic_1[, -1])
  hashed_id <- dplyr::pull(topic_1, "name") |>
    stand_names() |>
    hash_names()
  expect_identical(dplyr::pull(out[["data"]][["topic_1"]], "id"), hashed_id)

  # Success adding csv file
  files <- list.files(path = system.file("extdata", package = "elicitr"),
                      pattern = "topic_",
                      full.names = TRUE)
  expect_snapshot(out <- cat_add_data(x,
                                      data_source = files[[1]],
                                      topic = "topic_1"))
  # Test equal and not identical because when saved as file we loose precision
  expect_equal(out[["data"]][["topic_1"]][, -1], topic_1[, -1],
               tolerance = testthat_tolerance())
  hashed_id <- dplyr::pull(topic_1, "name") |>
    stand_names() |>
    hash_names()
  expect_identical(dplyr::pull(out[["data"]][["topic_1"]], "id"), hashed_id)

  # Success adding xlsx file
  file <- list.files(path = system.file("extdata", package = "elicitr"),
                     pattern = "topics",
                     full.names = TRUE)
  expect_snapshot(out <- cat_add_data(x,
                                      data_source = file,
                                      topic = "topic_1"))
  # Test equal and not identical because when saved as file we loose precision
  expect_equal(out[["data"]][["topic_1"]][, -1], topic_1[, -1],
               tolerance = testthat_tolerance())
  hashed_id <- dplyr::pull(topic_1, "name") |>
    stand_names() |>
    hash_names()
  expect_identical(dplyr::pull(out[["data"]][["topic_1"]], "id"), hashed_id)

  # Data imported from Google Sheets
  googlesheets4::gs4_deauth()
  # Google Sheet used for testing
  gs <- "18VHeHB89P1s-6banaVoqOP-ggFmQZYx-z_31nMffAb8"
  x <- cat_start(categories = paste0("category_", 1:5),
                 sites = paste0("site_", 1:4),
                 experts = 6,
                 topics = c("topic_1", "topic_2"),
                 verbose = FALSE)
  expect_snapshot(out <- cat_add_data(x,
                                      data_source = gs,
                                      topic = "topic_1"))
  # Test equal and not identical because when saved as file we loose precision
  expect_equal(out[["data"]][["topic_1"]][, -1], topic_1[, -1],
               tolerance = testthat_tolerance())
  hashed_id <- dplyr::pull(topic_1, "name") |>
    stand_names() |>
    hash_names()
  expect_identical(dplyr::pull(out[["data"]][["topic_1"]], "id"), hashed_id)
})
