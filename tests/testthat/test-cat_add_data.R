test_that("Errors", {
  x <- elic_cat_start(levels = paste0("level_", 1:5),
                      sites = paste0("site_", 1:4),
                      experts = 6,
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

  # When there are more experts than expected
  y <- mechanism_1
  new_expert <- y[1:5, ]
  new_expert[["name"]] <- "new_expert"
  y <- rbind(y, new_expert)
  expect_snapshot(elic_cat_add_data(x,
                                    data_source = y,
                                    mechanism = "mechanism_1"),
                  error = TRUE)

  # When one level is not on the metadata
  y <- mechanism_1
  y[1, 2] <- "level_6"
  expect_snapshot(elic_cat_add_data(x,
                                    data_source = y,
                                    mechanism = "mechanism_1"),
                  error = TRUE)

  # When two levels are not on the metadata
  y <- mechanism_1
  y[1, 2] <- "level_6"
  y[2, 2] <- "level_7"
  expect_snapshot(elic_cat_add_data(x,
                                    data_source = y,
                                    mechanism = "mechanism_1"),
                  error = TRUE)

  # When one site is not on the metadata
  y <- mechanism_1
  y[1:5, 3] <- "site_5"
  expect_snapshot(elic_cat_add_data(x,
                                    data_source = y,
                                    mechanism = "mechanism_1"),
                  error = TRUE)

  # When two sites are not on the metadata
  y <- mechanism_1
  y[1:5, 3] <- "site_5"
  y[6:10, 3] <- "site_6"
  expect_snapshot(elic_cat_add_data(x,
                                    data_source = y,
                                    mechanism = "mechanism_1"),
                  error = TRUE)

  # # When the column with the names is malformed
  # y <- mechanism_1[-1, ]
  # y[1:4, 5] <- 0.25
  # expect_snapshot(elic_cat_add_data(x,
  #                                   data_source = y,
  #                                   mechanism = "mechanism_1"),
  #                 error = TRUE)
  #


  # # When estimates don't sum to 1 for one expert and site
  # y <- mechanism_1
  # y[1, 5] <- 0.99
  # expect_snapshot(elic_cat_add_data(x,
  #                                   data_source = y,
  #                                   mechanism = "mechanism_1"),
  #                 error = TRUE)
  #
  # # When estimates don't sum to 1 for more experts and sites
  # y[19, 5] <- 0.99
  # y[120, 5] <- 0.99
  # expect_snapshot(elic_cat_add_data(x,
  #                                   data_source = y,
  #                                   mechanism = "mechanism_1"),
  #                 error = TRUE)
})
