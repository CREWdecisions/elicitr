test_that("Errors", {
  obj <- create_cat_obj()
  samp <- cat_sample_data(obj, method = "unweighted", topic = "topic_1",
                          verbose = FALSE)

  # When option is not available in the object
  expect_snapshot(summary(samp, option = "option_7"),
                  error = TRUE)

  # When the whole data is NA
  # One option
  option <- which(obj[["data"]][["topic_1"]][["option"]] == "option_1")
  obj[["data"]][["topic_1"]][option, 4:5] <- NA
  samp <- cat_sample_data(obj, method = "unweighted", topic = "topic_1",
                          verbose = FALSE)
  expect_snapshot(summary(samp, option = "option_1"),
                  error = TRUE)

  # All options
  obj[["data"]][["topic_1"]][, 4:5] <- NA
  samp <- cat_sample_data(obj, method = "unweighted", topic = "topic_1",
                          verbose = FALSE)
  expect_snapshot(summary(samp),
                  error = TRUE)

})

test_that("Output", {
  obj <- create_cat_obj()
  samp <- cat_sample_data(obj, method = "unweighted", topic = "topic_1",
                          verbose = FALSE)

  # for one option
  out <- summary(samp, option = "option_1")
  expect_type(out, "list")
  expect_length(out, 1L)
  expect_named(out, "option_1")
  expect_named(out[["option_1"]], c("category", "Min", "Q1", "Median",
                                    "Mean", "Q3", "Max"))
  expect_identical(nrow(out[["option_1"]]), 5L)
  expect_identical(out[["option_1"]][["category"]],
                   c("category_1", "category_2", "category_3",
                     "category_4", "category_5"))

  # for all options
  out <- summary(samp)
  expect_type(out, "list")
  expect_length(out, 4L)
  expect_named(out, c("option_1", "option_2", "option_3", "option_4"))
  expect_named(out[["option_1"]], c("category", "Min", "Q1", "Median",
                                    "Mean", "Q3", "Max"))
  expect_named(out[["option_2"]], c("category", "Min", "Q1", "Median",
                                    "Mean", "Q3", "Max"))
  expect_named(out[["option_3"]], c("category", "Min", "Q1", "Median",
                                    "Mean", "Q3", "Max"))
  expect_named(out[["option_4"]], c("category", "Min", "Q1", "Median",
                                    "Mean", "Q3", "Max"))
  expect_identical(nrow(out[["option_1"]]), 5L)
  expect_identical(nrow(out[["option_2"]]), 5L)
  expect_identical(nrow(out[["option_3"]]), 5L)
  expect_identical(nrow(out[["option_4"]]), 5L)
  expect_identical(out[["option_1"]][["category"]],
                   c("category_1", "category_2", "category_3",
                     "category_4", "category_5"))
  expect_identical(out[["option_2"]][["category"]],
                   c("category_1", "category_2", "category_3",
                     "category_4", "category_5"))
  expect_identical(out[["option_3"]][["category"]],
                   c("category_1", "category_2", "category_3",
                     "category_4", "category_5"))
  expect_identical(out[["option_4"]][["category"]],
                   c("category_1", "category_2", "category_3",
                     "category_4", "category_5"))
})

test_that("With NA", {
  #with one expert giving NA
  obj <- create_cat_obj()
  obj[["data"]][["topic_1"]][1:5, 4:5] <- NA
  samp <- cat_sample_data(obj, method = "unweighted", topic = "topic_1",
                          verbose = FALSE)
  out <- summary(samp, option = "all")
  expect_type(out, "list")
  expect_length(out, 4L)
  expect_named(out, c("option_1", "option_2", "option_3", "option_4"))
  expect_named(out[["option_1"]], c("category", "Min", "Q1", "Median",
                                    "Mean", "Q3", "Max"))
  expect_named(out[["option_2"]], c("category", "Min", "Q1", "Median",
                                    "Mean", "Q3", "Max"))
  expect_named(out[["option_3"]], c("category", "Min", "Q1", "Median",
                                    "Mean", "Q3", "Max"))
  expect_named(out[["option_4"]], c("category", "Min", "Q1", "Median",
                                    "Mean", "Q3", "Max"))
  expect_identical(nrow(out[["option_1"]]), 5L)
  expect_identical(nrow(out[["option_2"]]), 5L)
  expect_identical(nrow(out[["option_3"]]), 5L)
  expect_identical(nrow(out[["option_4"]]), 5L)
  expect_identical(out[["option_1"]][["category"]],
                   c("category_1", "category_2", "category_3",
                     "category_4", "category_5"))
  expect_identical(out[["option_2"]][["category"]],
                   c("category_1", "category_2", "category_3",
                     "category_4", "category_5"))
  expect_identical(out[["option_3"]][["category"]],
                   c("category_1", "category_2", "category_3",
                     "category_4", "category_5"))
  expect_identical(out[["option_4"]][["category"]],
                   c("category_1", "category_2", "category_3",
                     "category_4", "category_5"))

  # with multiple experts giving NA
  obj[["data"]][["topic_1"]][21:25, 4:5] <- NA
  samp <- cat_sample_data(obj, method = "unweighted", topic = "topic_1",
                          verbose = FALSE)
  out <- summary(samp, option = "all")
  expect_type(out, "list")
  expect_length(out, 4L)
  expect_named(out, c("option_1", "option_2", "option_3", "option_4"))
  expect_named(out[["option_1"]], c("category", "Min", "Q1", "Median",
                                    "Mean", "Q3", "Max"))
  expect_named(out[["option_2"]], c("category", "Min", "Q1", "Median",
                                    "Mean", "Q3", "Max"))
  expect_named(out[["option_3"]], c("category", "Min", "Q1", "Median",
                                    "Mean", "Q3", "Max"))
  expect_named(out[["option_4"]], c("category", "Min", "Q1", "Median",
                                    "Mean", "Q3", "Max"))
  expect_identical(nrow(out[["option_1"]]), 5L)
  expect_identical(nrow(out[["option_2"]]), 5L)
  expect_identical(nrow(out[["option_3"]]), 5L)
  expect_identical(nrow(out[["option_4"]]), 5L)
  expect_identical(out[["option_1"]][["category"]],
                   c("category_1", "category_2", "category_3",
                     "category_4", "category_5"))
  expect_identical(out[["option_2"]][["category"]],
                   c("category_1", "category_2", "category_3",
                     "category_4", "category_5"))
  expect_identical(out[["option_3"]][["category"]],
                   c("category_1", "category_2", "category_3",
                     "category_4", "category_5"))
  expect_identical(out[["option_4"]][["category"]],
                   c("category_1", "category_2", "category_3",
                     "category_4", "category_5"))

  # with one option being selected
  out <- summary(samp, option = "option_1")
  expect_type(out, "list")
  expect_length(out, 1L)
  expect_named(out, "option_1")
  expect_named(out[["option_1"]], c("category", "Min", "Q1", "Median",
                                    "Mean", "Q3", "Max"))
  expect_identical(nrow(out[["option_1"]]), 5L)
  expect_identical(out[["option_1"]][["category"]],
                   c("category_1", "category_2", "category_3",
                     "category_4", "category_5"))
})
