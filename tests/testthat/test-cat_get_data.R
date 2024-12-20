test_that("Errors", {
  obj <- create_cat_obj()

  # When x is not an elic_cat object
  expect_snapshot(cat_get_data("abc", topic = "topic_1"),
                  error = TRUE)

  # When topic is not a character string
  expect_snapshot(cat_get_data(obj, topic = 1),
                  error = TRUE)

  # When topic is a character string of length > 1
  expect_snapshot(cat_get_data(obj,
                               topic = c("topic_1", "topic_2")),
                  error = TRUE)

  # When topic is not among the available topics
  expect_snapshot(cat_get_data(obj, topic = "topic_4"),
                  error = TRUE)

  # When option is not available in the object
  expect_snapshot(cat_get_data(obj, topic = "topic_1", option = "option_5"),
                  error = TRUE)

  # When option is not among the available options in the object
  expect_snapshot(cat_get_data(obj, topic = "topic_3", option = "option_4"),
                  error = TRUE)
})

test_that("Output", {
  obj <- create_cat_obj()
  # Get all variables
  expect_identical(cat_get_data(obj, topic = "topic_1"),
                   obj[["data"]][["topic_1"]])
  expect_identical(cat_get_data(obj, topic = "topic_2"),
                   obj[["data"]][["topic_2"]])
  expect_identical(cat_get_data(obj, topic = "topic_3"),
                   obj[["data"]][["topic_3"]])

  # Get only 1 option
  data <- obj[["data"]][["topic_1"]]
  expect_identical(cat_get_data(obj,
                                topic = "topic_1",
                                option = "option_1"),
                   data[data[["option"]] == "option_1", ])

  # Get multiple options
  data <- obj[["data"]][["topic_1"]]
  expect_identical(cat_get_data(obj,
                                topic = "topic_1",
                                option = c("option_1", "option_3")),
                   data[data[["option"]] %in% c("option_1", "option_3"), ])
})
