test_that("Errors", {
  obj <- create_cat_obj()

  # When x is not an elic_cat object
  expect_snapshot(cat_sample_data("abc",
                                  method = "unweighted",
                                  topic = "topic_1"),
                  error = TRUE)

  # When method is given as character vector of length > 1
  expect_snapshot(cat_sample_data(obj,
                                  method = c("unweighted", "weighted"),
                                  topic = "topic_1"),
                  error = TRUE)

  # When method is not a available
  expect_snapshot(cat_sample_data(obj,
                                  method = "new_method",
                                  topic = "topic_1"),
                  error = TRUE)
})

test_that("Info", {
  obj <- create_cat_obj()

  # unweighted method
  expect_snapshot(out <- cat_sample_data(obj,
                                         method = "unweighted",
                                         topic = "topic_1",
                                         option = c("option_1", "option_2"),
                                         n_votes = 50))
  expect_s3_class(out, class = "cat_sample")
  expect_identical(attr(out, "topic"), "topic_1")
  expect_identical(nrow(out), as.integer(obj[["experts"]] * 2 * 50))

  # weighted method
  expect_snapshot(out <- cat_sample_data(obj,
                                         method = "weighted",
                                         topic = "topic_1"))
  expect_s3_class(out, class = "cat_sample")
  expect_identical(attr(out, "topic"), "topic_1")
  res <- as.integer(obj[["experts"]] * length(obj[["options"]]) * 100)
  expect_identical(nrow(out), res)
})

test_that("output all", {
  obj <- create_cat_obj()

  # unweighted method
  out <- cat_sample_data(obj,
                         method = "unweighted",
                         topic = "topic_1",
                         verbose = FALSE)
  expect_named(out, c("id", "option", "category_1", "category_2",
                      "category_3", "category_4", "category_5"))
  expect_true(all(unique(out[["option"]]) == c("option_1", "option_2",
                                           "option_3", "option_4")))
  expect_false(any(out[["category_1"]] == 1))
  expect_false(any(out[["category_2"]] == 1))
  expect_false(any(out[["category_3"]] == 1))
  expect_false(any(out[["category_4"]] == 1))
  expect_false(any(out[["category_5"]] == 1))
  expect_false(anyNA(out[["category_1"]]))
  expect_false(anyNA(out[["category_2"]]))
  expect_false(anyNA(out[["category_3"]]))
  expect_false(anyNA(out[["category_4"]]))
  expect_false(anyNA(out[["category_5"]]))
  expect_identical(nrow(out), 2400L)
  experts <- unique(obj[["data"]][["topic_1"]][["id"]])
  expect_identical(as.double(table(factor(out[["id"]],
                                          levels = experts))),
                   as.double(rep(400L, 6)))

  # weighted method
  out <- cat_sample_data(obj,
                         method = "weighted",
                         topic = "topic_1",
                         verbose = FALSE)
  expect_named(out, c("id", "option", "category_1", "category_2",
                      "category_3", "category_4", "category_5"))
  expect_true(all(unique(out[["option"]]) == c("option_1", "option_2",
                                           "option_3", "option_4")))
  expect_false(any(out[["category_1"]] == 1))
  expect_false(any(out[["category_2"]] == 1))
  expect_false(any(out[["category_3"]] == 1))
  expect_false(any(out[["category_4"]] == 1))
  expect_false(any(out[["category_5"]] == 1))
  expect_false(anyNA(out[["category_1"]]))
  expect_false(anyNA(out[["category_2"]]))
  expect_false(anyNA(out[["category_3"]]))
  expect_false(anyNA(out[["category_4"]]))
  expect_false(anyNA(out[["category_5"]]))
  expect_identical(nrow(out), 2400L)
  experts <- unique(obj[["data"]][["topic_1"]][["id"]])
  n_samp_expected <- NULL
  n_samp_actual <- NULL
  for (i in unique(out[["option"]])) {
    # Determine n sample of each expert
    conf <- get_conf(obj[["data"]][["topic_1"]], i, 5)
    n_samp <- get_boostrap_n_sample(experts,
                                              n_votes = 100,
                                              weights = conf,
                                              elic_type = "weighted")
    n_samp_expected <- c(n_samp_expected, n_samp)
    option_i <- which(out[["option"]] == i)
    n_samp_actual <- c(n_samp_actual,
                       as.double(table(factor(out[["id"]][option_i],
                                              levels = experts))))
  }
  expect_identical(n_samp_actual,
                   n_samp_expected)
})

test_that("output 1 option", {
  obj <- create_cat_obj()

  # unweighted method
  out <- cat_sample_data(obj,
                         method = "unweighted",
                         topic = "topic_1",
                         option = "option_1",
                         verbose = FALSE)
  expect_named(out, c("id", "option", "category_1", "category_2",
                      "category_3", "category_4", "category_5"))
  expect_true(unique(out[["option"]]) == "option_1")
  expect_false(any(out[["category_1"]] == 1))
  expect_false(any(out[["category_2"]] == 1))
  expect_false(any(out[["category_3"]] == 1))
  expect_false(any(out[["category_4"]] == 1))
  expect_false(any(out[["category_5"]] == 1))
  expect_false(anyNA(out[["category_1"]]))
  expect_false(anyNA(out[["category_2"]]))
  expect_false(anyNA(out[["category_3"]]))
  expect_false(anyNA(out[["category_4"]]))
  expect_false(anyNA(out[["category_5"]]))
  expect_identical(nrow(out), 600L)
  experts <- unique(obj[["data"]][["topic_1"]][["id"]])
  expect_identical(as.double(table(factor(out[["id"]],
                                          levels = experts))),
                   as.double(rep(100L, 6)))

  # weighted method
  out <- cat_sample_data(obj,
                         method = "weighted",
                         topic = "topic_1",
                         option = "option_1",
                         verbose = FALSE)
  expect_named(out, c("id", "option", "category_1", "category_2",
                      "category_3", "category_4", "category_5"))
  expect_true(unique(out[["option"]]) == "option_1")
  expect_false(any(out[["category_1"]] == 1))
  expect_false(any(out[["category_2"]] == 1))
  expect_false(any(out[["category_3"]] == 1))
  expect_false(any(out[["category_4"]] == 1))
  expect_false(any(out[["category_5"]] == 1))
  expect_false(anyNA(out[["category_1"]]))
  expect_false(anyNA(out[["category_2"]]))
  expect_false(anyNA(out[["category_3"]]))
  expect_false(anyNA(out[["category_4"]]))
  expect_false(anyNA(out[["category_5"]]))
  expect_identical(nrow(out), 600L)
  experts <- unique(obj[["data"]][["topic_1"]][["id"]])
  n_samp_expected <- NULL
  n_samp_actual <- NULL
  for (i in unique(out[["option"]])) {
    # Determine n sample of each expert
    conf <- get_conf(obj[["data"]][["topic_1"]], i, 5)
    n_samp <- get_boostrap_n_sample(experts,
                                              n_votes = 100,
                                              weights = conf,
                                              elic_type = "weighted")
    n_samp_expected <- c(n_samp_expected, n_samp)
    option_i <- which(out[["option"]] == i)
    n_samp_actual <- c(n_samp_actual,
                       as.double(table(factor(out[["id"]][option_i],
                                              levels = experts))))
  }
  expect_identical(n_samp_actual,
                   n_samp_expected)
})

test_that("output multiple options", {
  obj <- create_cat_obj()

  # unweighted method
  out <- cat_sample_data(obj,
                         method = "unweighted",
                         topic = "topic_1",
                         option = c("option_1", "option_2"),
                         verbose = FALSE)
  expect_named(out, c("id", "option", "category_1", "category_2",
                      "category_3", "category_4", "category_5"))
  expect_true(all(unique(out[["option"]]) == c("option_1", "option_2")))
  expect_false(any(out[["category_1"]] == 1))
  expect_false(any(out[["category_2"]] == 1))
  expect_false(any(out[["category_3"]] == 1))
  expect_false(any(out[["category_4"]] == 1))
  expect_false(any(out[["category_5"]] == 1))
  expect_false(anyNA(out[["category_1"]]))
  expect_false(anyNA(out[["category_2"]]))
  expect_false(anyNA(out[["category_3"]]))
  expect_false(anyNA(out[["category_4"]]))
  expect_false(anyNA(out[["category_5"]]))
  expect_identical(nrow(out), 1200L)
  experts <- unique(obj[["data"]][["topic_1"]][["id"]])
  expect_identical(as.double(table(factor(out[["id"]],
                                          levels = experts))),
                   as.double(rep(200L, 6)))

  # weighted method
  out <- cat_sample_data(obj,
                         method = "weighted",
                         topic = "topic_1",
                         option = c("option_1", "option_2"),
                         verbose = FALSE)
  expect_named(out, c("id", "option", "category_1", "category_2",
                      "category_3", "category_4", "category_5"))
  expect_true(all(unique(out[["option"]]) == c("option_1", "option_2")))
  expect_false(any(out[["category_1"]] == 1))
  expect_false(any(out[["category_2"]] == 1))
  expect_false(any(out[["category_3"]] == 1))
  expect_false(any(out[["category_4"]] == 1))
  expect_false(any(out[["category_5"]] == 1))
  expect_false(anyNA(out[["category_1"]]))
  expect_false(anyNA(out[["category_2"]]))
  expect_false(anyNA(out[["category_3"]]))
  expect_false(anyNA(out[["category_4"]]))
  expect_false(anyNA(out[["category_5"]]))
  expect_identical(nrow(out), 1200L)
  experts <- unique(obj[["data"]][["topic_1"]][["id"]])
  n_samp_expected <- NULL
  n_samp_actual <- NULL
  for (i in unique(out[["option"]])) {
    # Determine n sample of each expert
    conf <- get_conf(obj[["data"]][["topic_1"]], i, 5)
    n_samp <- get_boostrap_n_sample(experts,
                                              n_votes = 100,
                                              weights = conf,
                                              elic_type = "weighted")
    n_samp_expected <- c(n_samp_expected, n_samp)
    option_i <- which(out[["option"]] == i)
    n_samp_actual <- c(n_samp_actual,
                       as.double(table(factor(out[["id"]][option_i],
                                              levels = experts))))
  }
  expect_identical(n_samp_actual,
                   n_samp_expected)
})

test_that("Accepts 1/0 estimates", {
  obj <- create_cat_obj()

  #One expert with estimate of 100 for one category for one option
  obj[["data"]][["topic_1"]][["estimate"]][1] <- 1
  obj[["data"]][["topic_1"]][["estimate"]][2:5] <- 0

  # unweighted method
  out <- cat_sample_data(obj,
                         method = "unweighted",
                         topic = "topic_1",
                         verbose = FALSE)
  option1_expert1 <- which(out[["option"]] == "option_1" &
                              out[["id"]] == unique(out[["id"]])[1])
  option1_expert26 <- which(out[["option"]] == "option_1" &
                              out[["id"]] %in% unique(out[["id"]])[-1])
  option1 <- which(out[["option"]] == "option_1")
  expect_named(out, c("id", "option", "category_1", "category_2",
                      "category_3", "category_4", "category_5"))
  expect_true(all(unique(out[["option"]]) == c("option_1", "option_2",
                                               "option_3", "option_4")))
  expect_true(all(out[["category_1"]][option1_expert1] == 1))
  expect_true(all(out[["category_2"]][option1_expert1] == 0))
  expect_true(all(out[["category_3"]][option1_expert1] == 0))
  expect_true(all(out[["category_4"]][option1_expert1] == 0))
  expect_true(all(out[["category_5"]][option1_expert1] == 0))
  expect_false(any(out[["category_1"]][option1_expert26] == 1))
  expect_false(any(out[["category_2"]][option1_expert26] == 1))
  expect_false(any(out[["category_3"]][option1_expert26] == 1))
  expect_false(any(out[["category_4"]][option1_expert26] == 1))
  expect_false(any(out[["category_5"]][option1_expert26] == 1))
  expect_false(anyNA(out[["category_1"]][option1_expert26]))
  expect_false(anyNA(out[["category_2"]][option1_expert26]))
  expect_false(anyNA(out[["category_3"]][option1_expert26]))
  expect_false(anyNA(out[["category_4"]][option1_expert26]))
  expect_false(anyNA(out[["category_5"]][option1_expert26]))
  experts <- unique(obj[["data"]][["topic_1"]][["id"]])
  expect_identical(as.double(table(factor(out[["id"]][option1],
                                          levels = experts))),
                   as.double(rep(100L, length(experts))))
  expect_false(any(out[["category_1"]][-option1] == 1))
  expect_false(any(out[["category_2"]][-option1] == 1))
  expect_false(any(out[["category_3"]][-option1] == 1))
  expect_false(any(out[["category_4"]][-option1] == 1))
  expect_false(any(out[["category_5"]][-option1] == 1))
  expect_false(anyNA(out[["category_1"]][-option1]))
  expect_false(anyNA(out[["category_2"]][-option1]))
  expect_false(anyNA(out[["category_3"]][-option1]))
  expect_false(anyNA(out[["category_4"]][-option1]))
  expect_false(anyNA(out[["category_5"]][-option1]))
  expect_identical(nrow(out), 2400L)
  expect_identical(as.double(table(factor(out[["id"]][-option1],
                                          levels = experts))),
                   as.double(rep(300L, length(experts))))

  # weighted method
  out <- cat_sample_data(obj,
                         method = "weighted",
                         topic = "topic_1",
                         verbose = FALSE)
  option1_expert1 <- which(out[["option"]] == "option_1" &
                             out[["id"]] == unique(out[["id"]])[1])
  option1_expert26 <- which(out[["option"]] == "option_1" &
                              out[["id"]] %in% unique(out[["id"]])[-1])
  option1 <- which(out[["option"]] == "option_1")
  expect_named(out, c("id", "option", "category_1", "category_2",
                      "category_3", "category_4", "category_5"))
  expect_true(all(unique(out[["option"]]) == c("option_1", "option_2",
                                               "option_3", "option_4")))
  expect_true(all(out[["category_1"]][option1_expert1] == 1))
  expect_true(all(out[["category_2"]][option1_expert1] == 0))
  expect_true(all(out[["category_3"]][option1_expert1] == 0))
  expect_true(all(out[["category_4"]][option1_expert1] == 0))
  expect_true(all(out[["category_5"]][option1_expert1] == 0))
  expect_false(any(out[["category_1"]][option1_expert26] == 1))
  expect_false(any(out[["category_2"]][option1_expert26] == 1))
  expect_false(any(out[["category_3"]][option1_expert26] == 1))
  expect_false(any(out[["category_4"]][option1_expert26] == 1))
  expect_false(any(out[["category_5"]][option1_expert26] == 1))
  expect_false(anyNA(out[["category_1"]][option1_expert26]))
  expect_false(anyNA(out[["category_2"]][option1_expert26]))
  expect_false(anyNA(out[["category_3"]][option1_expert26]))
  expect_false(anyNA(out[["category_4"]][option1_expert26]))
  expect_false(anyNA(out[["category_5"]][option1_expert26]))
  experts <- unique(obj[["data"]][["topic_1"]][["id"]])
  n_samp_expected <- NULL
  n_samp_actual <- NULL
  for (i in unique(out[["option"]])) {
    # Determine n sample of each expert
    conf <- get_conf(obj[["data"]][["topic_1"]], i, 5)
    n_samp <- get_boostrap_n_sample(experts,
                                              n_votes = 100,
                                              weights = conf,
                                              elic_type = "weighted")
    n_samp_expected <- c(n_samp_expected, n_samp)
    option_i <- which(out[["option"]] == i)
    n_samp_actual <- c(n_samp_actual,
                       as.double(table(factor(out[["id"]][option_i],
                                              levels = experts))))
  }
  expect_identical(n_samp_actual,
                   n_samp_expected)
  expect_false(any(out[["category_1"]][-option1] == 1))
  expect_false(any(out[["category_2"]][-option1] == 1))
  expect_false(any(out[["category_3"]][-option1] == 1))
  expect_false(any(out[["category_4"]][-option1] == 1))
  expect_false(any(out[["category_5"]][-option1] == 1))
  expect_false(anyNA(out[["category_1"]][-option1]))
  expect_false(anyNA(out[["category_2"]][-option1]))
  expect_false(anyNA(out[["category_3"]][-option1]))
  expect_false(anyNA(out[["category_4"]][-option1]))
  expect_false(anyNA(out[["category_5"]][-option1]))
  expect_identical(nrow(out), 2400L)
  n_samp_expected <- NULL
  n_samp_actual <- NULL
  for (i in unique(out[["option"]])) {
    # Determine n sample of each expert
    conf <- get_conf(obj[["data"]][["topic_1"]], i, 5)
    n_samp <- get_boostrap_n_sample(experts,
                                              n_votes = 100,
                                              weights = conf,
                                              elic_type = "weighted")
    n_samp_expected <- c(n_samp_expected, n_samp)
    option_i <- which(out[["option"]] == i)
    n_samp_actual <- c(n_samp_actual,
                       as.double(table(factor(out[["id"]][option_i],
                                              levels = experts))))
  }
  expect_identical(n_samp_actual,
                   n_samp_expected)
})

test_that("Accepts NAs from one expert", {
  obj <- create_cat_obj()

  # Modify one expert estimate to have NAs for all categories in option 1
  obj_na <- obj
  obj_na[["data"]][["topic_1"]][1:5, 4:5] <- NA

  # unweighted method
  out_na <- cat_sample_data(obj_na,
                            method = "unweighted",
                            topic = "topic_1",
                            verbose = FALSE)

  option1_expert1 <- which(out_na[["option"]] == "option_1" &
                             out_na[["id"]] == unique(out_na[["id"]])[1])
  option1_expert26 <- which(out_na[["option"]] == "option_1" &
                              out_na[["id"]] %in% unique(out_na[["id"]])[-1])
  option1 <- which(out_na[["option"]] == "option_1")
  expect_named(out_na, c("id", "option", "category_1", "category_2",
                         "category_3", "category_4", "category_5"))
  expect_true(all(unique(out_na[["option"]]) == c("option_1", "option_2",
                                                  "option_3", "option_4")))
  expect_true(all(is.na(out_na[["category_1"]][option1_expert1])))
  expect_true(all(is.na(out_na[["category_2"]][option1_expert1])))
  expect_true(all(is.na(out_na[["category_3"]][option1_expert1])))
  expect_true(all(is.na(out_na[["category_4"]][option1_expert1])))
  expect_true(all(is.na(out_na[["category_5"]][option1_expert1])))
  expect_false(anyNA(out_na[["category_1"]][option1_expert26]))
  expect_false(anyNA(out_na[["category_2"]][option1_expert26]))
  expect_false(anyNA(out_na[["category_3"]][option1_expert26]))
  expect_false(anyNA(out_na[["category_4"]][option1_expert26]))
  expect_false(anyNA(out_na[["category_5"]][option1_expert26]))
  experts <- unique(obj[["data"]][["topic_1"]][["id"]])
  expect_identical(as.double(table(factor(out_na[["id"]][option1],
                                          levels = experts))),
                   as.double(c(1, rep(100L, length(experts)-1))))
  expect_false(anyNA(out_na[["category_1"]][-option1]))
  expect_false(anyNA(out_na[["category_2"]][-option1]))
  expect_false(anyNA(out_na[["category_3"]][-option1]))
  expect_false(anyNA(out_na[["category_4"]][-option1]))
  expect_false(anyNA(out_na[["category_5"]][-option1]))
  expect_identical(nrow(out_na), 2301L)
  expect_identical(as.double(table(factor(out_na[["id"]][-option1],
                                          levels = experts))),
                   as.double(rep(300L, length(experts))))

  # weighted method
  out_na <- cat_sample_data(obj_na,
                            method = "weighted",
                            topic = "topic_1",
                            verbose = FALSE)
  option1_expert1 <- which(out_na[["option"]] == "option_1" &
                             out_na[["id"]] == unique(out_na[["id"]])[1])
  option1_expert26 <- which(out_na[["option"]] == "option_1" &
                              out_na[["id"]] %in% unique(out_na[["id"]])[-1])
  option1 <- which(out_na[["option"]] == "option_1")
  expect_named(out_na, c("id", "option", "category_1", "category_2",
                         "category_3", "category_4", "category_5"))
  expect_true(all(unique(out_na[["option"]]) == c("option_1", "option_2",
                                                  "option_3", "option_4")))
  expect_true(all(is.na(out_na[["category_1"]][option1_expert1])))
  expect_true(all(is.na(out_na[["category_2"]][option1_expert1])))
  expect_true(all(is.na(out_na[["category_3"]][option1_expert1])))
  expect_true(all(is.na(out_na[["category_4"]][option1_expert1])))
  expect_true(all(is.na(out_na[["category_5"]][option1_expert1])))
  expect_false(anyNA(out_na[["category_1"]][option1_expert26]))
  expect_false(anyNA(out_na[["category_2"]][option1_expert26]))
  expect_false(anyNA(out_na[["category_3"]][option1_expert26]))
  expect_false(anyNA(out_na[["category_4"]][option1_expert26]))
  expect_false(anyNA(out_na[["category_5"]][option1_expert26]))
  experts <- unique(obj[["data"]][["topic_1"]][["id"]])
  n_samp_expected <- NULL
  n_samp_actual <- NULL
  for (i in unique(out_na[["option"]])) {
    # Determine n sample of each expert
    conf <- elicitr:::get_conf(obj_na[["data"]][["topic_1"]], i, 5)
    n_samp <- elicitr:::get_boostrap_n_sample(experts,
                                    n_votes = 100,
                                    weights = conf,
                                    elic_type = "weighted")
    n_samp_expected <- c(n_samp_expected, n_samp)
    option_i <- which(out_na[["option"]] == i)
    n_samp_actual <- c(n_samp_actual,
                       as.double(table(factor(out_na[["id"]][option_i],
                                              levels = experts))))
  }
  expect_identical(n_samp_actual,
                   n_samp_expected)
  expect_false(anyNA(out_na[["category_1"]][-option1]))
  expect_false(anyNA(out_na[["category_2"]][-option1]))
  expect_false(anyNA(out_na[["category_3"]][-option1]))
  expect_false(anyNA(out_na[["category_4"]][-option1]))
  expect_false(anyNA(out_na[["category_5"]][-option1]))
  expect_identical(nrow(out_na), 2401L)
  n_samp_expected <- NULL
  n_samp_actual <- NULL
  for (i in unique(out_na[["option"]])) {
    # Determine n sample of each expert
    conf <- get_conf(obj_na[["data"]][["topic_1"]], i, 5)
    n_samp <- get_boostrap_n_sample(experts,
                                              n_votes = 100,
                                              weights = conf,
                                              elic_type = "weighted")
    n_samp_expected <- c(n_samp_expected, n_samp)
    option_i <- which(out_na[["option"]] == i)
    n_samp_actual <- c(n_samp_actual,
                       as.double(table(factor(out_na[["id"]][option_i],
                                              levels = experts))))
  }
  expect_identical(n_samp_actual,
                   n_samp_expected)
})

test_that("Accepts NAs from all experts for one option", {
  obj <- create_cat_obj()

  # Modify one option to have NAs for all categories and all experts
  obj_na <- obj
  position <- obj_na[["data"]][["topic_1"]]["option"] == "option_1"
  obj_na[["data"]][["topic_1"]][position, 4:5] <- NA

  # unweighted method
  out_na <- cat_sample_data(obj_na,
                            method = "unweighted",
                            topic = "topic_1",
                            verbose = FALSE)
  option1 <- which(out_na[["option"]] == "option_1")
  expect_named(out_na, c("id", "option", "category_1", "category_2",
                         "category_3", "category_4", "category_5"))
  expect_true(all(unique(out_na[["option"]]) == c("option_1", "option_2",
                                                  "option_3", "option_4")))
  expect_true(all(is.na(out_na[["category_1"]][option1])))
  expect_true(all(is.na(out_na[["category_2"]][option1])))
  expect_true(all(is.na(out_na[["category_3"]][option1])))
  expect_true(all(is.na(out_na[["category_4"]][option1])))
  expect_true(all(is.na(out_na[["category_5"]][option1])))
  expect_length(which(is.na(out_na[["category_1"]])), 6L)
  expect_length(which(is.na(out_na[["category_2"]])), 6L)
  expect_length(which(is.na(out_na[["category_3"]])), 6L)
  expect_length(which(is.na(out_na[["category_4"]])), 6L)
  expect_length(which(is.na(out_na[["category_5"]])), 6L)
  experts <- unique(obj[["data"]][["topic_1"]][["id"]])
  expect_identical(as.double(table(factor(out_na[["id"]][option1],
                                          levels = experts))),
                   as.double(rep(1L, length(experts))))
  expect_false(anyNA(out_na[["catgeory_1"]][-option1]))
  expect_false(anyNA(out_na[["catgeory_2"]][-option1]))
  expect_false(anyNA(out_na[["catgeory_3"]][-option1]))
  expect_false(anyNA(out_na[["catgeory_4"]][-option1]))
  expect_false(anyNA(out_na[["catgeory_5"]][-option1]))
  expect_identical(nrow(out_na), 1806L)
  expect_identical(as.double(table(factor(out_na[["id"]][-option1],
                                          levels = experts))),
                   as.double(rep(300L, length(experts))))

  # weighted method
  out_na <- cat_sample_data(obj_na,
                            method = "weighted",
                            topic = "topic_1",
                            verbose = FALSE)
  option1 <- which(out_na[["option"]] == "option_1")
  expect_named(out_na, c("id", "option", "category_1", "category_2",
                         "category_3", "category_4", "category_5"))
  expect_true(all(unique(out_na[["option"]]) == c("option_1", "option_2",
                                                  "option_3", "option_4")))
  expect_true(all(is.na(out_na[["category_1"]][option1])))
  expect_true(all(is.na(out_na[["category_2"]][option1])))
  expect_true(all(is.na(out_na[["category_3"]][option1])))
  expect_true(all(is.na(out_na[["category_4"]][option1])))
  expect_true(all(is.na(out_na[["category_5"]][option1])))
  expect_length(which(is.na(out_na[["category_1"]])), 6L)
  expect_length(which(is.na(out_na[["category_2"]])), 6L)
  expect_length(which(is.na(out_na[["category_3"]])), 6L)
  expect_length(which(is.na(out_na[["category_4"]])), 6L)
  expect_length(which(is.na(out_na[["category_5"]])), 6L)
  experts <- unique(obj[["data"]][["topic_1"]][["id"]])
  n_samp_expected <- NULL
  n_samp_actual <- NULL
  for (i in unique(out_na[["option"]])) {
    # Determine n sample of each expert
    conf <- get_conf(obj_na[["data"]][["topic_1"]], i, 5)
    n_samp <- get_boostrap_n_sample(experts,
                                              n_votes = 100,
                                              weights = conf,
                                              elic_type = "weighted")
    n_samp_expected <- c(n_samp_expected, n_samp)
    option_i <- which(out_na[["option"]] == i)
    n_samp_actual <- c(n_samp_actual,
                       as.double(table(factor(out_na[["id"]][option_i],
                                              levels = experts))))
  }
  expect_identical(n_samp_actual,
                   n_samp_expected)
  expect_false(anyNA(out_na[["catgeory_1"]][-option1]))
  expect_false(anyNA(out_na[["catgeory_2"]][-option1]))
  expect_false(anyNA(out_na[["catgeory_3"]][-option1]))
  expect_false(anyNA(out_na[["catgeory_4"]][-option1]))
  expect_false(anyNA(out_na[["catgeory_5"]][-option1]))
  expect_identical(nrow(out_na), 1806L)
  n_samp_expected <- NULL
  n_samp_actual <- NULL
  for (i in unique(out_na[["option"]])) {
    # Determine n sample of each expert
    conf <- get_conf(obj_na[["data"]][["topic_1"]], i, 5)
    n_samp <- get_boostrap_n_sample(experts,
                                              n_votes = 100,
                                              weights = conf,
                                              elic_type = "weighted")
    n_samp_expected <- c(n_samp_expected, n_samp)
    option_i <- which(out_na[["option"]] == i)
    n_samp_actual <- c(n_samp_actual,
                       as.double(table(factor(out_na[["id"]][option_i],
                                              levels = experts))))
  }
  expect_identical(n_samp_actual,
                   n_samp_expected)
})
