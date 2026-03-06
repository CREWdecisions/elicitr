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

test_that("Output", {
  obj <- create_cat_obj()

  # Modify one expert estimate to have 100% for category 1 in option 1
  obj[["data"]][["topic_1"]][1, 5] <- 1
  obj[["data"]][["topic_1"]][2:5, 5] <- 0

  # unweighted method
  out <- cat_sample_data(obj,
                         method = "unweighted",
                         topic = "topic_1",
                         verbose = FALSE)
  expect_identical(dplyr::pull(out, "category_1")[1:5], rep(1, 5))
  expect_identical(dplyr::pull(out, "category_2")[1:5], rep(0, 5))
  expect_identical(dplyr::pull(out, "category_3")[1:5], rep(0, 5))
  expect_identical(dplyr::pull(out, "category_4")[1:5], rep(0, 5))
  expect_identical(dplyr::pull(out, "category_5")[1:5], rep(0, 5))
  expect_identical(nrow(out), 2400L)
  expect_identical(as.vector(table(out[["id"]])), rep(400L, 6))

  # weighted method
  out <- cat_sample_data(obj,
                         method = "weighted",
                         topic = "topic_1",
                         option = "option_1",
                         verbose = FALSE)
  expect_identical(dplyr::pull(out, "category_1")[1:5], rep(1, 5))
  expect_identical(dplyr::pull(out, "category_2")[1:5], rep(0, 5))
  expect_identical(dplyr::pull(out, "category_3")[1:5], rep(0, 5))
  expect_identical(dplyr::pull(out, "category_4")[1:5], rep(0, 5))
  expect_identical(dplyr::pull(out, "category_5")[1:5], rep(0, 5))
  expect_identical(nrow(out), 600L)
  conf <- get_conf(obj[["data"]][["topic_1"]], "option_1", 5)
  experts <- unique(obj[["data"]][["topic_1"]][["id"]])
  n_samp_actual <- table(factor(out[["id"]], levels = unique(out[["id"]]))) |>
    as.vector()
  n_samp_expected <- get_boostrap_n_sample(experts, 100, conf) |>
    as.integer()
  expect_identical(n_samp_actual, n_samp_expected)
})

test_that("Accepts NAs", {
  obj <- create_cat_obj()

  # Modify one expert estimate to have NAs for all categories in option 1
  obj_na <- obj
  obj_na[["data"]][["topic_1"]][1:5, 4:5] <- NA

  # unweighted method
  out_na <- cat_sample_data(obj_na,
                         method = "unweighted",
                         topic = "topic_1",
                         verbose = FALSE)
  expect_true(all(is.na(dplyr::pull(out_na, "category_1")[1:100])))
  expect_true(all(is.na(dplyr::pull(out_na, "category_2")[1:100])))
  expect_true(all(is.na(dplyr::pull(out_na, "category_3")[1:100])))
  expect_true(all(is.na(dplyr::pull(out_na, "category_4")[1:100])))
  expect_true(all(is.na(dplyr::pull(out_na, "category_5")[1:100])))
  expect_identical(nrow(out_na), 2400L)
  expect_identical(as.vector(table(out_na[["id"]])), rep(400L, 6))

  # weighted method
  out_na <- cat_sample_data(obj_na,
                            method = "weighted",
                            topic = "topic_1",
                            option = "option_1",
                            verbose = FALSE)
  out <- cat_sample_data(obj,
                         method = "weighted",
                         topic = "topic_1",
                         option = "option_1",
                         verbose = FALSE)
  expect_true(all(is.na(dplyr::pull(out_na, "category_1")[1])))
  expect_true(all(is.na(dplyr::pull(out_na, "category_2")[1])))
  expect_true(all(is.na(dplyr::pull(out_na, "category_3")[1])))
  expect_true(all(is.na(dplyr::pull(out_na, "category_4")[1])))
  expect_true(all(is.na(dplyr::pull(out_na, "category_5")[1])))
  expect_identical(nrow(out_na), nrow(out) + 1L)
  conf <- elicitr:::get_conf(obj[["data"]][["topic_1"]], "option_1", 5)
  #NA distributed over the other experts
  conf_norm <- conf[2:6] / sum(conf[2:6])*conf[1]
  conf_tot <- conf[2:6] + conf_norm

  experts <- unique(obj[["data"]][["topic_1"]][["id"]])
  n_samp_actual <- table(factor(out_na[["id"]],
                                levels = unique(out_na[["id"]]))) |>
    as.vector()
  n_samp_expected <- elicitr:::get_boostrap_n_sample(experts, 100, conf_tot) |>
    as.integer()
  expect_identical(n_samp_actual[2:6], n_samp_expected)
})
