test_that("Errors", {
  obj <- create_cat_obj()

  # When x is not an elic_cat object
  expect_snapshot(elic_cat_get_data("abc",
                                    mechanism = "mechanism_1"),
                  error = TRUE)

  # When mechanism is not a character string
  expect_snapshot(elic_cat_get_data(obj,
                                    mechanism = 1),
                  error = TRUE)

  # When mechanism is a character string of length > 1
  expect_snapshot(elic_cat_get_data(obj,
                                    mechanism = c("mechanism_1",
                                                  "mechanism_2")),
                  error = TRUE)

  # When mechanism is not among the available mechanisms
  expect_snapshot(elic_cat_get_data(obj,
                                    mechanism = "mechanism_4"),
                  error = TRUE)

  # When site is not available in the object
  expect_snapshot(elic_cat_get_data(obj,
                                    mechanism = "mechanism_1",
                                    site = "site_5"),
                  error = TRUE)

  # When site is not among the available sites in the object
  expect_snapshot(elic_cat_get_data(obj,
                                    mechanism = "mechanism_3",
                                    site = "site_4"),
                  error = TRUE)
})

test_that("Output", {
  obj <- create_cat_obj()
  # Get all variables
  expect_identical(elic_cat_get_data(obj, mechanism = "mechanism_1"),
                   obj[["data"]][["mechanism_1"]])
  expect_identical(elic_cat_get_data(obj, mechanism = "mechanism_2"),
                   obj[["data"]][["mechanism_2"]])
  expect_identical(elic_cat_get_data(obj, mechanism = "mechanism_3"),
                   obj[["data"]][["mechanism_3"]])

  # Get only 1 site
  data <- obj[["data"]][["mechanism_1"]]
  expect_identical(elic_cat_get_data(obj,
                                     mechanism = "mechanism_1",
                                     site = "site_1"),
                   data[data[["site"]] == "site_1", ])

  # Get multiple sites
  data <- obj[["data"]][["mechanism_1"]]
  expect_identical(elic_cat_get_data(obj,
                                     mechanism = "mechanism_1",
                                     site = c("site_1", "site_3")),
                   data[data[["site"]] %in% c("site_1", "site_3"), ])
})
