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
})
