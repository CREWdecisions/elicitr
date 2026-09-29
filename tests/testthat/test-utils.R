test_that("split_short_codes() works", {
  # For variable types
  x <- split_short_codes("abc")
  expect_length(x, 3)
  expect_type(x, "character")
  expect_identical(x, c("a", "b", "c"))
  # For elicitation types (the character "p" is added to the short codes)
  x <- split_short_codes("134", add_p = TRUE)
  expect_length(x, 3)
  expect_type(x, "character")
  expect_identical(x, c("1p", "3p", "4p"))
})

test_that("get_boostrap_n_sample() rejects negative weights", {
  #negative weights
  weights <- c(-1, 3)
  expect_snapshot(get_boostrap_n_sample(experts = c("A", "B"),
                                        n_votes = 10,
                                        weights = weights),
                  error = TRUE)
})
