test_that("Errors", {
  obj <- create_elicit_obj()
  # When x is not an elicit object
  expect_snapshot(elic_plot("abc", round = 1, var = "var1"),
                  error = TRUE)
  # When round is not 1 or 2
  expect_snapshot(elic_plot(obj, round = 3, var = "var1"),
                  error = TRUE)
  # When var is not a variable in the elicit object
  expect_snapshot(elic_plot(obj, round = 1, var = "var5"),
                  error = TRUE)
  # When var is a character vector of length > 1
  expect_snapshot(elic_plot(obj, round = 1, var = c("var1", "var5", "var7")),
                  error = TRUE)
})

test_that("Warnings", {
  obj <- create_elicit_obj()
  # When rescaled values are not within the limits
  expect_snapshot(p <- elic_plot(obj, round = 1, var = "var3", verbose = FALSE))
})

test_that("Info", {
  obj <- create_elicit_obj()
  # When values are rescaled
  expect_snapshot(p <- elic_plot(obj, round = 2, var = "var3"))
})

test_that("Output", {
  obj <- create_elicit_obj()
  # 1p----
  # Plot for a variable with 1 point elicitation
  p <- elic_plot(obj, round = 2, var = "var1", verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p$layers, 1)
  expect_identical(class(p$layers[[1]]$geom)[1], "GeomPoint")
  expect_identical(ncol(p$data), 3L)
  expect_identical(colnames(p$data), c("id", "best", "col"))
  expect_identical(unique(p$data$col), "experts")
  expect_true(is.factor(p$data$id))
  expect_identical(levels(p$data$id), obj$data$round_2$id)
  expect_identical(p$data$best, obj$data$round_2$var1_best)

  # Plot for a variable with 1 point elicitation and group
  p <- elic_plot(obj, round = 2, var = "var1", group = TRUE, verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p$layers, 1)
  expect_identical(class(p$layers[[1]]$geom)[1], "GeomPoint")
  expect_identical(ncol(p$data), 3L)
  expect_identical(colnames(p$data), c("id", "best", "col"))
  expect_identical(unique(p$data$col), c("experts", "group"))
  expect_true(is.factor(p$data$id))
  expect_identical(levels(p$data$id), c(obj$data$round_2$id, "Group"))
  expect_identical(p$data$best[7],
                   mean(obj$data$round_2$var1_best, na.rm = TRUE))

  # Plot for a variable with 1 point elicitation and truth
  p <- elic_plot(obj, round = 2, var = "var1", truth = list(best = 0.8),
                 verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p$layers, 1)
  expect_identical(class(p$layers[[1]]$geom)[1], "GeomPoint")
  expect_identical(ncol(p$data), 3L)
  expect_identical(colnames(p$data), c("id", "best", "col"))
  expect_identical(unique(p$data$col), c("experts", "truth"))
  expect_true(is.factor(p$data$id))
  expect_identical(levels(p$data$id), c(obj$data$round_2$id, "Truth"))
  expect_identical(p$data$best[7], 0.8)

  # Plot for a variable with 1 point elicitation, group and truth
  p <- elic_plot(obj, round = 2, var = "var1", group = TRUE,
                 truth = list(best = 0.8), verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p$layers, 1)
  expect_identical(class(p$layers[[1]]$geom)[1], "GeomPoint")
  expect_identical(ncol(p$data), 3L)
  expect_identical(colnames(p$data), c("id", "best", "col"))
  expect_identical(unique(p$data$col), c("experts", "group", "truth"))
  expect_true(is.factor(p$data$id))
  expect_identical(levels(p$data$id), c(obj$data$round_2$id, "Group", "Truth"))
  expect_identical(p$data$best[7],
                   mean(obj$data$round_2$var1_best, na.rm = TRUE))
  expect_identical(p$data$best[8], 0.8)

  # 3p----
  # Plot for a variable with 3 points elicitation
  p <- elic_plot(obj, round = 2, var = "var2", verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p$layers, 2)
  expect_identical(class(p$layers[[1]]$geom)[1], "GeomPoint")
  expect_identical(class(p$layers[[2]]$geom)[1], "GeomErrorbarh")
  expect_identical(ncol(p$data), 5L)
  expect_identical(colnames(p$data), c("id", "min", "max", "best", "col"))
  expect_identical(unique(p$data$col), "experts")
  expect_true(is.factor(p$data$id))
  expect_identical(levels(p$data$id), obj$data$round_2$id)
  for (i in seq_len(3)) {
    expect_identical(dplyr::pull(p$data[, i + 1]),
                     dplyr::pull(obj$data$round_2[, i + 2]))
  }

  # Plot for a variable with 3 points elicitation and group
  p <- elic_plot(obj, round = 2, var = "var2", group = TRUE, verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p$layers, 2)
  expect_identical(class(p$layers[[1]]$geom)[1], "GeomPoint")
  expect_identical(class(p$layers[[2]]$geom)[1], "GeomErrorbarh")
  expect_identical(ncol(p$data), 5L)
  expect_identical(colnames(p$data), c("id", "min", "max", "best", "col"))
  expect_identical(unique(p$data$col), c("experts", "group"))
  expect_true(is.factor(p$data$id))
  expect_identical(levels(p$data$id), c(obj$data$round_2$id, "Group"))
  for (i in seq_len(3)) {
    # The mean function converts the column to double
    expect_equal(dplyr::pull(p$data[-7, i + 1]),
                 dplyr::pull(obj$data$round_2[-7, i + 2]))
    expect_identical(dplyr::pull(p$data[7, i + 1]),
                     mean(dplyr::pull(obj$data$round_2[, i + 2]), na.rm = TRUE))
  }

  # Plot for a variable with 3 points elicitation and truth
  truth_data <- list(min = 0.7, max = 0.9, best = 0.8)
  p <- elic_plot(obj, round = 2, var = "var2",
                 truth = truth_data,
                 verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p$layers, 2)
  expect_identical(class(p$layers[[1]]$geom)[1], "GeomPoint")
  expect_identical(class(p$layers[[2]]$geom)[1], "GeomErrorbarh")
  expect_identical(ncol(p$data), 5L)
  expect_identical(colnames(p$data), c("id", "min", "max", "best", "col"))
  expect_identical(unique(p$data$col), c("experts", "truth"))
  expect_true(is.factor(p$data$id))
  expect_identical(levels(p$data$id), c(obj$data$round_2$id, "Truth"))
  for (i in seq_len(3)) {
    # The mean function converts the column to double
    expect_equal(dplyr::pull(p$data[-7, i + 1]),
                 dplyr::pull(obj$data$round_2[-7, i + 2]))
    expect_identical(dplyr::pull(p$data[7, i + 1]),
                     truth_data[[i]])
  }

  # Plot for a variable with 3 points elicitation, group and truth
  p <- elic_plot(obj, round = 2, var = "var2",
                 truth = truth_data,
                 group = TRUE, verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p$layers, 2)
  expect_identical(class(p$layers[[1]]$geom)[1], "GeomPoint")
  expect_identical(class(p$layers[[2]]$geom)[1], "GeomErrorbarh")
  expect_identical(ncol(p$data), 5L)
  expect_identical(colnames(p$data), c("id", "min", "max", "best", "col"))
  expect_identical(unique(p$data$col), c("experts", "group", "truth"))
  expect_true(is.factor(p$data$id))
  expect_identical(levels(p$data$id), c(obj$data$round_2$id, "Group", "Truth"))
  for (i in seq_len(3)) {
    # The mean function converts the column to double
    expect_equal(dplyr::pull(p$data[1:6, i + 1]),
                 dplyr::pull(obj$data$round_2[1:6, i + 2]))
    expect_identical(dplyr::pull(p$data[7, i + 1]),
                     mean(dplyr::pull(obj$data$round_2[, i + 2]), na.rm = TRUE))
    expect_identical(dplyr::pull(p$data[8, i + 1]),
                     truth_data[[i]])
  }

  # 4p----
  # Plot for a variable with 4 points elicitation
  p <- elic_plot(obj, round = 2, var = "var3", scale_conf = 90, verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p$layers, 2)
  expect_identical(class(p$layers[[1]]$geom)[1], "GeomPoint")
  expect_identical(class(p$layers[[2]]$geom)[1], "GeomErrorbarh")
  expect_identical(ncol(p$data), 6L)
  expect_identical(colnames(p$data),
                   c("id", "min", "max", "best", "conf", "col"))
  expect_identical(unique(p$data$col), "experts")
  expect_true(is.factor(p$data$id))
  expect_identical(levels(p$data$id), obj$data$round_2$id)
  # Here data are rescaled
  rescaled_data <- obj$data$round_2[, 6:9] |>
    stats::setNames(c("min", "max", "best", "conf")) |>
    rescale_data(scale_conf = 90)
  for (i in seq_len(4)) {
    expect_identical(dplyr::pull(p$data[, i + 1]),
                     dplyr::pull(rescaled_data[, i]))
  }

  # Plot for a variable with 4 points elicitation and group
  p <- elic_plot(obj, round = 2, var = "var3", scale_conf = 90,
                 group = TRUE, verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p$layers, 2)
  expect_identical(class(p$layers[[1]]$geom)[1], "GeomPoint")
  expect_identical(class(p$layers[[2]]$geom)[1], "GeomErrorbarh")
  expect_identical(ncol(p$data), 6L)
  expect_identical(colnames(p$data),
                   c("id", "min", "max", "best", "conf", "col"))
  expect_identical(unique(p$data$col), c("experts", "group"))
  expect_true(is.factor(p$data$id))
  expect_identical(levels(p$data$id), c(obj$data$round_2$id, "Group"))
  for (i in seq_len(4)) {
    expect_equal(dplyr::pull(p$data[-7, i + 1]),
                 dplyr::pull(rescaled_data[, i]))
  }
  for (i in seq_len(3)) {
    expect_equal(dplyr::pull(p$data[7, i + 1]),
                 mean(dplyr::pull(rescaled_data[, i]), na.rm = TRUE))
  }

  # Plot for a variable with 4 points elicitation and truth
  truth_data <- list(min = 0.7, max = 0.9, best = 0.8, conf = 100)
  truth_data_rescaled <- rescale_data(truth_data, scale_conf = 90)
  p <- elic_plot(obj, round = 2, var = "var3", scale_conf = 90,
                 truth = truth_data,
                 verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p$layers, 2)
  expect_identical(class(p$layers[[1]]$geom)[1], "GeomPoint")
  expect_identical(class(p$layers[[2]]$geom)[1], "GeomErrorbarh")
  expect_identical(ncol(p$data), 6L)
  expect_identical(colnames(p$data),
                   c("id", "min", "max", "best", "conf", "col"))
  expect_identical(unique(p$data$col), c("experts", "truth"))
  expect_true(is.factor(p$data$id))
  expect_identical(levels(p$data$id), c(obj$data$round_2$id, "Truth"))
    for (i in seq_len(4)) {
    # The mean function converts the column to double
    expect_equal(dplyr::pull(p$data[-7, i + 1]),
                 dplyr::pull(rescaled_data[, i]))
    expect_identical(dplyr::pull(p$data[7, i + 1]),
                     truth_data_rescaled[[i]])
  }

  # Plot for a variable with 4 points elicitation, group and truth
  p <- elic_plot(obj, round = 2, var = "var3", scale_conf = 90,
                 truth = list(min = 0.7, max = 0.9, best = 0.8, conf = 100),
                 group = TRUE, verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p$layers, 2)
  expect_identical(class(p$layers[[1]]$geom)[1], "GeomPoint")
  expect_identical(class(p$layers[[2]]$geom)[1], "GeomErrorbarh")
  expect_identical(ncol(p$data), 6L)
  expect_identical(colnames(p$data),
                   c("id", "min", "max", "best", "conf", "col"))
  expect_identical(unique(p$data$col), c("experts", "group", "truth"))
  expect_true(is.factor(p$data$id))
  expect_identical(levels(p$data$id), c(obj$data$round_2$id, "Group", "Truth"))
  for (i in seq_len(4)) {
    # The mean function converts the column to double
    expect_equal(dplyr::pull(p$data[1:6, i + 1]),
                 dplyr::pull(rescaled_data[, i]))
    expect_identical(dplyr::pull(p$data[8, i + 1]),
                     truth_data_rescaled[[i]])
  }
  for (i in seq_len(3)) {
    expect_equal(dplyr::pull(p$data[7, i + 1]),
                 mean(dplyr::pull(rescaled_data[, i]), na.rm = TRUE))
  }

  # Colours and shapes----
  p <- elic_plot(obj, round = 2, var = "var3",
                 truth = list(min = 0.7, max = 0.9, best = 0.8, conf = 100),
                 group = TRUE,
                 colour = "yellow",
                 group_colour = "brown",
                 truth_colour = "pink",
                 point_size = 3,
                 line_width = 2,
                 verbose = FALSE)
  ld1 <- ggplot2::layer_data(p, i = 1L)
  n <- obj$experts
  expect_identical(ld1$colour[seq_len(n)], rep("yellow", n))
  expect_identical(ld1$colour[n + 1], "brown")
  expect_identical(ld1$colour[n + 2], "pink")
  expect_identical(ld1$size, rep(3, n + 2))
  ld2 <- ggplot2::layer_data(p, i = 2L)
  expect_identical(ld2$colour[seq_len(n)], rep("yellow", n))
  expect_identical(ld2$colour[n + 1], "brown")
  expect_identical(ld2$colour[n + 2], "pink")
  expect_identical(ld2$linewidth, rep(2, n + 2))
})

test_that("get_type()", {
  obj <- create_elicit_obj()
  expect_identical(get_type(obj, "var1", "var"), "Z")
  expect_identical(get_type(obj, "var2", "var"), "N")
  expect_identical(get_type(obj, "var3", "var"), "p")
  expect_identical(get_type(obj, "var1", "elic"), "1p")
  expect_identical(get_type(obj, "var2", "elic"), "3p")
  expect_identical(get_type(obj, "var3", "elic"), "4p")
})
