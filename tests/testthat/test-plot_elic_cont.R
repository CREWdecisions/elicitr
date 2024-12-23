test_that("Errors", {
  obj <- create_cont_obj()
  # When round is not 1 or 2
  expect_snapshot(plot(obj, round = 3, var = "var1"),
                  error = TRUE)
  # When var is not a variable in the elicit object
  expect_snapshot(plot(obj, round = 1, var = "var5"),
                  error = TRUE)
  # When var is a character vector of length > 1
  expect_snapshot(plot(obj, round = 1, var = c("var1", "var5", "var7")),
                  error = TRUE)
  # Truth for 1p----
  # When truth is not a list
  expect_snapshot(plot(obj, round = 1, var = "var1", truth = 0.8),
                  error = TRUE)
  # When truth is a list with wrong elements
  expect_snapshot(plot(obj, round = 1, var = "var1",
                       truth = list(min = 0.7, max = 0.9)),
                  error = TRUE)
  # When truth is a list with the right amount of elements but wrong name
  expect_snapshot(plot(obj, round = 1, var = "var1", truth = list(beast = 0.8)),
                  error = TRUE)
  # Truth for 3p----
  # When truth is a list with wrong elements
  expect_snapshot(plot(obj, round = 2, var = "var2",
                       truth = list(min = 0.7, max = 0.9)),
                  error = TRUE)
  # When truth is a list with the right amount of elements but wrong names
  expect_snapshot(plot(obj, round = 2, var = "var2",
                       truth = list(min = 0.7, beast = 0.8, max = 0.9)),
                  error = TRUE)
  # Truth for 4p----
  # When truth is a list with wrong elements
  expect_snapshot(plot(obj, round = 2, var = "var3",
                       truth = list(min = 0.7, max = 0.9)),
                  error = TRUE)
  # When truth is a list with the right amount of elements but wrong names
  expect_snapshot(plot(obj, round = 2, var = "var3",
                       truth = list(min = 0.7, beast = 0.8,
                                    max = 0.9, conf = 100)),
                  error = TRUE)
})

test_that("Warnings", {
  obj <- create_cont_obj()
  # When rescaled values are not within the limits
  expect_snapshot(p <- plot(obj, round = 1, var = "var3", verbose = FALSE))
})

test_that("Info", {
  obj <- create_cont_obj()
  # When values are rescaled
  expect_snapshot(p <- plot(obj, round = 2, var = "var3"))
})

test_that("Output", {
  obj <- create_cont_obj()
  # 1p----
  # Plot for a variable with 1 point elicitation
  p <- plot(obj, round = 2, var = "var1", verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p[["layers"]], 1)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[1]], "GeomPoint")
  expect_identical(ncol(p[["data"]]), 3L)
  expect_identical(colnames(p[["data"]]), c("id", "best", "col"))
  expect_identical(unique(p[["data"]][["col"]]), "experts")
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]),
                   obj[["data"]][["round_2"]][["id"]])
  expect_identical(p[["data"]][["best"]],
                   obj[["data"]][["round_2"]][["var1_best"]])

  # Plot for a variable with 1 point elicitation and group
  p <- plot(obj, round = 2, var = "var1", group = TRUE, verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p[["layers"]], 1)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[1]], "GeomPoint")
  expect_identical(ncol(p[["data"]]), 3L)
  expect_identical(colnames(p[["data"]]), c("id", "best", "col"))
  expect_identical(unique(p[["data"]][["col"]]), c("experts", "group"))
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]),
                   c(obj[["data"]][["round_2"]][["id"]], "Group"))
  expect_identical(p[["data"]][["best"]][[7]],
                   mean(obj[["data"]][["round_2"]][["var1_best"]],
                        na.rm = TRUE))

  # Plot for a variable with 1 point elicitation and truth
  p <- plot(obj, round = 2, var = "var1", truth = list(best = 0.8),
            verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p[["layers"]], 1)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[1]], "GeomPoint")
  expect_identical(ncol(p[["data"]]), 3L)
  expect_identical(colnames(p[["data"]]), c("id", "best", "col"))
  expect_identical(unique(p[["data"]][["col"]]), c("experts", "truth"))
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]),
                   c(obj[["data"]][["round_2"]][["id"]], "Truth"))
  expect_identical(p[["data"]][["best"]][[7]], 0.8)

  # Plot for a variable with 1 point elicitation, group and truth
  p <- plot(obj, round = 2, var = "var1", group = TRUE,
            truth = list(best = 0.8), verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p[["layers"]], 1)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[1]], "GeomPoint")
  expect_identical(ncol(p[["data"]]), 3L)
  expect_identical(colnames(p[["data"]]), c("id", "best", "col"))
  expect_identical(unique(p[["data"]][["col"]]), c("experts", "group", "truth"))
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]),
                   c(obj[["data"]][["round_2"]][["id"]], "Group", "Truth"))
  expect_identical(p[["data"]][["best"]][[7]],
                   mean(obj[["data"]][["round_2"]][["var1_best"]],
                        na.rm = TRUE))
  expect_identical(p[["data"]][["best"]][[8]], 0.8)

  # 3p----
  # Plot for a variable with 3 points elicitation
  p <- plot(obj, round = 2, var = "var2", verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p[["layers"]], 2)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[1]], "GeomPoint")
  expect_identical(class(p[["layers"]][[2]][["geom"]])[[1]], "GeomErrorbarh")
  expect_identical(ncol(p[["data"]]), 5L)
  expect_identical(colnames(p[["data"]]), c("id", "min", "max", "best", "col"))
  expect_identical(unique(p[["data"]][["col"]]), "experts")
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]),
                   obj[["data"]][["round_2"]][["id"]])
  expect_identical(p[["data"]][, 2:4],
                   obj[["data"]][["round_2"]][, 3:5],
                   ignore_attr = TRUE)

  # Plot for a variable with 3 points elicitation and group
  p <- plot(obj, round = 2, var = "var2", group = TRUE, verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p[["layers"]], 2)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[1]], "GeomPoint")
  expect_identical(class(p[["layers"]][[2]][["geom"]])[[1]], "GeomErrorbarh")
  expect_identical(ncol(p[["data"]]), 5L)
  expect_identical(colnames(p[["data"]]), c("id", "min", "max", "best", "col"))
  expect_identical(unique(p[["data"]][["col"]]), c("experts", "group"))
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]),
                   c(obj[["data"]][["round_2"]][["id"]], "Group"))
  # The mean function converts the column to double
  expect_equal(p[["data"]][-7, 2:4],
               obj[["data"]][["round_2"]][-7, 3:5],
               ignore_attr = TRUE)
  expect_equal(as.numeric(p[["data"]][7, 2:4]),
               colMeans(obj[["data"]][["round_2"]][, 3:5], na.rm = TRUE),
               ignore_attr = TRUE)

  # Plot for a variable with 3 points elicitation and truth
  truth_data <- list(min = 0.7, max = 0.9, best = 0.8)
  p <- plot(obj, round = 2, var = "var2", truth = truth_data, verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p[["layers"]], 2)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[1]], "GeomPoint")
  expect_identical(class(p[["layers"]][[2]][["geom"]])[[1]], "GeomErrorbarh")
  expect_identical(ncol(p[["data"]]), 5L)
  expect_identical(colnames(p[["data"]]), c("id", "min", "max", "best", "col"))
  expect_identical(unique(p[["data"]][["col"]]), c("experts", "truth"))
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]),
                   c(obj[["data"]][["round_2"]][["id"]], "Truth"))
  # The mean function converts the column to double
  expect_equal(p[["data"]][-7, 2:4],
               obj[["data"]][["round_2"]][-7, 3:5],
               ignore_attr = TRUE)
  expect_identical(p[["data"]][7, 2:4],
                   truth_data[1:3],
                   ignore_attr = TRUE)

  # Plot for a variable with 3 points elicitation, group and truth
  p <- plot(obj, round = 2, var = "var2", truth = truth_data,
            group = TRUE, verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p[["layers"]], 2)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[1]], "GeomPoint")
  expect_identical(class(p[["layers"]][[2]][["geom"]])[[1]], "GeomErrorbarh")
  expect_identical(ncol(p[["data"]]), 5L)
  expect_identical(colnames(p[["data"]]), c("id", "min", "max", "best", "col"))
  expect_identical(unique(p[["data"]][["col"]]), c("experts", "group", "truth"))
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]),
                   c(obj[["data"]][["round_2"]][["id"]], "Group", "Truth"))
  # The mean function converts the column to double
  expect_equal(p[["data"]][1:6, 2:4],
               obj[["data"]][["round_2"]][1:6, 3:5],
               ignore_attr = TRUE)
  expect_equal(as.numeric(p[["data"]][7, 2:4]),
               colMeans(obj[["data"]][["round_2"]][, 3:5], na.rm = TRUE),
               ignore_attr = TRUE)
  expect_identical(p[["data"]][8, 2:4],
                   truth_data[1:3],
                   ignore_attr = TRUE)

  # 4p----
  # Plot for a variable with 4 points elicitation
  p <- plot(obj, round = 2, var = "var3", scale_conf = 90, verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p[["layers"]], 2)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[1]], "GeomPoint")
  expect_identical(class(p[["layers"]][[2]][["geom"]])[[1]], "GeomErrorbarh")
  expect_identical(ncol(p[["data"]]), 6L)
  expect_identical(colnames(p[["data"]]),
                   c("id", "min", "max", "best", "conf", "col"))
  expect_identical(unique(p[["data"]][["col"]]), "experts")
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]),
                   obj[["data"]][["round_2"]][["id"]])
  # Here data are rescaled
  rescaled_data <- obj[["data"]][["round_2"]][, 6:9] |>
    stats::setNames(c("min", "max", "best", "conf")) |>
    rescale_data(s = 90)
  expect_identical(p[["data"]][, 2:5],
                   rescaled_data,
                   ignore_attr = TRUE)

  # Plot for a variable with 4 points elicitation and group
  p <- plot(obj, round = 2, var = "var3", scale_conf = 90,
            group = TRUE, verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p[["layers"]], 2)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[1]], "GeomPoint")
  expect_identical(class(p[["layers"]][[2]][["geom"]])[[1]], "GeomErrorbarh")
  expect_identical(ncol(p[["data"]]), 6L)
  expect_identical(colnames(p[["data"]]),
                   c("id", "min", "max", "best", "conf", "col"))
  expect_identical(unique(p[["data"]][["col"]]), c("experts", "group"))
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]),
                   c(obj[["data"]][["round_2"]][["id"]], "Group"))
  # The mean function converts the column to double
  expect_equal(p[["data"]][-7, 2:5],
               rescaled_data,
               ignore_attr = TRUE)
  expect_equal(as.numeric(p[["data"]][7, 2:4]),
               colMeans(rescaled_data[, 1:3], na.rm = TRUE),
               ignore_attr = TRUE)

  # Plot for a variable with 4 points elicitation and truth
  truth_data <- list(min = 0.7, max = 0.9, best = 0.8, conf = 100)
  truth_data_rescaled <- rescale_data(truth_data, s = 90)
  p <- plot(obj, round = 2, var = "var3", scale_conf = 90,
            truth = truth_data, verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p[["layers"]], 2)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[1]], "GeomPoint")
  expect_identical(class(p[["layers"]][[2]][["geom"]])[[1]], "GeomErrorbarh")
  expect_identical(ncol(p[["data"]]), 6L)
  expect_identical(colnames(p[["data"]]),
                   c("id", "min", "max", "best", "conf", "col"))
  expect_identical(unique(p[["data"]][["col"]]), c("experts", "truth"))
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]),
                   c(obj[["data"]][["round_2"]][["id"]], "Truth"))
  # The mean function converts the column to double
  expect_equal(p[["data"]][-7, 2:5],
               rescaled_data,
               ignore_attr = TRUE)
  expect_identical(p[["data"]][7, 2:5],
                   truth_data_rescaled,
                   ignore_attr = TRUE)

  # Plot for a variable with 4 points elicitation, group and truth
  p <- plot(obj, round = 2, var = "var3", scale_conf = 90,
            truth = list(min = 0.7, max = 0.9, best = 0.8, conf = 100),
            group = TRUE, verbose = FALSE)
  expect_true(ggplot2::is.ggplot(p))
  expect_length(p[["layers"]], 2)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[1]], "GeomPoint")
  expect_identical(class(p[["layers"]][[2]][["geom"]])[[1]], "GeomErrorbarh")
  expect_identical(ncol(p[["data"]]), 6L)
  expect_identical(colnames(p[["data"]]),
                   c("id", "min", "max", "best", "conf", "col"))
  expect_identical(unique(p[["data"]][["col"]]), c("experts", "group", "truth"))
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]),
                   c(obj[["data"]][["round_2"]][["id"]], "Group", "Truth"))
  # The mean function converts the column to double
  expect_equal(p[["data"]][1:6, 2:5],
               rescaled_data,
               ignore_attr = TRUE)
  expect_equal(as.numeric(p[["data"]][7, 2:4]),
               colMeans(rescaled_data[, 1:3], na.rm = TRUE),
               ignore_attr = TRUE)
  expect_identical(p[["data"]][8, 2:5],
                   truth_data_rescaled,
                   ignore_attr = TRUE)

  # Colours and and other plot elements----
  p <- plot(obj,
            round = 2,
            var = "var3",
            truth = list(min = 0.7, max = 0.9, best = 0.8, conf = 100),
            group = TRUE,
            colour = "yellow",
            group_colour = "brown",
            truth_colour = "pink",
            point_size = 3,
            line_width = 2,
            title = "Test",
            xlab = "test",
            ylab = "Text",
            family = "serif",
            verbose = FALSE)
  ld1 <- ggplot2::layer_data(p, i = 1L)
  n <- obj[["experts"]]
  expect_identical(ld1[["colour"]][seq_len(n)], rep("yellow", n))
  expect_identical(ld1[["colour"]][n + 1], "brown")
  expect_identical(ld1[["colour"]][n + 2], "pink")
  expect_identical(ld1[["size"]], rep(3, n + 2))
  ld2 <- ggplot2::layer_data(p, i = 2L)
  expect_identical(ld2[["colour"]][seq_len(n)], rep("yellow", n))
  expect_identical(ld2[["colour"]][n + 1], "brown")
  expect_identical(ld2[["colour"]][n + 2], "pink")
  expect_identical(ld2[["linewidth"]], rep(2, n + 2))
  expect_identical(ggplot2::ggplot_build(p)[["plot"]][["plot_env"]][["title"]],
                   "Test")
  expect_identical(ggplot2::ggplot_build(p)[["plot"]][["plot_env"]][["xlab"]],
                   "test")
  expect_identical(ggplot2::ggplot_build(p)[["plot"]][["plot_env"]][["ylab"]],
                   "Text")
  expect_identical(p[["theme"]][["axis.text"]][["family"]], "serif")
  expect_identical(p[["theme"]][["axis.title.x"]][["family"]], "serif")
  expect_identical(p[["theme"]][["axis.title.y"]][["family"]], "serif")
  expect_identical(ggplot2::layer_scales(p)[["x"]][["limits"]], c(0, 1))
  expect_identical(p[["theme"]][["legend.position"]], "none")

  # Test theme
  test_theme <- ggplot2::theme(plot.title = ggplot2::element_text(size = 14,
                                                                  hjust = 1))
  p <- plot(obj, round = 2, var = "var3",
            truth = list(min = 0.7, max = 0.9, best = 0.8, conf = 100),
            group = TRUE, theme = test_theme, verbose = FALSE)
  expect_identical(p[["theme"]][["plot.title"]][["size"]], 14)
  expect_identical(p[["theme"]][["plot.title"]][["hjust"]], 1)
  expect_null(p[["theme"]][["plot.face"]][["hjust"]])

})

test_that("Rows with all NAs are removed", {
  obj <- create_cont_obj()
  obj[["data"]][["round_1"]][5:6, 2] <- NA
  obj[["data"]][["round_1"]][1:2, ] <- NA
  p <- plot(obj, round = 1, var = "var1", verbose = FALSE)
  expect_identical(nrow(p[["data"]]), 4L)
  expect_identical(dplyr::pull(p[["data"]][5:6, 2]), rep(NA_integer_, 2))

})

test_that("get_type()", {
  obj <- create_cont_obj()
  expect_identical(get_type(obj, "var1", "var"), "Z")
  expect_identical(get_type(obj, "var2", "var"), "N")
  expect_identical(get_type(obj, "var3", "var"), "p")
  expect_identical(get_type(obj, "var1", "elic"), "1p")
  expect_identical(get_type(obj, "var2", "elic"), "3p")
  expect_identical(get_type(obj, "var3", "elic"), "4p")
})
