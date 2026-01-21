test_that("Errors", {
  obj <- create_cat_obj()
  samp <- cat_sample_data(obj, method = "unweighted", topic = "topic_1",
                          verbose = FALSE)

  # When option is not available in the data
  expect_snapshot(plot(samp, option = "option_7"), error = TRUE)

  # When colours are less than the number of categories
  expect_snapshot(plot(samp, colours = c("red", "blue")),
                  error = TRUE)

  #When type is not valid
  expect_snapshot(plot(samp, type = "boxplot"), error = TRUE)
})

test_that("Output", {
  obj <- create_cat_obj()
  samp <- cat_sample_data(obj, method = "unweighted", topic = "topic_1",
                          verbose = FALSE)

  # When option is not specified
  p <- plot(samp)
  expect_true(ggplot2::is_ggplot(p))
  expect_length(p[["layers"]], 2)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[2]], "Geom")
  expect_identical(class(p[["layers"]][[2]][["geom"]])[[1]], "GeomPoint")
  expect_identical(ncol(p[["data"]]), 4L)
  expect_identical(colnames(p[["data"]]), c("id", "option", "category", "prob"))
  expect_s3_class(p[["data"]][["category"]], "factor")
  expect_identical(levels(p[["data"]][["category"]]), colnames(samp)[-(1:2)])
  expect_identical(ggplot2::layer_scales(p)[["y"]][["limits"]], c(0, 1))

  # When option is specified
  p <- plot(samp, option = c("option_3", "option_2"))
  expect_true(ggplot2::is_ggplot(p))
  expect_length(p[["layers"]], 2)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[2]], "Geom")
  expect_identical(class(p[["layers"]][[2]][["geom"]])[[1]], "GeomPoint")
  expect_identical(ncol(p[["data"]]), 4L)
  expect_identical(colnames(p[["data"]]), c("id", "option", "category", "prob"))
  expect_s3_class(p[["data"]][["category"]], "factor")
  expect_identical(levels(p[["data"]][["category"]]), colnames(samp)[-(1:2)])
  expect_s3_class(p[["data"]][["option"]], "factor")
  expect_identical(levels(p[["data"]][["option"]]), c("option_3", "option_2"))
  expect_identical(ggplot2::layer_scales(p)[["y"]][["limits"]], c(0, 1))

  # Colours and and other plot elements
  p <- plot(samp,
            colours = c("red", "blue", "green", "yellow", "purple"),
            title = "Title",
            ylab = "Y-axis",
            family = "serif")
  ld1 <- ggplot2::layer_data(p, i = 1L)

  expect_identical(unique(ld1[["fill"]]),
                   c("red", "blue", "green", "yellow", "purple"))
  expect_identical(ggplot2::ggplot_build(p)[["plot"]][["plot_env"]][["title"]],
                   "Title")
  expect_identical(ggplot2::ggplot_build(p)[["plot"]][["plot_env"]][["ylab"]],
                   "Y-axis")
  expect_identical(p[["theme"]][["axis.title.y"]][["family"]], "serif")
  expect_identical(p[["theme"]][["axis.text"]][["family"]], "serif")
  expect_identical(p[["theme"]][["legend.position"]], "bottom")

  # Test type = "beeswarm"
  p <- plot(samp, type = "beeswarm")
  # ld1 <- ggplot2::layer_data(p, i = 1L)
  expect_true(ggplot2::is_ggplot(p))
  expect_length(p[["layers"]], 2)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[2]], "Geom")
  expect_identical(class(p[["layers"]][[2]][["geom"]])[[1]], "GeomPoint")
  expect_identical(ncol(p[["data"]]), 4L)
  expect_identical(colnames(p[["data"]]), c("id", "option", "category", "prob"))
  expect_s3_class(p[["data"]][["category"]], "factor")
  expect_identical(levels(p[["data"]][["category"]]), colnames(samp)[-(1:2)])
  # expect_length(unique(ld1[["colour"]]), 5L)
  expect_identical(p[["theme"]][["legend.position"]], "bottom")

  # Test theme
  test_theme <- ggplot2::theme(plot.title = ggplot2::element_text(size = 14,
                                                                  hjust = 1))
  p <- plot(samp, theme = test_theme)

  expect_identical(p[["theme"]][["plot.title"]][["size"]], 14)
  expect_identical(p[["theme"]][["plot.title"]][["hjust"]], 1)
})
