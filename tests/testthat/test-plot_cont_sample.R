test_that("Errors", {
  obj <- create_cont_obj()
  samp <- cont_sample_data(obj, round = 2, method = "basic", verbose = FALSE)

  # When var is of length > 1
  expect_snapshot(plot(samp, var = c("var1", "var2")),
                  error = TRUE)

  # When var is not on the object
  expect_snapshot(plot(samp, var = "var5"),
                  error = TRUE)

  # When plot type is not available
  expect_snapshot(plot(samp, var = "var1", type = "boxplot"),
                  error = TRUE)

  # When colours has length != number of experts
  expect_snapshot(plot(samp, var = "var1", colours = c("red", "blue")),
                  error = TRUE)

  # When colours has length != 1 and group is TRUE
  expect_snapshot(plot(samp, var = "var1", colours = c("red", "blue"),
                       group = TRUE),
                  error = TRUE)
  # When expert_names has the wrong length
  expect_snapshot(plot(obj, round = 1, var = "var1",
                       expert_names = paste0("E", 1:obj[["experts"]])[-1]),
                  error = TRUE)
  #When multiple names are the same
  expect_snapshot(plot(samp, var = "var1",
                       expert_names = rep("Same", obj[["experts"]])),
                  error = TRUE)
  #When Group is used as expert name
  expect_snapshot(plot(samp, var = "var1",
                       expert_names = c("Group",
                                        paste0("E", 1:obj[["experts"]])[-1])),
                  error = TRUE)
})

test_that("Output", {
  obj <- create_cont_obj()
  samp <- cont_sample_data(obj, round = 2, method = "basic", verbose = FALSE)

  # Violin plot without group
  p <- plot(samp, var = "var1", type = "violin")
  ld1 <- ggplot2::layer_data(p, i = 1L)
  expect_true(ggplot2::is_ggplot(p))
  expect_length(p[["layers"]], 2)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[2]], "Geom")
  expect_identical(class(p[["layers"]][[2]][["geom"]])[[1]], "GeomPoint")
  expect_identical(ncol(p[["data"]]), 3L)
  expect_identical(colnames(p[["data"]]), c("id", "var", "value"))
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]), unique(samp[["id"]]))
  expect_length(unique(ld1[["fill"]]), 6L)
  expect_identical(p[["theme"]][["legend.position"]], "none")

  # Density plot without group
  p <- plot(samp, var = "var1", type = "density")
  ld1 <- ggplot2::layer_data(p, i = 1L)
  expect_true(ggplot2::is_ggplot(p))
  expect_length(p[["layers"]], 1)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[1]], "GeomLine")
  expect_identical(ncol(p[["data"]]), 3L)
  expect_identical(colnames(p[["data"]]), c("id", "var", "value"))
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]), unique(samp[["id"]]))
  expect_length(unique(ld1[["colour"]]), 6L)
  expect_identical(p[["theme"]][["legend.position"]], "bottom")

  # Beeswarm plot without group
  p <- plot(samp, var = "var1", type = "beeswarm")
  # ld1 <- ggplot2::layer_data(p, i = 1L)
  expect_true(ggplot2::is_ggplot(p))
  expect_length(p[["layers"]], 2)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[1]], "GeomPoint")
  expect_identical(ncol(p[["data"]]), 3L)
  expect_identical(colnames(p[["data"]]), c("id", "var", "value"))
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]), unique(samp[["id"]]))
  # expect_length(unique(ld1[["colour"]]), 6L)
  expect_identical(p[["theme"]][["legend.position"]], "none")

  # Violin plot with group
  p <- plot(samp, var = "var1", type = "violin", group = TRUE)
  ld1 <- ggplot2::layer_data(p, i = 1L)
  expect_true(ggplot2::is_ggplot(p))
  expect_length(p[["layers"]], 2)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[2]], "Geom")
  expect_identical(class(p[["layers"]][[2]][["geom"]])[[1]], "GeomPoint")
  expect_identical(ncol(p[["data"]]), 3L)
  expect_identical(colnames(p[["data"]]), c("id", "var", "value"))
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]), unique(samp[["id"]]))
  expect_length(unique(ld1[["fill"]]), 1L)
  expect_identical(p[["theme"]][["legend.position"]], "none")

  # Density plot with group
  p <- plot(samp, var = "var1", type = "density", group = TRUE)
  ld1 <- ggplot2::layer_data(p, i = 1L)
  expect_true(ggplot2::is_ggplot(p))
  expect_length(p[["layers"]], 1)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[1]], "GeomLine")
  expect_identical(ncol(p[["data"]]), 3L)
  expect_identical(colnames(p[["data"]]), c("id", "var", "value"))
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]), unique(samp[["id"]]))
  expect_length(unique(ld1[["colour"]]), 1L)
  expect_identical(p[["theme"]][["legend.position"]], "bottom")

  # Beeswarm plot with group
  p <- plot(samp, var = "var1", type = "beeswarm", group = TRUE)
  # ld1 <- ggplot2::layer_data(p, i = 1L)
  expect_true(ggplot2::is_ggplot(p))
  expect_length(p[["layers"]], 2)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[2]], "Geom")
  expect_identical(class(p[["layers"]][[2]][["geom"]])[[1]], "GeomPoint")
  expect_identical(ncol(p[["data"]]), 3L)
  expect_identical(colnames(p[["data"]]), c("id", "var", "value"))
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]), unique(samp[["id"]]))
  # expect_length(unique(ld1[["colour"]]), 1L)
  expect_identical(p[["theme"]][["legend.position"]], "none")

  # Beeswarm plot with cex and corral
  p <- plot(samp, var = "var1", type = "beeswarm", group = TRUE,
            beeswarm_cex = 0.8,
            beeswarm_corral = "wrap")
  # ld1 <- ggplot2::layer_data(p, i = 1L)
  expect_true(ggplot2::is_ggplot(p))
  expect_length(p[["layers"]], 2)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[2]], "Geom")
  expect_identical(class(p[["layers"]][[2]][["geom"]])[[1]], "GeomPoint")
  expect_identical(ncol(p[["data"]]), 3L)
  expect_identical(colnames(p[["data"]]), c("id", "var", "value"))
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]), unique(samp[["id"]]))
  # expect_length(unique(ld1[["colour"]]), 1L)
  expect_identical(p[["theme"]][["legend.position"]], "none")

  # Colours and and other plot elements
  cols <- c("steelblue4", "darkcyan", "chocolate1",
            "chocolate3", "orangered4", "royalblue1")
  p <- plot(samp,
            var = "var1",
            title = "title",
            xlab = "xlab",
            ylab = "ylab",
            colours = cols,
            line_size = 1.5,
            family = "serif")
  ld1 <- ggplot2::layer_data(p, i = 1L)
  expect_identical(unique(ld1[["fill"]]), cols)
  expect_identical(ggplot2::ggplot_build(p)[["plot"]][["plot_env"]][["title"]],
                   "title")
  expect_identical(ggplot2::ggplot_build(p)[["plot"]][["plot_env"]][["xlab"]],
                   "xlab")
  expect_identical(ggplot2::ggplot_build(p)[["plot"]][["plot_env"]][["ylab"]],
                   "ylab")
  expect_identical(p[["theme"]][["axis.title.y"]][["family"]], "serif")
  expect_identical(p[["theme"]][["axis.text"]][["family"]], "serif")

  # Test theme
  test_theme <- ggplot2::theme(plot.title = ggplot2::element_text(size = 14,
                                                                  hjust = 1))
  p <- plot(samp, var = "var1", theme = test_theme)
  expect_identical(p[["theme"]][["plot.title"]][["size"]], 14)
  expect_identical(p[["theme"]][["plot.title"]][["hjust"]], 1)
  expect_null(p[["theme"]][["plot.face"]][["hjust"]])

  # Test expert renaming
  new_names <- paste("Expert", 1:obj[["experts"]])
  p <- plot(samp, var = "var1",
            expert_names = new_names,
            verbose = FALSE)
  expect_identical(levels(p[["data"]][["id"]]),
                   new_names)
})
