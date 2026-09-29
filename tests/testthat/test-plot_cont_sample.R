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

  #When only NAs are present in the estimates (1p variable)
  obj[["data"]][["round_2"]][["var1_best"]] <- NA
  samp <- cont_sample_data(obj, round = 2, method = "basic", verbose = FALSE)

  expect_snapshot(plot(samp, var = "var1", type = "beeswarm"),
                  error = TRUE)

  #When only NAs are present in the estimates (no variable provided)
  samp <- cont_sample_data(obj, var = "var1", round = 2,
                           method = "basic", verbose = FALSE)

  expect_snapshot(plot(samp, type = "beeswarm"),
                  error = TRUE)

  #When only NAs are present in the estimates (2p variable)
  obj <- create_cont_obj()
  obj[["data"]][["round_2"]][["var2_min"]] <- NA
  obj[["data"]][["round_2"]][["var2_max"]] <- NA
  obj[["data"]][["round_2"]][["var2_best"]] <- NA
  samp <- cont_sample_data(obj, round = 2, method = "basic", verbose = FALSE)

  expect_snapshot(plot(samp, var = "var2", type = "beeswarm"),
                  error = TRUE)

  #When only NAs are present in the estimates (3p variable)
  obj <- create_cont_obj()
  obj[["data"]][["round_2"]][["var3_min"]] <- NA
  obj[["data"]][["round_2"]][["var3_max"]] <- NA
  obj[["data"]][["round_2"]][["var3_best"]] <- NA
  obj[["data"]][["round_2"]][["var3_conf"]] <- NA
  samp <- cont_sample_data(obj, round = 2, method = "basic", verbose = FALSE)

  expect_snapshot(plot(samp, var = "var3", type = "beeswarm"),
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
  expect_identical(ncol(p[["data"]]), 5L)
  expect_identical(colnames(p[["data"]]), c("id", "var", "value",
                                            "missing", "violin_value"))
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]), unique(samp[["id"]]))
  expect_length(unique(ld1[["fill"]]), 6L)
  expect_identical(p[["theme"]][["legend.position"]], "none")

  # Density plot without group
  p <- plot(samp, var = "var2", type = "density")
  ld1 <- ggplot2::layer_data(p, i = 1L)
  expect_true(ggplot2::is_ggplot(p))
  expect_length(p[["layers"]], 1)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[1]], "GeomLine")
  expect_identical(ncol(p[["data"]]), 5L)
  expect_identical(colnames(p[["data"]]), c("id", "var", "value",
                                            "missing", "violin_value"))
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]), unique(samp[["id"]]))
  expect_length(unique(ld1[["colour"]]), 6L)
  expect_identical(p[["theme"]][["legend.position"]], "bottom")

  # Beeswarm plot without group
  p <- plot(samp, var = "var2", type = "beeswarm")
  expect_true(ggplot2::is_ggplot(p))
  expect_length(p[["layers"]], 2)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[1]], "GeomPoint")
  expect_identical(ncol(p[["data"]]), 5L)
  expect_identical(colnames(p[["data"]]), c("id", "var", "value",
                                            "missing", "violin_value"))
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]), unique(samp[["id"]]))
  expect_identical(p[["theme"]][["legend.position"]], "none")

  # Violin plot with group
  p <- plot(samp, var = "var1", type = "violin", group = TRUE)
  ld1 <- ggplot2::layer_data(p, i = 1L)
  expect_true(ggplot2::is_ggplot(p))
  expect_length(p[["layers"]], 2)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[2]], "Geom")
  expect_identical(class(p[["layers"]][[2]][["geom"]])[[1]], "GeomPoint")
  expect_identical(ncol(p[["data"]]), 5L)
  expect_identical(colnames(p[["data"]]), c("id", "var", "value",
                                            "missing", "violin_value"))
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
  expect_identical(ncol(p[["data"]]), 5L)
  expect_identical(colnames(p[["data"]]), c("id", "var", "value",
                                            "missing", "violin_value"))
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]), unique(samp[["id"]]))
  expect_length(unique(ld1[["colour"]]), 1L)
  expect_identical(p[["theme"]][["legend.position"]], "bottom")

  # Beeswarm plot with group
  p <- plot(samp, var = "var1", type = "beeswarm", group = TRUE)
  expect_true(ggplot2::is_ggplot(p))
  expect_length(p[["layers"]], 2)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[2]], "Geom")
  expect_identical(class(p[["layers"]][[2]][["geom"]])[[1]], "GeomPoint")
  expect_identical(ncol(p[["data"]]), 5L)
  expect_identical(colnames(p[["data"]]), c("id", "var", "value",
                                            "missing", "violin_value"))
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]), unique(samp[["id"]]))
  expect_identical(p[["theme"]][["legend.position"]], "none")

  # Beeswarm plot with cex and corral
  p <- plot(samp, var = "var1", type = "beeswarm", group = TRUE,
            beeswarm_cex = 0.8,
            beeswarm_corral = "wrap")
  expect_true(ggplot2::is_ggplot(p))
  expect_length(p[["layers"]], 2)
  expect_identical(class(p[["layers"]][[1]][["geom"]])[[2]], "Geom")
  expect_identical(class(p[["layers"]][[2]][["geom"]])[[1]], "GeomPoint")
  expect_identical(ncol(p[["data"]]), 5L)
  expect_identical(colnames(p[["data"]]), c("id", "var", "value",
                                            "missing", "violin_value"))
  expect_s3_class(p[["data"]][["id"]], "factor")
  expect_identical(levels(p[["data"]][["id"]]), unique(samp[["id"]]))
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
  expect_identical(ggplot2::ggplot_build(p)[["plot"]][["labels"]][["title"]],
                   "title")
  expect_identical(ggplot2::ggplot_build(p)[["plot"]][["labels"]][["x"]],
                   "xlab")
  expect_identical(ggplot2::ggplot_build(p)[["plot"]][["labels"]][["y"]],
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

  #Test no variable input when only one variable in sampled data
  samp <- list(samp1 = cont_sample_data(obj, round = 2, var = "var1",
                                        method = "basic", verbose = FALSE), #1p
               samp3 = cont_sample_data(obj, round = 2, var = "var2",
                                        method = "basic", verbose = FALSE), #2p
               samp4 = cont_sample_data(obj, round = 2, var = "var3",
                                        method = "basic", verbose = FALSE)) #3p

  for (i in 1:3) {
    p <- plot(samp[[i]], verbose = FALSE)
    ld1 <- ggplot2::layer_data(p, i = 1L)
    expect_true(ggplot2::is_ggplot(p))
    expect_length(p[["layers"]], 2)
    expect_identical(class(p[["layers"]][[1]][["geom"]])[[2]], "Geom")
    expect_identical(class(p[["layers"]][[2]][["geom"]])[[1]], "GeomPoint")
    expect_identical(ncol(p[["data"]]), 5L)
    expect_identical(colnames(p[["data"]]), c("id", "var", "value",
                                              "missing", "violin_value"))
    expect_s3_class(p[["data"]][["id"]], "factor")
    expect_identical(levels(p[["data"]][["id"]]), unique(samp[[i]][["id"]]))
    expect_length(unique(ld1[["fill"]]), 6L)
    expect_identical(p[["theme"]][["legend.position"]], "none")
  }
})

test_that("violin plot rendered if type is not violin and elic_type = 1p", {
  withr::local_pdf(NULL)
  obj <- create_cont_obj()
  samp <- cont_sample_data(obj, round = 2, method = "basic", verbose = FALSE)
  #beeswarm
  expect_snapshot(p <- plot(samp, var = "var1", type = "beeswarm"))
  ld1 <- ggplot2::layer_data(p, i = 1L)
  expect_true(ggplot2::is_ggplot(p))
  expect_length(p[["layers"]], 2)
  expect_named(p[["layers"]], c("geom_violin", "stat_summary"))

  #density
  expect_snapshot(p <- plot(samp, var = "var1", type = "density"))
  ld1 <- ggplot2::layer_data(p, i = 1L)
  expect_true(ggplot2::is_ggplot(p))
  expect_length(p[["layers"]], 2)
  expect_named(p[["layers"]], c("geom_violin", "stat_summary"))

  #still density if group
  #beeswarm
  p <- plot(samp, var = "var1", type = "beeswarm", group = TRUE)
  ld1 <- ggplot2::layer_data(p, i = 1L)
  expect_true(ggplot2::is_ggplot(p))
  expect_length(p[["layers"]], 2)
  expect_named(p[["layers"]], c("geom_beeswarm", "stat_summary"))

  #density
  p <- plot(samp, var = "var1", type = "density", group = TRUE)
  ld1 <- ggplot2::layer_data(p, i = 1L)
  expect_true(ggplot2::is_ggplot(p))
  expect_length(p[["layers"]], 1)
  expect_named(p[["layers"]], "stat_density")
})

test_that("Deals with NAs correctly", {
  withr::local_pdf(NULL)
  #1p variable & 2 NA, beeswarm
  obj <- create_cont_obj()
  obj[["data"]][["round_2"]][["var1_best"]][c(1, 3)] <- NA
  samp <- cont_sample_data(obj, round = 2, method = "basic", verbose = FALSE)

  p <- plot(samp, var = "var1", type = "beeswarm")
  expect_length(p[["layers"]], 3L)
  expect_named(p[["layers"]][1], "geom_violin")
  expect_named(p[["layers"]][2], "stat_summary")
  expect_named(p[["layers"]][3], "geom_label")
  p_data <- ggplot2::ggplot_build(p)
  expect_false(is.null(p_data[["plot"]][["labels"]][["subtitle"]]))
  expect_length(which(!is.na(p_data[["data"]][[3]][["label"]])), 2L)

  #1p variable & 2 NA, violin
  p <- plot(samp, var = "var1", type = "violin")
  expect_length(p[["layers"]], 3L)
  expect_named(p[["layers"]][1], "geom_violin")
  expect_named(p[["layers"]][2], "stat_summary")
  expect_named(p[["layers"]][3], "geom_label")
  p_data <- ggplot2::ggplot_build(p)
  expect_false(is.null(p_data[["plot"]][["labels"]][["subtitle"]]))
  expect_length(which(!is.na(p_data[["data"]][[3]][["label"]])), 2L)

  #1p variable & 2 NA, density
  p <- plot(samp, var = "var1", type = "density")
  expect_length(p[["layers"]], 3L)
  expect_named(p[["layers"]][1], "geom_violin")
  p_data <- ggplot2::ggplot_build(p)
  expect_false(is.null(p_data[["plot"]][["labels"]][["subtitle"]]))

  #not 1p variable & 2 NA, beeswarm
  obj <- create_cont_obj()
  obj[["data"]][["round_2"]][["var2_min"]][c(1, 3)] <- NA
  obj[["data"]][["round_2"]][["var2_max"]][c(1, 3)] <- NA
  obj[["data"]][["round_2"]][["var2_best"]][c(1, 3)] <- NA
  samp <- cont_sample_data(obj, round = 2, method = "basic", verbose = FALSE)

  p <- plot(samp, var = "var2", type = "beeswarm")
  expect_length(p[["layers"]], 3L)
  expect_named(p[["layers"]][1], "geom_beeswarm")
  expect_named(p[["layers"]][2], "stat_summary")
  expect_named(p[["layers"]][3], "geom_label")
  p_data <- ggplot2::ggplot_build(p)
  expect_false(is.null(p_data[["plot"]][["labels"]][["subtitle"]]))
  expect_length(which(!is.na(p_data[["data"]][[3]][["label"]])), 2L)

  #1p variable & 1 NA, beeswarm
  obj <- create_cont_obj()
  obj[["data"]][["round_2"]][["var1_best"]][3] <- NA
  samp <- cont_sample_data(obj, round = 2, method = "basic", verbose = FALSE)

  p <- plot(samp, var = "var1", type = "beeswarm")
  expect_length(p[["layers"]], 3L)
  expect_named(p[["layers"]][1], "geom_violin")
  expect_named(p[["layers"]][2], "stat_summary")
  expect_named(p[["layers"]][3], "geom_label")
  p_data <- ggplot2::ggplot_build(p)
  expect_false(is.null(p_data[["plot"]][["labels"]][["subtitle"]]))
  expect_length(which(!is.na(p_data[["data"]][[3]][["label"]])), 1L)

  #1p variable & 1 NA, violin
  p <- plot(samp, var = "var1", type = "violin")
  expect_length(p[["layers"]], 3L)
  expect_named(p[["layers"]][1], "geom_violin")
  expect_named(p[["layers"]][2], "stat_summary")
  expect_named(p[["layers"]][3], "geom_label")
  p_data <- ggplot2::ggplot_build(p)
  expect_false(is.null(p_data[["plot"]][["labels"]][["subtitle"]]))
  expect_length(which(!is.na(p_data[["data"]][[3]][["label"]])), 1L)

  #1p variable & 1 NA, density
  p <- plot(samp, var = "var1", type = "density")
  expect_length(p[["layers"]], 3L)
  expect_named(p[["layers"]][1], "geom_violin")
  p_data <- ggplot2::ggplot_build(p)
  expect_false(is.null(p_data[["plot"]][["labels"]][["subtitle"]]))

  #not 1p variable & 1 NA, beeswarm
  obj <- create_cont_obj()
  obj[["data"]][["round_2"]][["var2_min"]][3] <- NA
  obj[["data"]][["round_2"]][["var2_max"]][3] <- NA
  obj[["data"]][["round_2"]][["var2_best"]][3] <- NA
  samp <- cont_sample_data(obj, round = 2, method = "basic", verbose = FALSE)

  p <- plot(samp, var = "var2", type = "beeswarm")
  expect_length(p[["layers"]], 3L)
  expect_named(p[["layers"]][1], "geom_beeswarm")
  expect_named(p[["layers"]][2], "stat_summary")
  expect_named(p[["layers"]][3], "geom_label")
  p_data <- ggplot2::ggplot_build(p)
  expect_false(is.null(p_data[["plot"]][["labels"]][["subtitle"]]))
  expect_length(which(!is.na(p_data[["data"]][[3]][["label"]])), 1L)
})
