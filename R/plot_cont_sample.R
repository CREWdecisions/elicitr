#' Plot continuous samples
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `plot()` aggregates and plots continuous samples as violin or density plot.
#'
#' @inheritParams cont_get_data
#' @param x n object of class `cont_sample` created by the function
#' [cont_sample_data].
#' @param var character string with the name of the variable to be plotted.
#' @param group logical, if `TRUE` data are aggregated by expert.
#' @param type character string with the type of plot, either _violin_ or
#' _density_.
#' @param title character string with the title of the plot.
#' @param xlab character string with the x-axis label.
#' @param ylab character string with the y-axis label.
#' @param colours character vector with the colours to be used in the plot.
#' @param line_width numeric with the width of the lines in the density plot.
#' @param family character string with the font family to be used in the plot.
#' @param expert_names numeric or character, the labels for the experts.
#' @param theme [`theme`][`ggplot2::theme`] function to be used in the plot.
#'
#' #' @section scale_conf:
#'
#' If the variable plotted is the result of a four points elicitation where
#' expert confidence is provided, the minimum and maximum values provided by
#' each expert are rescaled using their provided confidence categories. Users
#' can choose how they want to rescale minimum and maximum values by providing a
#' value for the `scale_conf` argument. If no argument is provided, a default
#' value of 100 is used for scale_conf.
#'
#' The scaled minimum and maximum values are obtained with:
#'
#' \eqn{minimum = best\ guess - (best\ guess - minimum)\frac{scale\_conf}
#' {confidence}}
#'
#' \eqn{maximum = best\ guess + (maximum - best\ guess) \frac{scale\_conf}
#' {confidence}}
#'
#'
#' @details If a `theme` is provided, the `family` argument is ignored.
#'
#' @returns Invisibly a [`ggplot`][`ggplot2::ggplot`] object.
#' @export
#'
#' @family plot helpers
#'
#' @author Sergio Vignali
#'
#' @examples
#' # Create the elict object and add data for the first and second round from a
#' # data.frame.
#' my_elicit <- cont_start(var_names = c("var1", "var2", "var3"),
#'                         var_types = "ZNp",
#'                         elic_types = "134",
#'                         experts = 6) |>
#'   cont_add_data(x, data_source = round_1, round = 1) |>
#'   cont_add_data(data_source = round_2, round = 2)
#'
#' # Sample data for the first round for all variables
#' samp <- cont_sample_data(my_elicit, round = 1)
#'
#' # Plot the sampled data for the variable `var3` as violin plot
#' plot(samp, var = "var3", type = "violin")
#'
#' # Plot the sampled data for the variable `var2` as density plot
#' plot(samp, var = "var2", type = "density")
#'
#' # Plot the sampled data for the variable `var1` as density plot for the group
#' plot(samp, var = "var1", group = TRUE, type = "density")
#'
#' # Plot the sampled data for the variable `var3` as violin plot passing the
#' # colours
#' plot(samp, var = "var3", type = "violin",
#'      colours = c("steelblue4", "darkcyan", "chocolate1",
#'                  "chocolate3", "orangered4", "royalblue1"))
plot.cont_sample <- function(x,
                             var,
                             ...,
                             group = FALSE,
                             type = "violin",
                             title = NULL,
                             xlab = "",
                             ylab = "",
                             colours = NULL,
                             line_width = 0.7,
                             family = "sans",
                             expert_names = NULL,
                             theme = NULL) {

  if (!is.null(expert_names)) {
    x <- cont_rename_experts(x = x,
                             data = x,
                             expert_names)
  }

  # Check if var is available
  check_length(var, "var", 1)
  check_var_in_sample(x, var)

  # Avoid overwrite dplyr variable
  vars <- var
  x <- x |>
    dplyr::filter(.data[["var"]] %in% vars) |>
    dplyr::mutate("id" = factor(.data[["id"]], levels = unique(x[["id"]])))

  if (group) {
    n <- 1
    x_var <- "var"
    error <- "The number of colours provided is more than {.val {1}}."
    info <- "Please provide only {.val {1}} colour when {.code group = TRUE}."
  } else {
    n <- unique(x[["id"]]) |>
      length()
    x_var <- "id"
    error <- "The number of colours provided does not match the number of \\
              experts."
    info <- "Please provide a vector with {.val {n}} colours."
  }

  if (is.null(title)) {
    title <- paste("Round", attr(x, "round"))
  }

  if (is.null(colours)) {
    colours <- scales::hue_pal()(n)
  } else {

    if (length(colours) != n) {

      cli::cli_abort(c("Invalid value for argument {.arg colours}:",
                       "x" = error,
                       "i" = info))
    }
  }

  if (is.null(theme)) {
    theme <- elic_theme(family = family) +
      cont_sample_theme(type = type, group = group)
  }

  if (type == "violin") {
    p <- ggplot2::ggplot(x) +
      ggplot2::geom_violin(mapping = ggplot2::aes(x = .data[[x_var]],
                                                  y = .data[["value"]],
                                                  fill = .data[[x_var]]),
                           colour = "black",
                           alpha = 0.8,
                           scale = "width",
                           linewidth = 0.2,
                           quantiles = c(0.25, 0.75),
                           quantile.linetype = 1L,
                           key_glyph = "dotplot") +
      ggplot2::stat_summary(mapping = ggplot2::aes(x = .data[[x_var]],
                                                   y = .data[["value"]]),
                            fun = mean,
                            geom = "point",
                            colour = "black",
                            size = 0.8)
  } else if (type == "density") {
    p <- ggplot2::ggplot(x) +
      ggplot2::stat_density(mapping = ggplot2::aes(x = .data[["value"]],
                                                   colour = .data[[x_var]]),
                            geom = "line",
                            position = "identity",
                            linewidth = line_width) +
      ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1))
  } else {

    info <- "Available types are {.val violin} and {.val density}."
    cli::cli_abort(c("Invalid value for argument {.arg type}:",
                     "x" = "Type {.val {type}} is not implemented.",
                     "i" = info))
  }

  p <- p +
    ggplot2::labs(title = title,
                  x = xlab,
                  y = ylab) +
    ggplot2::scale_fill_manual(values = colours) +
    theme

  p
}

# Plot theme----

#'Theme
#'
#' Custom theme for continuous samples.
#'
#' @return A [`theme`][`ggplot2::theme`] function.
#' @noRd
#'
#' @author Sergio Vignali
cont_sample_theme <- function(type, group) {

  y_grid <- ggplot2::element_line(colour = "black",
                                  linetype = 8,
                                  linewidth = 0.1)

  if (type == "violin") {
    th <- ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                         panel.grid.major.x = ggplot2::element_blank(),
                         panel.grid.major.y = y_grid,
                         legend.position = "none")

    if (!group) {
      th <- th +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                           vjust = 0.5,
                                                           hjust = 1))
    }

  } else if (type == "density") {
    th <- ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                         panel.grid.major.x = ggplot2::element_blank(),
                         panel.grid.major.y = y_grid,
                         legend.position = "bottom",
                         legend.key.size = ggplot2::unit(1.5, "line"),
                         legend.title = ggplot2::element_blank())
  } else {
    th <- NULL
  }

  th
}
