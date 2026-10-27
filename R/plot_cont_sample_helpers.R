# Plot theme----

#'Theme
#'
#' Custom theme for continuous samples.
#'
#' @return A [`theme`][`ggplot2::theme`] function.
#' @noRd
#'
#' @author Sergio Vignali and Maude Vernet
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
  } else if (type == "beeswarm") {

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
  }

  th
}

#' Check type
#'
#' Check the imputed type and change it if needed.
#' @param type a character string with the type of plot
#' @param x the data being plotted
#' @param var a character string the variable to plot
#' @param group logical, whether data should be grouped or not
#' @return The modified type
#' @noRd
#'
#' @author Maude Vernet
update_type <- function(type, x, var, group) {

  same_vars <- x[x[["var"]] == var, ] |>
    dplyr::group_by(.data[["id"]]) |>
    dplyr::summarise(all_same =
                       dplyr::n_distinct(.data[["value"]]) == 1) |>
    dplyr::pull("all_same")
  #check if all experts have one value in var (aka is a 1p variable)
  if (!all(is.na(x[["value"]][x[["var"]] == var])) &&
        all(same_vars) &&
        type != "violin" &&
        !group) {
    type <- "violin"
    cli::cli_alert_info("Replacing {.arg type} with {.val {type}} as \\
                        {.val {var}} is a one-point variable.")
  }
  type
}

#' Make base plot
#'
#' Make the baseline plot depending on the type of plot needed
#' @param type character, the type of plot wanted
#' @param x the data to plot
#' @param x_var character, the data to plot on the x axis
#' @param line_width numeric, the linewidth to use in the density plot
#' @param beeswarm_cex character, the cex for the beeswarm plot
#' @param beeswarm_coral character, the coral for the beeswarm plot
#' @return The plot
#' @noRd
#'
#' @author Maude Vernet
make_base_plot <- function(type,
                           x,
                           x_var,
                           line_width,
                           beeswarm_cex,
                           beeswarm_corral) {
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
                           key_glyph = "dotplot",
                           na.rm = TRUE) +
      ggplot2::stat_summary(mapping = ggplot2::aes(x = .data[[x_var]],
                                                   y = .data[["violin_value"]]),
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
                            linewidth = line_width,
                            na.rm = TRUE) +
      ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1))
  } else if (type == "beeswarm") {
    p <- ggplot2::ggplot(x) +
      ggbeeswarm::geom_beeswarm(mapping = ggplot2::aes(x = .data[[x_var]],
                                                       y = .data[["value"]],
                                                       colour = .data[[x_var]]),
                                cex = beeswarm_cex,
                                size = 1,
                                corral = beeswarm_corral,
                                na.rm = TRUE) +
      ggplot2::stat_summary(mapping = ggplot2::aes(x = .data[[x_var]],
                                                   y = .data[["violin_value"]]),
                            fun = mean,
                            geom = "point",
                            colour = "black",
                            size = 0.8)
  }
  p
}
