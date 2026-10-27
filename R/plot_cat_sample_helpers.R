# Plot theme----

#'Theme
#'
#' Custom theme for categorical samples.
#'
#' @return A [`theme`][`ggplot2::theme`] function.
#' @noRd
#'
#' @author Sergio Vignali and Maude Vernet
cat_sample_theme <- function() {
  ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_blank(),
                 panel.grid.major.x = ggplot2::element_blank(),
                 panel.grid.major.y = ggplot2::element_line(colour = "black",
                                                            linetype = 8,
                                                            linewidth = 0.1),
                 legend.position = "bottom",
                 legend.key.size = ggplot2::unit(1.5, "line"),
                 legend.title = ggplot2::element_blank())
}
