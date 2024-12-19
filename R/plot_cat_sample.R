#' Plot categorical samples
#'
#' The function aggregates and plots the categorical samples as violin plot.
#'
#' @param x an object of class `cat_sample` created by the function
#' [cat_sample_data].
#' @inheritParams cat_get_data
#' @param title character string with the title of the plot. If `NULL`, the
#' title will be the topic name.
#' @param ylab character string with the label of the y-axis.
#' @param colours vector of colours to use for the categories.
#' @param family character string with the font family to use in the plot.
#' @param theme a [`theme`][`ggplot2::theme`] function to overwrite the default
#' theme.
#'
#' @details If a `theme` is provided, the `family` argument is ignored.
#'
#' @returns Invisibly a [`ggplot`][`ggplot2::ggplot`] object.
#' @export
#'
#' @author Sergio Vignali and Maude Vernet
#'
#' @examples
#' # Create the elic_cat object for an elicitation process with three
#' # mechanisms, four sites, five levels and a maximum of six experts per
#' # mechanism
#' my_levels <- c("level_1", "level_2", "level_3", "level_4", "level_5")
#' my_sites <- c("site_1", "site_2", "site_3", "site_4")
#' my_mechanisms <- c("mechanism_1", "mechanism_2", "mechanism_3")
#' my_elicit <- cat_start(levels = my_levels,
#'                        sites = my_sites,
#'                        experts = 6,
#'                        mechanisms = my_mechanisms) |>
#'   cat_add_data(data_source = mechanism_1, mechanism = "mechanism_1") |>
#'   cat_add_data(data_source = mechanism_2, mechanism = "mechanism_2") |>
#'   cat_add_data(data_source = mechanism_3, mechanism = "mechanism_3")
#'
#' # Sample data from Mechanism 1 for all sites using the basic method
#' samp <- cat_sample_data(my_elicit,
#'                         method = "basic",
#'                         mechanism = "mechanism_1")
#'
#' # Plot the sampled data for all sites
#' plot(samp)
#'
#' # Plot the sampled data for site 1
#' plot(samp, site = "site_1")
#'
#' # Plot the sampled data for site 1 and 3
#' plot(samp, site = c("site_1", "site_3"))
#'
#' # Provide custom colours
#' plot(samp, colours = c("steelblue4", "darkcyan", "chocolate1",
#'                        "chocolate3", "orangered4"))
#'
#' # Overwrite the default theme
#' plot(samp, theme = ggplot2::theme_minimal())
plot.cat_sample <- function(x,
                            ...,
                            site = "all",
                            title = NULL,
                            ylab = "Probabolity",
                            colours = NULL,
                            family = "sans",
                            theme = NULL) {

  if (sum(site != "all") > 0) {

    # Check if site is not among the available sites in the data
    available_sites <- unique(x[["site"]])
    diff <- setdiff(site, available_sites)

    if (length(diff) > 0) {
      error <- "{cli::qty(diff)} Site{?s} {.val {diff}} not available in \\
               the sampled data."
      info <- "Available site{?s}: {.val {available_sites}}."
      cli::cli_abort(c("Invalid value for argument {.arg site}:",
                       "x" = error,
                       "i" = info))
    }

    # Avoid overwrite dplyr variable
    vals <- site
    x <- x |>
      dplyr::filter(.data[["site"]] %in% vals)
  }

  if (is.null(title)) {
    title <- attr(x, "mechanism")
  }

  x <- x |>
    tidyr::pivot_longer(cols = -c("id", "site"),
                        names_to = "level",
                        values_to = "prob")

  if (is.null(colours)) {
    colours <- scales::hue_pal()(length(unique(x[["level"]])))
  } else {

    n_level <- length(unique(x[["level"]]))
    if (length(colours) != n_level) {

      error <- "The number of colours provided does not match the number \\
                of levels."
      cli::cli_abort(c("Invalid value for argument {.arg colours}:",
                       "x" = error,
                       "i" = "Please provide a vector of colours of length \\
                              {.val {n_level}}."))
    }
  }

  if (is.null(theme)) {
    theme <- elic_theme(family = family) +
      cat_sample_theme()
  }

  p <- ggplot2::ggplot(x) +
    ggplot2::geom_violin(mapping = ggplot2::aes(x = .data[["level"]],
                                                y = .data[["prob"]],
                                                fill = .data[["level"]]),
                         color = "black",
                         alpha = 0.8,
                         scale = "width",
                         linewidth = 0.2,
                         draw_quantiles = c(0.25,0.75),
                         key_glyph = "dotplot") +
    ggplot2::stat_summary(mapping = ggplot2::aes(x = .data[["level"]],
                                                 y = .data[["prob"]]),
                          fun = mean,
                          geom = "point",
                          color = "black",
                          size = 0.8) +
    ggplot2::labs(title = title,
                  y = ylab) +
    ggplot2::facet_wrap("site") +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::scale_y_continuous(limits = c(0, 1),
                                expand = ggplot2::expansion(mult = c(0, .04))) +
    theme

  p
}

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
