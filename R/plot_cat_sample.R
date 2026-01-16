#' Plot categorical samples
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `plot()` aggregates and plots categorical samples as violin plot.
#'
#' @inheritParams cat_get_data
#' @param x an object of class `cat_sample` created by the function
#' [cat_sample_data].
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
#' @family plot helpers
#'
#' @author Sergio Vignali and Maude Vernet
#'
#' @examples
#' # Create the elic_cat object for an elicitation process with three topics,
#' # four options, five categories and a maximum of six experts per topic
#' my_categories <- c("category_1", "category_2", "category_3",
#'                    "category_4", "category_5")
#' my_options <- c("option_1", "option_2", "option_3", "option_4")
#' my_topics <- c("topic_1", "topic_2", "topic_3")
#' my_elicit <- cat_start(categories = my_categories,
#'                        options = my_options,
#'                        experts = 6,
#'                        topics = my_topics) |>
#'   cat_add_data(data_source = topic_1, topic = "topic_1") |>
#'   cat_add_data(data_source = topic_2, topic = "topic_2") |>
#'   cat_add_data(data_source = topic_3, topic = "topic_3")
#'
#' # Sample data from Topic 1 for all options using the basic method
#' samp <- cat_sample_data(my_elicit,
#'                         method = "basic",
#'                         topic = "topic_1")
#'
#' # Plot the sampled data for all options
#' plot(samp)
#'
#' # Plot the sampled data for option 1
#' plot(samp, option = "option_1")
#'
#' # Plot the sampled data for option 1 and 3
#' plot(samp, option = c("option_1", "option_3"))
#'
#' # Provide custom colours
#' plot(samp, colours = c("steelblue4", "darkcyan", "chocolate1",
#'                        "chocolate3", "orangered4"))
#'
#' # Overwrite the default theme
#' plot(samp, theme = ggplot2::theme_minimal())
plot.cat_sample <- function(x,
                            ...,
                            option = "all",
                            title = NULL,
                            ylab = "Probability",
                            colours = NULL,
                            family = "sans",
                            theme = NULL) {

  if (any(option != "all")) {

    # Check if option is available
    check_option(x, option)

    # Avoid overwrite dplyr variable
    vals <- option
    x <- x |>
      dplyr::filter(.data[["option"]] %in% vals) |>
      dplyr::mutate("option" = factor(.data[["option"]], levels = vals))
  }

  if (is.null(title)) {
    title <- attr(x, "topic")
  }

  x <- x |>
    tidyr::pivot_longer(cols = -c("id", "option"),
                        names_to = "category",
                        values_to = "prob") |>
    dplyr::mutate("category" = factor(.data[["category"]],
                                      levels = unique(.data[["category"]])))

  if (is.null(colours)) {
    colours <- scales::hue_pal()(length(unique(x[["category"]])))
  } else {

    n_cat <- length(unique(x[["category"]]))
    if (length(colours) != n_cat) {

      error <- "The number of colours provided does not match the number \\
                of categories."
      cli::cli_abort(c("Invalid value for argument {.arg colours}:",
                       "x" = error,
                       "i" = "Please provide a vector with {.val {n_cat}} \\
                              colours."))
    }
  }

  if (is.null(theme)) {
    theme <- elic_theme(family = family) +
      cat_sample_theme()
  }

  p <- ggplot2::ggplot(x) +
    ggplot2::geom_violin(mapping = ggplot2::aes(x = .data[["category"]],
                                                y = .data[["prob"]],
                                                fill = .data[["category"]]),
                         colour = "black",
                         alpha = 0.8,
                         scale = "width",
                         linewidth = 0.2,
                         quantiles = c(0.25, 0.75),
                         quantile.linetype = 1L,
                         key_glyph = "dotplot") +
    ggplot2::stat_summary(mapping = ggplot2::aes(x = .data[["category"]],
                                                 y = .data[["prob"]]),
                          fun = mean,
                          geom = "point",
                          colour = "black",
                          size = 0.8) +
    ggplot2::labs(title = title,
                  y = ylab) +
    ggplot2::facet_wrap("option") +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::scale_y_continuous(limits = c(0, 1),
                                expand = ggplot2::expansion(mult = c(0,
                                                                     0.04))) +
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
