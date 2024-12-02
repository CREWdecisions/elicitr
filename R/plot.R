#' Title
#'
#' @param x
#' @param round
#' @param var
#' @param ...
#' @param group
#' @param group_colour
#' @param truth
#' @param truth_colour
#' @param colour
#' @param point_size
#' @param line_width
#' @param theme
#'
#' @return
#' @export
#'
#' @author Sergio Vignali and Stefano Canessa
#'
#' @examples
plot.elicit <- function(x,
                        round,
                        var,
                        ...,
                        group = FALSE,
                        group_colour = "orange",
                        truth = NULL,
                        truth_colour = "red",
                        colour = "purple",
                        point_size = 4,
                        line_width = 1.5,
                        theme = NULL) {

  check_round(round)
  check_var(x, var)

  if (is.null(theme)) {
    theme <- elic_theme()
  }

  data <- elic_get_data(x, round = round, var = var)
  colnames(data) <- gsub(paste0(var, "_"), "", colnames(data))
  elic_type <- get_type(x, var, "elic_types")
  var_type <- get_type(x, var, "var_types")

  p <- ggplot2::ggplot(data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = .data$best,
                                               y = .data$id),
                        colour = colour,
                        size = point_size) +
    ggplot2::ggtitle(paste("Round", round, "-", var))

  if (!is.null(truth)) {
    check_truth(truth, elic_type)

    truth_data <- data.frame(id = "Truth",
                             best = truth$best)

    p <- p +
      ggplot2::geom_point(data = truth_data,
                          mapping = ggplot2::aes(x = .data$best,
                                                 y = .data$id),
                          colour = truth_colour,
                          size = point_size)
  }

  if (elic_type %in% c("3p", "4p")) {

    if (elic_type == "4p") {

      # Rescale min and max
      var_min <- paste0(var, "_min")
      var_max <- paste0(var, "_max")
      var_conf <- paste0(var, "_conf")
      best <- data$best
      conf <- data$conf

      var_min_scaled <- best - (best - data$min) * (100 / (conf))
      var_max_scaled <- best + (data$max - best) * (100 / (conf))

      data$min <- var_min_scaled
      datamax <- var_max_scaled

      cli::cli_alert_success("Rescaled min and max")
    }

    p <- p +
      ggplot2::geom_errorbarh(data = data,
                              mapping = ggplot2::aes(y = .data$id,
                                                     xmin = .data$min,
                                                     xmax = .data$max),
                              colour = colour,
                              position = "identity",
                              height = 0,
                              size = line_width)

  }

  if (var_type == "p") {
    p <- p +
      ggplot2::scale_x_continuous(name = var,
                                  limits = c(0, 1),
                                  expand = c(0, 0))
  } else {
    p <- p +
      ggplot2::scale_x_continuous(name = var)
  }

  p +
    ggplot2::scale_y_discrete(name = "Expert") +
    theme
}

# Helpers----
get_type <- function(x, var, type) {
  x[[type]][obj$var_names == var]
}

# Checkers----
check_trouth <- function(x, elic_type) {
  if (!is.list(x)) {
    error <- "{.arg truth} is a {.cls {class(x)}} but it should a named \\
              {.cls list}."
    cli::cli_abort(c("Incorrect value for {.arg truth}:",
                     "x" = error,
                     "i" = "See {.fn elicitr::plot.elicit}"),
                   call = rlang::caller_env())
  }
}


# Plot theme----
elic_theme <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(size = 16,
                                                      face = "bold",
                                                      hjust = 0.5),
                   panel.grid.major.x = ggplot2::element_line(colour = "black",
                                                              linetype = 8,
                                                              size = 0.1),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_text(size = 16,
                                                        color = "black"),
                   axis.title.x = ggplot2::element_text(vjust = -1.2,
                                                        size = 16,
                                                        color = "black"),
                   axis.ticks.length = ggplot2::unit(0.5, units = "mm"),
                   axis.text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(5, 10, 5, 5), units = "mm")
    )
}
