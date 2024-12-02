#' Plot elicitation data
#'
#' Plot elicitation data for a specific round and variable.
#'
#' @param var character string, the variable to be plotted.
#' @param group logical, whether to plot the group mean.
#' @param group_colour character string, the colour of the group mean.
#' @param scale_conf numeric, the scale factor for the confidence interval.
#' @param truth list, the true value of the variable, see Details for more.
#' @param truth_colour character string, the colour of the true value.
#' @param colour character string, the colour of estimated values.
#' @param point_size numeric, the size of the points.
#' @param line_width numeric, the width of the lines.
#' @param theme a [`theme`][`ggplot2`] function to overwrite the default theme.
#' @inheritParams elic_add_data
#'
#' @details
#' The `truth` argument is useful when the elicitation process is part of a
#' workshop and is used for demonstration. In this case the true value is known
#' and can be added to the plot. This argument must be a list with the following
#' elements: `min`, `max`, `best`, and `conf`. When `var` refers to a
#' _one point elicitation_ estimate, only the `best` element is required. When
#' `var` refers to a  _two points elicitation_.estimate, the `min` and `max`
#' elements are also required. Finally, when `var` refers to a
#' _three points elicitation_ estimate, the `conf` element is also required. The
#' `conf` element is used to rescale the `min` and `max` values.
#'
#' @return Invisibly a [`ggplot`][`ggplot2`] object.
#' @export
#'
#' @author Sergio Vignali and Stefano Canessa
#'
#' @examples
#' # Create the elict object and add data for the first and second round from a
#' # data.frame.
#' my_elicit <- elic_start(var_names = c("var1", "var2", "var3"),
#'                         var_types = "ZNp",
#'                         elic_types = "134",
#'                         experts = 6) |>
#'   elic_add_data(x, data_source = round_1, round = 1) |>
#'   elic_add_data(data_source = round_2, round = 2)
#'
#' # Plot the elicitation data for the first round and the variable var1 (only
#' # the best estimate)
#' plot.elicit(my_elicit, round = 1, var = "var1")
#'
#' # Plot the elicitation data for the first round and the variable var2 (best
#' # estimate with min and max errors)
#' plot.elicit(my_elicit, round = 1, var = "var2")
#'
#' # Plot the elicitation data for the first round and the variable var3 (best
#' # estimate with min and max errors rescaled to the confidence value)
#' plot.elicit(my_elicit, round = 1, var = "var3")
#'
#' # Add the group mean
#' plot.elicit(my_elicit, round = 1, var = "var3", group = TRUE)
#'
#' # Add the true value
#' plot.elicit(my_elicit, round = 1, var = "var3",
#'             truth = list(min = 0.6, max = 0.85, best = 0.75, conf = 80))
#'
#' # Overwrite the default theme
#' plot.elicit(my_elicit, round = 1, var = "var3",
#'             theme = ggplot2::theme_classic())
elic_plot <- function(x,
                      round,
                      var,
                      ...,
                      group = FALSE,
                      group_colour = "orange",
                      scale_conf = 100,
                      truth = NULL,
                      truth_colour = "red",
                      colour = "purple",
                      point_size = 4,
                      line_width = 1.5,
                      verbose = TRUE,
                      theme = NULL) {

  check_elicit(x)
  check_round(round)
  # TODO: it should check that only one variable is passed
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

  if (group) {

    group_data <- data.frame(id = "Group",
                             best = mean(data$best))

    p <- p +
      ggplot2::geom_point(data = group_data,
                          mapping = ggplot2::aes(x = .data$best,
                                                 y = .data$id),
                          colour = group_colour,
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

      var_min_scaled <- best - (best - data$min) * (scale_conf / (conf))
      var_max_scaled <- best + (data$max - best) * (scale_conf / (conf))

      data$min <- var_min_scaled
      data$max <- var_max_scaled

      if (!is.null(truth)) {
        truth_min_scaled <- truth$best - (truth$best - truth$min) *
          (scale_conf / (truth$conf))
        truth_max_scaled <- truth$best + (truth$max - truth$best) *
          (scale_conf / (truth$conf))
        truth$min <- truth_min_scaled
        truth$max <- truth_max_scaled
      }

      if (verbose) {
        cli::cli_alert_success("Rescaled min and max")
      }
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

    if (!is.null(truth)) {

      truth_data$min <- truth$min
      truth_data$max <- truth$max

      p <- p +
        ggplot2::geom_errorbarh(data = truth_data,
                                mapping = ggplot2::aes(y = .data$id,
                                                       xmin = .data$min,
                                                       xmax = .data$max),
                                colour = truth_colour,
                                position = "identity",
                                height = 0,
                                size = line_width)
    }

    if (group) {
      group_data$min <- mean(data$min)
      group_data$max <- mean(data$max)

      p <- p +
        ggplot2::geom_errorbarh(data = group_data,
                                mapping = ggplot2::aes(y = .data$id,
                                                       xmin = .data$min,
                                                       xmax = .data$max),
                                colour = group_colour,
                                position = "identity",
                                height = 0,
                                size = line_width)
    }
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
check_truth <- function(x, elic_type) {

  n <- length(x)
  error <- ""

  if (!is.list(x)) {
    error <- "{.arg truth} is a {.cls {class(x)}} but it should a named \\
              {.cls list}."
  } else {

    if (elic_type == "1p") {

      if (n != 1) {
        error <- "{.arg truth} is a list with {.val {n}} elements but it \\
                  should have only {.val {1}} element."
      } else if (names(x) != "best") {
        error <- "{.arg truth} is a list with an element named \\
                  {.val {names(x)}} but its name should be {.val best}."
      }

    } else if (elic_type == "3p") {

      if (n != 3) {
        error <- "{.arg truth} is a list with {.val {n}} elements but it \\
                  should have {.val {3}} elements."
      } else if (!all(c("min", "max", "best") %in% names(x))) {
        error <- "{.arg truth} is a list with elements named \\
                  {.val {names(x)}} but it should have elements named \\
                  {.val min}, {.val max} and {.val best}."
      }

    } else if (elic_type == "4p") {

      if (n != 4) {
        error <- "{.arg truth} is a list with {.val {n}} elements but it \\
                  should have {.val {4}} elements."
      } else if (!all(c("min", "max", "best", "conf") %in% names(x))) {
        error <- "{.arg truth} is a list with elements named \\
                  {.val {names(x)}} but it should have elements named \\
                  {.val min}, {.val max}, {.val best} and {.val conf}."
      }
    }
  }

  if (nchar(error) > 0) {

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
