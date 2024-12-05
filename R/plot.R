#' Plot elicitation data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `elic_plot()` plots elicitation data for a specific round and variable.
#'
#' @param var character string, the variable to be plotted.
#' @param xlab character, the title of the x axis
#' @param ylab character, the title of the y axis
#' @param title character, the title of the plot
#' @param scale_conf numeric, the scale factor for the confidence interval.
#' @param group logical, whether to plot the group mean.
#' @param truth list, the true value of the variable, see Details for more.
#' @param colour character string, the colour of estimated values.
#' @param group_colour character string, the colour of the group mean.
#' @param truth_colour character string, the colour of the true value.
#' @param point_size numeric, the size of the points.
#' @param line_width numeric, the width of the lines.
#' @param family character, the font family.
#' @param theme a [`theme`][`ggplot2::theme`] function to overwrite the default
#' theme.
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
#' @return Invisibly a [`ggplot`][`ggplot2::ggplot`] object.
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
#' elic_plot(my_elicit, round = 1, var = "var1")
#'
#' # Plot the elicitation data for the first round and the variable var2 (best
#' # estimate with min and max errors)
#' elic_plot(my_elicit, round = 1, var = "var2")
#'
#' # Plot the elicitation data for the first round and the variable var3 (best
#' # estimate with min and max errors rescaled to the confidence value)
#' elic_plot(my_elicit, round = 1, var = "var3")
#'
#' # Add the group mean
#' elic_plot(my_elicit, round = 1, var = "var3", group = TRUE)
#'
#' # Add the true value
#' elic_plot(my_elicit, round = 1, var = "var3",
#'           truth = list(min = 0.6, max = 0.85, best = 0.75, conf = 100))
#'
#' # Overwrite the default theme
#' elic_plot(my_elicit, round = 1, var = "var3",
#'           theme = ggplot2::theme_classic())
elic_plot <- function(x,
                      round,
                      var,
                      ...,
                      xlab = var,
                      ylab = "Expert",
                      title = paste("Round", round),
                      scale_conf = 100,
                      group = FALSE,
                      truth = NULL,
                      colour = "purple",
                      group_colour = "orange",
                      truth_colour = "red",
                      point_size = 4,
                      line_width = 1.5,
                      family = "sans",
                      theme = NULL,
                      verbose = TRUE) {

  # Check arguments
  check_elicit(x)
  check_round(round)
  check_var_in_obj(x, var)

  if (is.null(theme)) {
    theme <- elic_theme(family = family)
  }

  data <- elic_get_data(x, round = round, var = var) |>
    dplyr::filter(!dplyr::if_all(dplyr::everything(), is.na)) |>
    dplyr::mutate(col = "experts")
  colnames(data) <- gsub(paste0(var, "_"), "", colnames(data))

  ids <- data |>
    dplyr::pull(.data[["id"]])
  elic_type <- get_type(x, var, "elic")
  var_type <- get_type(x, var, "var")
  idx <- seq_len(x[["experts"]])

  if (group) {

    ids <- c(ids, "Group")
    data <- add_group_data(data, elic_type)
  }

  if (!is.null(truth)) {

    ids <- c(ids, "Truth")
    data <- add_truth_data(data, truth, elic_type)
  }

  data <- data |>
    mutate("id" = factor(.data[["id"]], levels = ids))

  if (elic_type %in% c("3p", "4p")) {

    if (elic_type == "4p") {

      # Rescale min and max
      data <- rescale_data(data, scale_conf)
      needs_resc <- any(data[["min"]][idx] < 0) || any(data[["max"]][idx] > 1)

      if (var_type == "p" && needs_resc) {

        data[["min"]] <- pmax(0, pmin(1, data[["min"]]))
        data[["max"]] <- pmax(0, pmin(1, data[["max"]]))
        warn <- "Some values have been constrained to be between {.val {0}} \\
                 and {.val {1}}."
        cli::cli_warn(c("!" = warn))
      }

      if (verbose) {
        cli::cli_alert_success("Rescaled min and max")
      }
    }

    if (group) {
      data[["min"]][data[["id"]] == "Group"] <- mean(data[["min"]][idx],
                                                     na.rm = TRUE)
      data[["max"]][data[["id"]] == "Group"] <- mean(data[["max"]][idx],
                                                     na.rm = TRUE)
    }
  }

  p <- ggplot2::ggplot(data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = .data[["best"]],
                                               y = .data[["id"]],
                                               colour = .data[["col"]]),
                        size = point_size) +
    ggplot2::labs(title = title,
                  x = xlab,
                  y = ylab)

  if (elic_type %in% c("3p", "4p")) {
    p <- p +
      ggplot2::geom_errorbarh(mapping = ggplot2::aes(y = .data[["id"]],
                                                     xmin = .data[["min"]],
                                                     xmax = .data[["max"]],
                                                     colour = .data[["col"]]),
                              position = "identity",
                              height = 0,
                              linewidth = line_width)
  }

  if (var_type == "p") {
    p <- p +
      ggplot2::scale_x_continuous(limits = c(0, 1),
                                  expand = c(0, 0))
  }

  p +
    ggplot2::scale_colour_manual(values = c("experts" = colour,
                                            "group" = group_colour,
                                            "truth" = truth_colour)) +
    theme
}

# Helpers----

#' Add group data
#'
#' Add data for the group mean to the data.frame.
#'
#' @param data tibble with the elicitation data.
#' @param elic_type character string with the elicitation type.
#'
#' @return A tibble with the group data added.
#' @noRd
#'
#' @author Sergio Vignali
add_group_data <- function(data, elic_type) {

  if (elic_type == "1p") {
    data <- data |>
      dplyr::add_row("id" = "Group",
                     "best" = mean(data[["best"]], na.rm = TRUE),
                     "col" = "group")
  } else if (elic_type == "3p") {
    data <- data |>
      dplyr::add_row("id" = "Group",
                     "best" = mean(data[["best"]], na.rm = TRUE),
                     "min" = NA,
                     "max" = NA,
                     "col" = "group")
  } else {
    data <- data |>
      dplyr::add_row("id" = "Group",
                     "best" = mean(data[["best"]], na.rm = TRUE),
                     "min" = NA,
                     "max" = NA,
                     "conf" = 100,
                     "col" = "group")
  }

  data
}

#' Add truth data
#'
#' Add data for the true values to the data.frame.
#'
#' @param data tibble with the elicitation data.
#' @param truth list with the true values.
#' @param elic_type character string with the elicitation type.
#'
#' @return A tibble with the true data added.
#' @noRd
#'
#' @author Sergio Vignali
add_truth_data <- function(data, truth, elic_type) {

  if (elic_type == "1p") {
    data <- data |>
      dplyr::add_row("id" = "Truth",
                     "best" = truth[["best"]],
                     "col" = "truth")
  } else if (elic_type == "3p") {
    data <- data |>
      dplyr::add_row("id" = "Truth",
                     "best" = truth[["best"]],
                     "min" = truth[["min"]],
                     "max" = truth[["max"]],
                     "col" = "truth")
  } else if (elic_type == "4p") {
    data <- data |>
      dplyr::add_row("id" = "Truth",
                     "best" = truth[["best"]],
                     "min" = truth[["min"]],
                     "max" = truth[["max"]],
                     "conf" = truth[["conf"]],
                     "col" = "truth")
  }

  data
}

#' Get type
#'
#' Get variable or elicitation type for the given variable.
#'
#' @param x an object of class `elicit`.
#' @param var character string with the variable name.
#' @param type character string, either `var` or `elic`.
#'
#' @return A character string with the variable or elicitation type.
#' @noRd
#'
#' @author Sergio Vignali
get_type <- function(x, var, type) {
  x[[paste0(type, "_types")]][x[["var_names"]] == var]
}

#' Rescale data
#'
#' Rescale the min and max values of the data to the confidence value.
#'
#' @param x a data.frame with the elicitation data.
#' @param s numeric, the scale factor for the confidence interval.
#'
#' @return A data.frame with the rescaled min and max values.
#' @noRd
#'
#' @author Sergio Vignali and Stefano Canessa
rescale_data <- function(x, s) {

  x[["min"]] <- x[["best"]] - (x[["best"]] - x[["min"]]) * s / x[["conf"]]
  x[["max"]] <- x[["best"]] + (x[["max"]] - x[["best"]]) * s / x[["conf"]]

  x
}

# Checkers----

#' Check variable in object
#'
#' Check if the variable is present in the `elicit` object and if it is of
#' length 1.
#'
#' @param x an object of class `elicit`.
#' @param var character to check.
#'
#' @return An error if the variable is not present in the object or if it is
#' not of length 1.
#' @noRd
#'
#' @author Sergio Vignali
check_var_in_obj <- function(x, var) {

  if (length(var) > 1) {
    error <- "Only one variable can be plotted at a time, you passed \\
              {.val {length(var)}} variables."
    cli::cli_abort(c("Incorrect value for {.arg var}:",
                     "x" = error,
                     "i" = "See {.fn elicitr::plot.elicit}."),
                   call = rlang::caller_env())
  }

  if (!var %in% x[["var_names"]]) {
    error <- "Variable {.val {var}} not found in the {.cls elicit} object."
    cli::cli_abort(c("Invalid value for {.arg var}:",
                     "x" = error,
                     "i" = "Available variables are {.val {x$var_names}}."),
                   call = rlang::caller_env())
  }
}

#' Check truth argument
#'
#' Checks if the `truth` argument is a list with the correct elements for the
#' given elicitation type.
#'
#' @param x the value to be checked.
#' @param elic_type character string with the elicitation type.
#'
#' @return An error if `truth` is not a list or if it does not have the correct
#' elements.
#' @noRd
#'
#' @author Sergio Vignali
check_truth <- function(x, elic_type) {

  n <- length(x)
  error <- ""

  if (is.list(x)) {

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
  } else {
    error <- "{.arg truth} is a {.cls {class(x)}} but it should a named \\
              {.cls list}."
  }

  if (nchar(error) > 0) {

    cli::cli_abort(c("Incorrect value for {.arg truth}:",
                     "x" = error,
                     "i" = "See {.fn elicitr::plot.elicit}"),
                   call = rlang::caller_env())
  }
}

# Plot theme----


#' Elic theme
#'
#' Custom theme for elicitation plots.
#'
#' @return A [`theme`][`ggplot2::theme`] function.
#' @noRd
#'
#' @author Sergio Vignali
elic_theme <- function(family = "sans") {
  ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(size = 16,
                                                      face = "bold",
                                                      hjust = 0.5,
                                                      family = family),
                   panel.grid.major.x = ggplot2::element_line(colour = "black",
                                                              linetype = 8,
                                                              linewidth = 0.1),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_text(size = 16,
                                                        color = "black",
                                                        family = family),
                   axis.title.x = ggplot2::element_text(vjust = -1.2,
                                                        size = 16,
                                                        color = "black",
                                                        family = family),
                   axis.ticks.length = ggplot2::unit(0.5, units = "mm"),
                   axis.text = ggplot2::element_text(size = 14,
                                                     family = family),
                   plot.margin = ggplot2::unit(c(5, 10, 5, 5), units = "mm"))
}
