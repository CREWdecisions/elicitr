#' Plot elicitation data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' `plot()` plots elicitation data for a specific round and variable.
#'
#' @param var character string, the variable to be plotted.
#' @param scale_conf numeric, the scale factor for the confidence interval.
#' @param group logical, whether to plot the group mean, see Details for more.
#' @param truth list, the true value of the variable, see Details for more.
#' @param colour character string, the colour of estimated values.
#' @param group_colour character string, the colour of the group mean.
#' @param truth_colour character string, the colour of the true value.
#' @param point_size numeric, the size of the points.
#' @param line_width numeric, the width of the lines.
#' @param title character, the title of the plot.
#' @param xlab character, the title of the x axis.
#' @param ylab character, the title of the y axis.
#' @param expert_names numeric or character, the labels for the experts.
#' @param family character, the font family.
#' @param theme a [theme object][ggplot2::theme] to use in the plot,
#' such as `ggplot2::theme_minimal()`.
#' theme.
#' @inheritParams cont_add_data
#'
#' @section scale_conf:
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
#' For probability variables (`var_types = "p"`), confidence scaling may
#' extend the estimated minimum and maximum value below zero or above one.
#' To keep the interval within the valid probability range, any scaled minimum
#' below zero is set to zero, and any scaled maximum above one is set to one.
#' A warning is issued when this adjustment is needed.
#'
#' The best estimate and the original responses stored in the elicitation object
#' remain unchanged. The adjusted bounds are used to define the displayed
#' intervals when plotting.
#'
#' @details
#' If a one-point elicitation is plotted, the `group` argument will show the
#' mean and 2.5th–97.5th percentile interval of expert estimates.
#'
#' The `truth` argument is useful when the elicitation process is part of a
#' workshop and is used for demonstration. In this case the true value is known
#' and can be added to the plot. This argument must be a list with the following
#' elements: `min`, `max`, `best`, and `conf`. When `var` refers to a
#' _one point elicitation_ estimate, only the `best` element is required. When
#' `var` refers to a  _three points elicitation_.estimate, the `min` and `max`
#' elements are also required. Finally, when `var` refers to a
#' _four points elicitation_ estimate, the `conf` element is also required. The
#' `conf` element is used to rescale the `min` and `max` values.
#'
#' If a `theme` is provided, the `family` argument is ignored.
#'
#' @return A [`ggplot`][`ggplot2::ggplot`] object.
#' @export
#'
#' @family plot helpers
#'
#' @author Sergio Vignali, Maude Vernet and Stefano Canessa
#'
#' @examples
#' # Create the elict object and add data for the first and second round from a
#' # data.frame.
#' my_elicit <- cont_start(var_names = c("var1", "var2", "var3"),
#'                         var_types = "ZNp",
#'                         elic_types = "134",
#'                         experts = 6) |>
#'   cont_add_data(data_source = round_1, round = 1) |>
#'   cont_add_data(data_source = round_2, round = 2)
#'
#' # Plot the elicitation data for the first round and the variable var1 (only
#' # the best estimate)
#' plot(my_elicit, round = 1, var = "var1")
#'
#' # Plot the elicitation data for the first round and the variable var2 (best
#' # estimate with min and max errors)
#' plot(my_elicit, round = 1, var = "var2")
#'
#' # Plot the elicitation data for the first round and the variable var3 (best
#' # estimate with min and max errors rescaled to the confidence value)
#' plot(my_elicit, round = 1, var = "var3")
#'
#' # Add the group mean
#' plot(my_elicit, round = 1, var = "var3", group = TRUE)
#'
#' # Add the true value
#' plot(my_elicit, round = 1, var = "var3",
#'           truth = list(min = 0.6, max = 0.85, best = 0.75, conf = 100))
#'
#' # Overwrite the default theme
#' plot(my_elicit, round = 1, var = "var3",
#'           theme = ggplot2::theme_classic())
plot.elic_cont <- function(x,
                           round,
                           var,
                           ...,
                           scale_conf = 100,
                           group = FALSE,
                           truth = NULL,
                           colour = "purple",
                           group_colour = "orange",
                           truth_colour = "red",
                           point_size = 4,
                           line_width = 1.5,
                           title = paste("Round", round),
                           xlab = var,
                           ylab = "Experts",
                           family = "sans",
                           expert_names = NULL,
                           theme = NULL,
                           verbose = TRUE) {

  # Check arguments
  check_round(round)
  check_var_in_obj(x, var)

  if (is.null(theme)) {
    theme <- elic_theme(family = family) +
      cont_theme()
  }

  data <- cont_get_data(x, round = round, var = var) |>
    dplyr::filter(!dplyr::if_all(dplyr::everything(), is.na)) |>
    dplyr::mutate(col = "experts")
  prefix <- paste0(var, "_")
  cols <- names(data)

  idx <- startsWith(cols, prefix) & cols != "id"
  cols[idx] <- substring(cols[idx], nchar(prefix) + 1L)

  names(data) <- cols

  if (!is.null(expert_names)) {
    data <- cont_rename_experts(x,
                                data,
                                expert_names)
  }

  ids <- dplyr::pull(data,
                     .data[["id"]])
  elic_type <- get_type(x, var, "elic")
  var_type <- get_type(x, var, "var")
  idx <- seq_len(x[["experts"]])

  grouptruth <- check_grouptruth(group, ids, elic_type, data, truth)

  data <- grouptruth$data
  ids <- grouptruth$ids
  data_ci <- grouptruth$data_ci

  data <- data |>
    mutate("id" = factor(.data[["id"]], levels = ids))



  if (elic_type == "4p") {

    # Rescale min and max
    data <- rescale_data(data, scale_conf)

    needs_resc <- check_resc_na(data = data, idx = idx)

    if (var_type == "p" && needs_resc) {

      data[["min"]] <- pmax(0, pmin(1, data[["min"]]))
      data[["max"]] <- pmax(0, pmin(1, data[["max"]]))
      fn <- as.list(sys.call())[[1]]
      warn <- "Some values have been constrained to be between {.val {0}} \\
                 and {.val {1}}."
      cli::cli_warn(c("!" = warn,
                      " " = "See the {.field scale_conf} section in the \\
                             help page for {.fn elicitr::{fn}} for more \\
                             information."))
    }

    if (verbose) {
      cli::cli_alert_success("Rescaled min and max based on confidence \\
                              estimates")
    }
  }
  if (elic_type %in% c("3p", "4p") && group) {
    data[["min"]][data[["id"]] == "Group"] <- mean(data[["min"]][idx],
                                                   na.rm = TRUE)
    data[["max"]][data[["id"]] == "Group"] <- mean(data[["max"]][idx],
                                                   na.rm = TRUE)
  }

  p <- ggplot2::ggplot(data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = .data[["best"]],
                                               y = .data[["id"]],
                                               colour = .data[["col"]]),
                        size = point_size) +
    ggplot2::labs(title = title,
                  x = xlab,
                  y = ylab)

  p <- plot_addons(p, elic_type, var_type, group, line_width, data_ci)

  p +
    ggplot2::scale_colour_manual(values = c("experts" = colour,
                                            "group" = group_colour,
                                            "truth" = truth_colour)) +
    theme
}
