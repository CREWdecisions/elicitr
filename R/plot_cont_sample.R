#' Plot continuous samples
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' `plot()` aggregates and plots continuous samples as violin or density plot.
#'
#' @inheritParams cont_get_data
#' @param x n object of class `cont_sample` created by the function
#' [cont_sample_data].
#' @param var character string with the name of the variable to be plotted.
#' @param group logical, if `TRUE` data are aggregated by expert.
#' @param type character string with the type of plot, either _beeswarm_ or
#' _violin_ or _density_.
#' @param title character string with the title of the plot.
#' @param xlab character string with the x-axis label.
#' @param ylab character string with the y-axis label.
#' @param colours character vector with the colours to be used in the plot.
#' @param line_width numeric with the width of the lines in the density plot.
#' @param family character string with the font family to be used in the plot.
#' @param expert_names numeric or character, the labels for the experts.
#' @param theme a [theme object][ggplot2::theme] to use in the plot,
#' such as `ggplot2::theme_minimal()`.
#' @param beeswarm_cex numeric, the space between points in the beeswarm plot.
#' @param beeswarm_corral character string, the wrapping corral for the beeswarm
#' plot. Anything accepted by the [geom_beeswarm][ggbeeswarm::geom_beeswarm]
#' function.
#'
#'
#' @details If a `theme` is provided, the `family` argument is ignored.
#'
#' @returns A [`ggplot`][`ggplot2::ggplot`] object.
#' @export
#'
#' @family plot helpers
#'
#' @author Sergio Vignali and Maude Vernet
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
#' # Sample data for the first round for all variables
#' samp <- cont_sample_data(my_elicit, round = 1)
#'
#' # Plot the sampled data for the variable `var3` as violin plot
#' plot(samp, var = "var3", type = "violin")
#'
#' # Plot the sampled data for the variable `var1` as beeswarm plot
#' plot(samp, var = "var1", type = "beeswarm")
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
                             var = NULL,
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
                             theme = NULL,
                             beeswarm_cex = 0.6,
                             beeswarm_corral = "none") {

  check_plot_type(type, elic = "cont")

  if (!is.null(expert_names)) {
    x <- cont_rename_experts(x = x,
                             data = x,
                             expert_names)
  }

  if (length(unique(x[["var"]])) == 1) {
    var <- unique(x[["var"]])
  }

  # Check if var is available
  check_length(var, "var", 1)
  check_var_in_sample(x, var)
  type <- update_type(type, x, var, group)

  x[["missing"]] <- NA
  x[["violin_value"]] <- x[["value"]]
  mean_value <- mean(x[["violin_value"]][x[["var"]] == var], na.rm = TRUE)
  if (anyNA(x[["value"]])) {
    x[["missing"]][is.na(x[["value"]])] <- "no data"
    x[["violin_value"]][is.na(x[["violin_value"]])] <- mean_value
  }

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
  } else if (length(colours) != n) {

    fn <- as.list(sys.call())[[1]]
    cli::cli_abort(c("Invalid value for argument {.arg colours}:",
                     "x" = error,
                     "i" = info,
                     " " = "See {.fn elicitr::{fn}} for more information."))
  }

  if (is.null(theme)) {
    theme <- elic_theme(family = family) +
      cont_sample_theme(type = type, group = group)
  }

  if (all(is.na(x[["value"]][x[["var"]] == var]))) {
    text <- "Experts did not provide any estimate for variable {.val {var}}."
    error <- "Use {.fn elicitr::cont_get_data} with {.arg var} = \\
                {.val {var}} to see the data in the \\
                {.help elicitr::elic_cont} object."
    info <- "If the resulting data is also only NA, review raw data. \\
             Otherwise review the object parsed in {.arg x}}."
    fn <- as.list(sys.call())[[1]]
    cli::cli_abort(c(text,
                     "x" = error,
                     "i" = info,
                     " " = "See {.fn elicitr::{fn}} for more information."))
  }

  p <- make_base_plot(type, x, x_var, line_width, beeswarm_cex, beeswarm_corral)

  subtitle <- ggplot2::waiver()
  if (!all(is.na(x[["missing"]])) && !isTRUE(group)) {
    if (type %in% c("violin", "beeswarm")) {
      p <- p + ggplot2::geom_label(aes(x = .data[[x_var]],
                                       y = .data[["violin_value"]],
                                       label = .data[["missing"]]),
                                   na.rm = TRUE)
    }
    n_na <- length(x[["id"]][is.na(x[["value"]])])
    experts_na <- paste0(x[["id"]][is.na(x[["value"]])], " ")
    ifelse(length(x[["id"]][is.na(x[["value"]])]) > 1,
           subtitle <- paste("No answer from expert",
                             toString(x[["id"]][is.na(x[["value"]])][2:n_na]),
                             "and",
                             x[["id"]][is.na(x[["value"]])][1],
                             sep = " "),
           subtitle <- paste0("No answer from expert ",
                              x[["id"]][is.na(x[["value"]])]))
  }

  p <- p +
    ggplot2::labs(title = title,
                  subtitle = subtitle,
                  x = xlab,
                  y = ylab)

  if (type == "violin") {
    p <- p + ggplot2::scale_fill_manual(values = colours)
  } else {
    p <- p + ggplot2::scale_colour_manual(values = colours)
  }

  p <- p + theme

  p
}
