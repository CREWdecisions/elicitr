#' Plot elicitation data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `plot()` plots elicitation data for a specific round and variable.
#'
#' @param var character string, the variable to be plotted.
#' @param scale_conf numeric, the scale factor for the confidence interval.
#' @param group logical, whether to plot the group mean.
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
#' @param theme a [`theme`][`ggplot2::theme`] function to overwrite the default
#' theme.
#' @inheritParams cont_add_data
#'
#' @details
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
#' @return Invisibly a [`ggplot`][`ggplot2::ggplot`] object.
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
#'   cont_add_data(x, data_source = round_1, round = 1) |>
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
  colnames(data) <- gsub(paste0(var, "_"), "", colnames(data))

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

  if (group) {

    ids <- c(ids, "Group")
    data <- add_group_data(data, elic_type)
  }

  if (!is.null(truth)) {

    ids <- c(ids, "Truth")
    check_truth(truth, elic_type)
    data <- add_truth_data(data, truth, elic_type)
  }

  data <- data |>
    mutate("id" = factor(.data[["id"]], levels = ids))

  if (elic_type %in% c("3p", "4p")) {

    if (elic_type == "4p") {

      # Rescale min and max
      data <- rescale_data(data, scale_conf)

      needs_resc <- check_resc_na(data = data, idx = idx)

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
      ggplot2::geom_errorbar(mapping = ggplot2::aes(y = .data[["id"]],
                                                    xmin = .data[["min"]],
                                                    xmax = .data[["max"]],
                                                    colour = .data[["col"]]),
                             position = "identity",
                             width = 0,
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
#' @author Sergio Vignali and Maude Vernet
add_truth_data <- function(data, truth, elic_type) {

  if (elic_type == "1p") {
    data <- dplyr::add_row(data,
                           "id" = "Truth",
                           "best" = truth[["best"]],
                           "col" = "truth")
  } else if (elic_type == "3p") {
    data <- dplyr::add_row(data,
                           "id" = "Truth",
                           "best" = truth[["best"]],
                           "min" = truth[["min"]],
                           "max" = truth[["max"]],
                           "col" = "truth")
  } else if (elic_type == "4p") {
    data <- dplyr::add_row(data,
                           "id" = "Truth",
                           "best" = truth[["best"]],
                           "min" = truth[["min"]],
                           "max" = truth[["max"]],
                           "conf" = truth[["conf"]],
                           "col" = "truth")
  }

  data
}

# Checkers----

#' Check variable in object
#'
#' Check if the variable is present in the `elic_cont` object and if it is of
#' length 1.
#'
#' @param x an object of class `elic_cont`.
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
                     "i" = "See {.fn elicitr::plot.elic_cont}."),
                   call = rlang::caller_env())
  }

  if (!var %in% x[["var_names"]]) {
    error <- "Variable {.val {var}} not found in the {.cls elic_cont} object."
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

    info <- "See Details in {.fn elicitr::plot.elic_cont}."

    if (elic_type == "1p") {

      if (n != 1) {
        error <- "Argument {.arg truth} is a list with {.val {n}} elements \\
                  but it should have only {.val {1}} element named {.val best}."
      } else if (names(x) != "best") {
        error <- "The name of the element in {.arg truth} should be \\
                  {.val best} and not {.val {names(x)}}."
      }

    } else if (elic_type == "3p") {

      if (n != 3) {
        error <- "Argument {.arg truth} is a list with {.val {n}} elements \\
                  but should have {.val {3}} elements named {.val min}, \
                  {.val max} and {.val best}."
      } else if (!all(c("min", "max", "best") %in% names(x))) {
        error <- "The name of the element in {.arg truth} should be \\
                  {.val min}, {.val max}, and {.val best} and not \\
                  {.val {names(x)}}."
      }

    } else if (elic_type == "4p") {

      if (n != 4) {
        error <- "Argument {.arg truth} is a list with {.val {n}} elements \\
                  but should have {.val {4}} elements named {.val min}, \
                  {.val max}, {.val best} and {.val conf}."
      } else if (!all(c("min", "max", "best", "conf") %in% names(x))) {
        error <- "The name of the element in {.arg truth} should be \\
                  {.val min}, {.val max}, {.val best}, and {.val conf} and \\
                  not {.val {names(x)}}."
      }
    }
  } else {
    error <- "Argument {.arg truth} is of class {.cls {class(x)}} but it \\
              should be a named {.cls list}."
    info <- "See {.fn elicitr::plot.elic_cont}."
  }

  if (nzchar(error, keepNA = TRUE)) {

    cli::cli_abort(c("Incorrect value for {.arg truth}:",
                     "x" = error,
                     "i" = info),
                   call = rlang::caller_env())
  }
}

# Plot theme----

#'Theme
#'
#' Custom theme for continuous data.
#'
#' @return A [`theme`][`ggplot2::theme`] function.
#' @noRd
#'
#' @author Sergio Vignali and Maude Vernet
cont_theme <- function() {
  ggplot2::theme(panel.grid.major.x = ggplot2::element_line(colour = "black",
                                                            linetype = 8,
                                                            linewidth = 0.1),
                 panel.grid.major.y = ggplot2::element_blank(),
                 axis.ticks.length = ggplot2::unit(0.5, units = "mm"),
                 legend.position = "none")
}

# Rename experts----

#' Rename experts
#' Check if the new names vector is of the correct length
#' Rename experts in elic_cont object
#' @param x the object to be plotted
#' @param data tibble with the elicitation data
#' @param expert_names character vector with the new names for the experts
#'
#' @return the object with renamed experts
#' @noRd
#'
#' @author Maude Vernet
cont_rename_experts <- function(x,
                                data,
                                expert_names) {

  n_experts <- length(unique(data[["id"]]))

  if (length(expert_names) != n_experts) {
    error <- "You provided {.val {length(expert_names)}} expert names but the \\
              elicitation process has {.val {n_experts}} experts."
    cli::cli_abort(c("Incorrect length of {.arg expert_names}:",
                     "x" = error,
                     "i" = "Provide a vector of length {.val {n_experts}}."),
                   call = rlang::caller_env())
  }

  if (anyDuplicated(expert_names) > 0) {
    error <- "Multiple experts have the same name in {.arg expert_names}."
    cli::cli_abort(c("Invalid value for {.arg expert_names}:",
                     "x" = error,
                     "i" = "Please provide unique names for each expert."),
                   call = rlang::caller_env())
  }

  if (any(expert_names == "Group") || any(expert_names == "Truth")) {
    error <- "The names {.val Group} and {.val Truth} are reserved and cannot \\
              be used in {.arg expert_names}."
    cli::cli_abort(c("Invalid value for {.arg expert_names}:",
                     "x" = error,
                     "i" = "Please provide different names for the experts."),
                   call = rlang::caller_env())
  }

  if (class(x)[1] == "elic_cont") {
    expert_names <- as.character(expert_names)
    data[["id"]] <- expert_names
  }

  if (class(data)[1] == "cont_sample") {
    names(expert_names) <- unique(data$id)
    data$id <- expert_names[data$id]
    data
  }

  data
}

#' Check if rescaling needed and if NA present
#'
#' @param data the data to be checked.
#' @param idx the indices of the experts.
#'
#' @returns a logical indicating if rescaling is needed.
#' @noRd
#'
#' @author Maude Vernet
check_resc_na <- function(data, idx) {
  if (anyNA(data)) {
    has_na <- which(rowSums(is.na(data)) > 0)
    data_na <- data[-has_na, ]
    idx_na <- seq_len(nrow(data_na))
    needs_resc <-
      any(data_na[["min"]][idx_na] < 0) || any(data_na[["max"]][idx_na] > 1)

  } else {
    needs_resc <- any(data[["min"]][idx] < 0) || any(data[["max"]][idx] > 1)
  }
  needs_resc
}
