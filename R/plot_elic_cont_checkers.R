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
#' @author Sergio Vignali and Maude Vernet
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
#' @author Sergio Vignali and Maude Vernet
check_truth <- function(x, elic_type, miss_conf) {

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
        error <- "The name of the elements in {.arg truth} should be \\
                  {.val min}, {.val max}, and {.val best} and not \\
                  {.val {names(x)}}."
      }

    } else if (elic_type == "4p") {

      if (isTRUE(miss_conf)) {
        n <- n - 1
      }

      if (!n %in% c(3, 4)) {
        error <- "Argument {.arg truth} is a list with {.val {n}} elements \\
                  but should have {.val {3}} or {.val {4}} elements named \
                  {.val min}, {.val max}, {.val best} and optionally \
                  {.val conf}."
      } else if (!all(c("min", "max", "best", "conf") %in% names(x))) {
        error <- "The name of the elements in {.arg truth} should be \\
                  {.val min}, {.val max}, {.val best}, and optionally \\
                  {.val conf} and not {.val {names(x)}}."
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
                   call = rlang::caller_env(n = 2))
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

#' Finish plot with depended variables
#'
#' Add group error bars to group value if plotting a 1-point elicitation, add
#' error bars if plotting a 3 or 4-point, and rescale plot if probability values
#'
#' @param p a ggplot object.
#' @param elic_type character string with the elicitation type.
#' @param var_type character string with the variable type.
#' @param line_width numeric string with the width of the line.
#' @param data_ci table with the 95\% CI around the mean of 1-point elicitations
#'
#' @return a ggplot object.
#' @noRd
#'
#' @author Maude Vernet
plot_addons <- function(p,
                        elic_type,
                        var_type,
                        group,
                        line_width,
                        data_ci) {
  if (elic_type == "1p" && group) {
    p <- p +
      ggplot2::geom_errorbar(data = data_ci,
                             mapping =
                               ggplot2::aes(y = .data[["id"]],
                                            xmin = .data[["group95ciNEG"]],
                                            xmax = .data[["group95ciPOS"]],
                                            colour = .data[["col"]]),
                             position = "identity",
                             width = 0,
                             linewidth = line_width)
  }

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

  p
}

#' Verify if group or truth are called
#'
#' Check if group or truth are called and make all necessary checks.
#'
#' @param group logical if group should be plotted.
#' @param ids the names of experts
#' @param elic_type character string with the elicitation type.
#' @param data tibble with the elicitation data.
#' @param truth logical if truth should be plotted.
#'
#' @return a list with the object with added group and truth, thenew ids and the
#' potential CIs.
#' @noRd
#'
#' @author Maude Vernet
check_grouptruth <- function(group, ids, elic_type, data, truth) {
  data_ci <- NULL

  if (group) {

    ids <- c(ids, "Group")
    if (elic_type == "1p") {
      data_ci <- add_ci(data, elic_type)
    }
    data <- add_group_data(data, elic_type)
  }

  if (!is.null(truth)) {

    ids <- c(ids, "Truth")
    miss_conf <- FALSE
    if (elic_type == "4p" && !"conf" %in% names(truth)) {
      truth$conf <- 100
      miss_conf <- TRUE
    }

    check_truth(truth, elic_type, miss_conf)
    data <- add_truth_data(data, truth, elic_type)
  }
  list(data_ci = data_ci,
       data = data,
       ids = ids)
}
