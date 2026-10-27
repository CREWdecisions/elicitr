#' Fix variable order
#'
#' Sometimes values are not in the correct order _min_ _max_ _best_. This
#' function checks the order for each variable and row that are a three or four
#' points estimation and, if necessary, reorders the values. It also raises a
#' warn with information on which variable/s and row/s that has/have been
#' reordered.
#'
#' @param x tibble with the data.
#' @param var_names character vector with the variable names.
#' @param elic_types character string with the elicitation types.
#'
#' @return The reordered tibble. Also a warn with information on which
#' variable/s and row/s that has/have been reordered.
#' @noRd
#'
#' @author Sergio Vignali
fix_var_order <- function(x,
                          var_names,
                          elic_types) {

  # Only 3p and 4p elicitation types should be checked
  idx_vars <- elic_types != "1p"
  vars <- var_names[idx_vars]

  for (i in seq_along(vars)) {
    expected_cols <- paste0(vars[i], c("_min", "_max", "_best"))
    idx_cols <- names(x) %in% expected_cols

    idx_rows <- apply(x[, idx_cols], 1, is_not_min_max_best)

    if (sum(idx_rows, na.rm = TRUE) > 0) {
      x[, idx_cols] <- apply(x[, idx_cols], 1, min_max_best) |>
        t()

      ids <- dplyr::pull(x, 1)[which(idx_rows)]

      warn <- "{.field {vars[i]}} of {.cls id} {.val {ids}} reordered \\
                following the order {.val min-max-best}."
      info <- "Check raw data and if you want to update the dataset use
               {.fn elicitr::cont_add_data} with {.code overwrite = TRUE}."

      cli::cli_warn(c("!" = warn))
    }
  }
  x
}

#' Min max best
#'
#' Given a vector of three values, the function returns the same vector
#' reordered following the sequence min-max-best.
#'
#' @param x numeric vector of three elements with the values to be reordered.
#'
#' @return A vector with the reordered values.
#' @noRd
#'
#' @author Sergio Vignali
min_max_best <- function(x) {
  c(min(x), max(x), stats::median(x))
}

#' Is min max best
#'
#' The function checks if the elements of a vector are in the order
#' min-max-best.
#'
#' @param x numeric vector of three elements to be checked.
#'
#' @return A logical value with `TRUE` if the elements of the vector are in the
#' order min-max-best, `FALSE` otherwise.
#' @noRd
#'
#' @author Sergio Vignali
is_not_min_max_best <- function(x) {
  !all(x == min_max_best(x))
}
