#' elic_cat class
#'
#' The `elic_cat` class is a container for the elicitation data. It is used to
#' store the data collected during the elicitation process and their metadata.
#'
#' @section Object elements:
#'
#' There are 6 elements in the `elic_cat` object:
#'
#' * `levels`: character vector with the names of the levels of impact.
#' * `sites`: character vector with the names of the sites investigated.
#' * `experts`: numeric, indicating the maximum number of experts participating
#' in the elicitation process for one mechanism.
#' * `mechanisms`: character vector with the names of the mechanisms of impact
#' investigated.
#' * `data`: list with the data collected during the elicitation process. The
#' list has multiple elements, corresponding to the mechanisms of impact
#' investigated.
#'
#' Moreover, the object has a `title` attribute that binds a name to the object.
#'
#' @name elic_cat
NULL

# Constructor for the `elic_cat` object
new_elic_cat <- function(levels,
                         sites,
                         experts,
                         mechanisms,
                         title,
                         data = NULL) {

  if (is.null(data)) {
    data <- vector(mode = "list", length = length(mechanisms)) |>
      stats::setNames(mechanisms)
  }

  obj <- list(levels = levels,
              sites = sites,
              experts = experts,
              data = data)

  structure(obj,
            class = "elic_cat",
            title = title)
}

#' @export
print.elic_cat <- function(x, ...) {

  n_mec <- sum(lengths(x[["data"]]) != 0)

  title <- attr(x, "title")

  cli::cli_h2(title)
  cli::cli_li("Level{?s}: {.val {x$levels}}")
  cli::cli_li("Site{?s}: {.val {x$sites}}")
  cli::cli_li("Number of expert{?s}: {.val {x$experts}}")
  cli::cli_li("Mechanisms{?s}: {.val {names(x$data)}}")
  cli::cli_li("Data available for {.val {n_mec}} mechanism{?s}")

  invisible(x)
}
