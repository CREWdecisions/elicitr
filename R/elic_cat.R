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
