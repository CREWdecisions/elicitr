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
#' in the elicitation process for one topic.
#' * `topics`: character vector with the names of the topics investigated.
#' * `data`: list with the data collected during the elicitation process. The
#' list has multiple elements, corresponding to the topics investigated.
#'
#' Moreover, the object has a `title` attribute that binds a name to the object.
#'
#' @name elic_cat
NULL

# Constructor for the `elic_cat` object
new_elic_cat <- function(levels,
                         sites,
                         experts,
                         topics,
                         title,
                         data = NULL) {

  if (is.null(data)) {
    data <- vector(mode = "list", length = length(topics)) |>
      stats::setNames(topics)
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

  n_topic <- sum(lengths(x[["data"]]) != 0)

  title <- attr(x, "title")

  cli::cli_h2(title)
  cli::cli_li("Level{?s}: {.val {x$levels}}")
  cli::cli_li("Site{?s}: {.val {x$sites}}")
  cli::cli_li("Number of expert{?s}: {.val {x$experts}}")
  cli::cli_li("Topic{?s}: {.val {names(x$data)}}")

  if (n_topic > 0) {
    idx <- which(lengths(x[["data"]]) != 0)
    topics <- names(x[["data"]])[idx]
    cli::cli_li("Data available for topic{?s} {.val {topics}}")
  } else {
    cli::cli_li("Data available for {.val {0}} topics")
  }

  invisible(x)
}
