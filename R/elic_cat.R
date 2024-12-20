#' elic_cat class
#'
#' The `elic_cat` class is a container for the elicitation data. It is used to
#' store the data collected during the elicitation process and their metadata.
#'
#' @section Object elements:
#'
#' There are 6 elements in the `elic_cat` object:
#'
#' * `categories`: character vector with the names of the categories.
#' * `options`: character vector with the names of the options investigated.
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
new_elic_cat <- function(categories,
                         options,
                         experts,
                         topics,
                         title,
                         data = NULL) {

  if (is.null(data)) {
    data <- vector(mode = "list", length = length(topics)) |>
      stats::setNames(topics)
  }

  obj <- list(categories = categories,
              options = options,
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
  cli::cli_li("Categor{?y/ies}: {.val {x$categories}}")
  cli::cli_li("Option{?s}: {.val {x$options}}")
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
