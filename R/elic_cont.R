#' elic_cont class
#'
#' The `elic_cont` class is a container for the elicitation data. It is used to
#' store the data collected during the elicitation process and their matadata.
#'
#' @section Object elements:
#'
#' There are 5 elements in the `elic_cont` object:
#'
#' * `var_names`: character vector with the name of the estimated variables.
#' * `var_types`: character string with short codes indicating the variable
#' type.
#' * `elic_types`: character string with short codes indicating the elicitation
#' type.
#' * `experts`: numeric indicating the number of experts participating in the
#' elicitation process.
#' * `data`: list with the data collected during the elicitation process. The
#' list has two elements, `round_1` and `round_2`, which store the data
#' collected during the first and second round of the elicitation process,
#' respectively.
#'
#' Moreover, the object has a `title` attribute that binds a name to the object.
#'
#' @name elic_cont
NULL

# Constructor for the `elic_cont` object
new_elic_cont <- function(var_names,
                          var_types,
                          elic_types,
                          experts,
                          title,
                          round_1 = NULL,
                          round_2 = NULL) {

  obj <- list(var_names = var_names,
              var_types = var_types,
              elic_types = elic_types,
              experts = experts,
              data = list(round_1 = round_1,
                          round_2 = round_2))

  structure(obj,
            class = "elic_cont",
            title = title)
}

#' @export
print.elic_cont <- function(x, ...) {

  rounds <- sum(lengths(x[["data"]]) != 0)

  title <- attr(x, "title")

  cli::cli_h2(title)
  cli::cli_li("Variable{?s}: {.val {x$var_names}}")
  cli::cli_li("Variable type{?s}: {.val {unique(x$var_types)}}")
  cli::cli_li("Elicitation type{?s}: {.val {unique(x$elic_types)}}")
  cli::cli_li("Number of expert{?s}: {.val {x$experts}}")
  cli::cli_li("Number of rounds: {.val {rounds}}")

  invisible(x)
}
