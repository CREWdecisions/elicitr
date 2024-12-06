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
