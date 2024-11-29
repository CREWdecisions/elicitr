# Constructor fro the `elicit` object
new_elicit <- function(var_names,
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
            class = "elicit",
            title = title)
}

#' @export
print.elicit <- function(x, ...) {

  rounds <- sum(lengths(x$data) != 0)

  title <- attr(x, "title")

  cli::cli_h2(title)
  cli::cli_li("Variable{?s}: {.val {x$var_names}}")
  cli::cli_li("Variable type{?s}: {.val {unique(x$var_types)}}")
  cli::cli_li("Elicitation type{?s}: {.val {unique(x$elic_types)}}")
  cli::cli_li("Number of expert{?s}: {.val {x$experts}}")
  cli::cli_li("Number of round{?s}: {.val {rounds}}")
  cli::cli_text()

  if (rounds > 0) {
    cli::style_underline("Round 1") |>
      cli::col_magenta() |>
      cli::cli_text()
    print(x$data$round_1)
  }

  if (rounds == 2) {
    cli::cli_text()
    cli::style_underline("Round 2") |>
      cli::col_magenta() |>
      cli::cli_text()
    print(x$data$round_2)
  }

  invisible(x)
}
