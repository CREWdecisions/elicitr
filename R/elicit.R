# Constructor fro the `elicit` object
new_elicit <- function(var_names,
                       var_types,
                       elic_types,
                       title,
                       round_1 = NULL,
                       round_2 = NULL) {

  obj <- list(var_names = var_names,
              var_types = var_types,
              elic_types = elic_types,
              data = list(round_1 = round_1,
                          round_2 = round_2))

  structure(obj,
            class = "elicit",
            title = title)
}

#' @export
print.elicit <- function(x, ...) {

  rounds <- lengths(x$data) |>
    sum()

  title <- attr(x, "title")

  cli::cli_h2(title)
  cli::cli_li("Variable{?s}: {.field {x$var_names}}")
  cli::cli_li("Variable type{?s}: {.field {unique(x$var_types)}}")
  cli::cli_li("Elicitation type{?s}: {.field {unique(x$elic_types)}}")
  cli::cli_li("Number of rounds: {.val {rounds}}")
  cli::cli_text()

  if (rounds > 0) {
    cli::style_underline("Round 1") |>
      cli::col_magenta() |>
      cli::cli_text()
    print(x$data$round_1)
  }

  if (rounds == 2) {
    cli::style_underline("Round 2") |>
      cli::col_magenta() |>
      cli::cli_text()
    print(x$data$round_2)
  }

  invisible(x)
}
