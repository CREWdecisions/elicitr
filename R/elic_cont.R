#' The elic_cont class: storing continuous elicitation metadata and data
#'
#' An `elic_cont` object is an object of class `elic_cont`. It keeps the
#' information about your elicitation and the experts' responses together. It
#' acts as a folder for one elicitation project.
#'
#'The object holds two kinds of information:
#'
#' * **Metadata* describe the elicitation: what the experts are estimating (the
#' names of the variables), the expected type of variables (which values are
#' expected), the elicitation type of each variable (one-, three- or four-point
#' elicitations) and how many experts are expected to participate.
#' * **Data* are the responses provided by the experts, stored separately in a
#' maximum of two elicitation rounds (round 1 and round 2).
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
#' @section Creating the object and adding responses:
#'
#' First, use [cont_start()][elicitr::cont_start()] to create the object by
#' providing the metadata (aka the description of expected responses). Once this
#' is done, the object describes your elicitation but contains no responses.
#' Then use [cont_add_data()][elicitr::cont_add_data()] to add the responses for
#' the first round. If your elicitation includes a second round, add those
#' responses to the same object with another call to
#' [cont_add_data()][elicitr::cont_add_data()].
#'
#' Save the result of each call using `<-`. For example,
#' `my_elicitation <- cont_add_data(my_elicitation, responses, round = 1)`
#' updates the round `1` of the `elic_cont` object called `my_elicitation` with
#' the responses stored in the data file `responses`. Use these functions to
#' create and update the object rather than editing its contents manually, so
#' that the package can check the information.
#'
#' @section Why keep metadata and data together?:
#'
#' The metadata tell the package what responses are expected for each variable.
#' This allows [elicitr][elicitr] to check whether data subsequently added in
#' the `elic_cont` object fit the expectations. This way, data verification
#' happens automatically instead of having to be done manually by the user.
#'
#' For example, if a variable is defined in the metadata as a probability
#' four-point elicitation, [elicitr][elicitr] knows that estimates must fall
#' between zero for that variable and that minimum, maximum, best-guess and
#' confidence estimates must be provided. [elicitr][elicitr] will check that
#' data loaded in the `elic_cont` object fit that description and provide errors
#' or warnings if that is not the case.
#'
#' Keeping this information together means you do not need to enter it again
#' for every analysis. You can pass the same object to
#' [cont_get_data()][elicitr::cont_get_data()],
#' [plot()][elicitr::plot.elic_cont()],
#' [cont_sample_data()][elicitr::cont_sample_data()],
#' [cont_summary()][elicitr::cont_summary()], and
#' [plot()][elicitr::plot.cont_sample()] to retrieve responses, plot raw data,
#' generate samples, summarise results, and create plots. Keeping both rounds in
#' the same object also helps you compare responses before and after experts
#' have had an opportunity to review feedback and revise their estimates.
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
