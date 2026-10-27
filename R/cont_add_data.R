# List with column labels
var_labels <- list("1p" = "best",
                   "3p" = c("min", "max", "best"),
                   "4p" = c("min", "max", "best", "conf"))

#' Add data
#'
#' @description `r lifecycle::badge("stable")`
#'
#' `cont_add_data()` adds data to an [elic_cont][elicitr::elic_cont] object from
#' different sources.
#'
#' @param x an object of class [elic_cont][elicitr::elic_cont] created with
#'   [cont_start][elicitr::cont_start].
#' @param data_source either a [`data.frame`][base::data.frame] or
#'   [`tibble`][tibble::tibble], a string with the path to a _csv_ or _xlsx_
#'   file, or anything accepted by the [read_sheet][googlesheets4::read_sheet]
#'   function.
#' @param round integer (1 or 2) indicating if the data belongs to the first or
#'   second elicitation round. See section rounds for more detail.
#' @param ... Unused arguments, included only for future extensions of the
#'   function.
#' @param sep character used as field separator, used only when `data_source` is
#'   a path to a _csv_ file.
#' @param sheet integer or character to select the sheet. The sheet can be
#'   referenced by its position with a number or by its name with a string. Used
#'   only when `data_source` is a path to a _xlsx_ file or when data are
#'   imported from _Google Sheets_.
#' @param overwrite logical, whether to overwrite existing data already added to
#'   the [elic_cont][elicitr::elic_cont] object. See section rounds for more
#'   detail.
#' @param verbose logical, if `TRUE` it prints informative messages.
#' @param anonymise logical, if `TRUE` expert names are anonymised before adding
#'   the data to the [elic_cont][elicitr::elic_cont] object.
#'
#' @section Rounds: [elicitr][elicitr] supports two elicitation rounds,
#'   following approaches such as the Delphi and IDEA protocols. The idea behind
#'   the two rounds is that experts first provide individual estimates, then
#'   review feedback or discuss differences before submitting revised estimates
#'   in a second round. This gives experts an opportunity to reconsider their
#'   judgments while retaining both sets of responses for comparison.
#'
#'   When adding data to the [elic_cont][elicitr::elic_cont] object previously
#'   created with [cont_start][elicitr::cont_start], [elicitr][elicitr] checks
#'   that the number of responses matches the declared number of experts. When
#'   adding round 2, it matches expert IDs across rounds and aligns their
#'   responses. It also flags inconsistencies in expert IDs and can correct a
#'   single differing ID as a presumed typo under specific conditions. Data must
#'   be loaded in round 1 before a second elicitation round can be loaded in
#'   round 2. To replace existing data, the argument *overwrite* must be set to
#'   *TRUE* in [cont_add_data][elicitr::cont_add_data].
#'
#' @section Data format:
#'
#'   Data are expected to have the name of the expert always as first column.
#'   The only exception is for data coming from _Google Sheet_ which can have an
#'   additional column with a timestamp. This column is automatically removed
#'   before the data are added to the [elic_cont][elicitr::elic_cont] object
#'   (see "Data cleaning"). After the name there should be one or more blocks
#'   which follow the specifications below:
#'
#'   _One point elicitation_:
#'
#' * `var_best`: best estimate for the variable
#'
#'   _Three points elicitation_:
#'
#' * `var_min`: minimum estimate for the variable
#' * `var_max`: maximum estimate for the variable
#' * `var_best`: best estimate for the variable
#'
#'   _Four points elicitation_:
#'
#' * `var_min`: minimum estimate for the variable
#' * `var_max`: maximum estimate for the variable
#' * `var_best`: best estimate for the variable
#' * `var_conf`: confidence for the estimate
#'
#'   The column with names is unique, the other columns are a block and can be
#'   repeated for each variable.
#'
#' @section Column and variable names:
#'
#'   The name of the columns is not important, as long as they are different.
#'   `cont_add_data()` will overwrite them according to the convention
#' *variable_name*_*suffix* with _suffix_ being one of _min_, _max_, _best_, or
#'   _conf_. The information to build the column names is taken from the
#'   metadata available in the [elic_cont][elicitr::elic_cont] object.
#'
#'   `var_conf`, given as percentage, can be any number in the range (50, 100].
#'   This is because any value smaller or equal to 50 would imply that the
#'   accuracy of the estimates is only due to chance).
#'
#' @section NAs:
#'
#'   Experts can decide to not provide estimates for all variables. In this
#'   case, the missing values should be recorded as `NA`. However, experts
#'   should always answer completely to a variable (aka provide values for all
#'   aspect of the variable (minimum, maximum, best guess and confidence)) or
#'   not at all. If estimates are only partially given for a  variable (example
#'   _min_ and _max_ provided but not _best_ for a three-point elicitation), an
#'   error is raised.
#'
#' @section Data cleaning:
#'
#'   When data are added to the [elic_cont][elicitr::elic_cont] object, first
#'   names are standardised by converting capital letters to lower case, and by
#'   removing any whitespaces and punctuation. Then, data are anonymised by
#'   converting names to short sha1 hashes. In this way, sensible information
#'   collected during the elicitation process never reaches the
#'   [elic_cont][elicitr::elic_cont] object. For three and four points
#'   elicitation processes, the order of the values is checked for each expert.
#'   If it is not _min-max-best_, the values are swapped accordingly and a
#'   informative warn is raised.
#'
#'   If the data are imported from _Google Sheets_, `cont_add_data()` performs
#'   additional data cleaning operations. This is relevant when data are
#'   collected with Google Forms because, for example, there could be multiple
#'   submission by the same expert or a different decimal separator could be
#'   used. When data are collected with Google Form, a column with the date and
#'   time is recorded. First, the function checks for multiple submissions and
#'   if present, only the last submission is retained. Second, the function
#'   removes the column with the timestamp. Then it checks for consistency of
#'   the decimal separator, i.e. commas _,_ are replaced with periods _._.
#'   Finally, all columns but the first one (which contains the names) are
#'   forced to numeric.
#'
#' @return The provided object of class [elic_cont][elicitr::elic_cont] updated
#'   with the data.
#' @export
#'
#' @family cont data helpers
#'
#' @author Sergio Vignali and Maude Vernet
#'
#' @examples
#' # Create the elic_cont object for an elicitation process that estimates 3
#' # variables, the first for a one point estimation of a positive round number,
#' # the second for three points estimation of a negative real, and the last for
#' # a four point estimation of a probability
#' x <- cont_start(var_names = c("var1", "var2", "var3"),
#'                 var_types = "ZNp",
#'                 elic_types = "134",
#'                 experts = 6)
#'
#' # Add data for the first and second round from a data.frame. Notice that the
#' # two commands can be piped
#' my_elicit <- cont_add_data(x, data_source = round_1, round = 1) |>
#'   cont_add_data(data_source = round_2, round = 2)
#' my_elicit
#'
#' # Add data for the first and second round from a csv file
#' files <- list.files(path = system.file("extdata", package = "elicitr"),
#'                     pattern = "round_",
#'                     full.names = TRUE)
#' my_elicit <- cont_add_data(x, data_source = files[1], round = 1) |>
#'   cont_add_data(data_source = files[2], round = 2)
#' my_elicit
#'
#' # Add data for the first and second round from a xlsx file with two sheets
#' file <- list.files(path = system.file("extdata", package = "elicitr"),
#'                    pattern = "rounds",
#'                    full.names = TRUE)
#' # Using the sheet index
#' my_elicit <- cont_add_data(x, data_source = file, sheet = 1, round = 1) |>
#'   cont_add_data(data_source = file, sheet = 2, round = 2)
#' my_elicit
#' # Using the sheet name
#' my_elicit <- cont_add_data(x, data_source = file,
#'                            sheet = "Round 1", round = 1) |>
#'   cont_add_data(data_source = file, sheet = "Round 2", round = 2)
#' my_elicit
#'
#' @examplesIf interactive()
#' # Add data for the first and second round from Google Sheets
#' googlesheets4::gs4_deauth()
#' gs1 <- "12lGIPa-jJOh3fogUDaERmkf04pVpPu9i8SloL2jAdqc"
#' gs2 <- "1wImcfJYnC9a423jlxZiU_BFKXZpTZ7AIsZSxFtEsBQw"
#' my_elicit <- cont_add_data(x, data_source = gs1, round = 1) |>
#'   cont_add_data(data_source = gs2, round = 2)
#' my_elicit
cont_add_data <- function(x,
                          data_source,
                          round,
                          ...,
                          sep = ",",
                          sheet = 1,
                          overwrite = FALSE,
                          verbose = TRUE,
                          anonymise = TRUE) {

  check_elic_obj(x, type = "cont", metadata = TRUE)
  check_round(round)

  # Read data
  data <- read_data(data_source,
                    sep = sep,
                    sheet = sheet)

  if (nrow(data) == 0) {
    cli::cli_abort(c("Data is empty.",
                     "x" = "Please add data",
                     "i" = "To test whether the data is correctly formed, add \\
                     a test response."))
  }

  # Prepare column names and set them
  col_names <- get_col_names(x[["var_names"]],
                             x[["elic_types"]])
  # Check if data has the correct number of columns
  check_columns(data, length(col_names), type = "cont")

  colnames(data) <- col_names

  if (!is.character(data[["id"]])) {
    fn <- as.list(sys.call())[[1]]
    cli::cli_abort(c("Malformated data.",
                     "x" = "The first column does not contain the expert IDs.",
                     "i" = "See Data format in {.fn elicitr::{fn}}"))
  }

  if (anonymise) {
    # Anonymise names
    data <- anonimise_names(data)
  }

  check_data_types(x, data)

  # If necessary, fix element order for each variable
  data <- fix_var_order(data,
                        var_names = x[["var_names"]],
                        elic_types = x[["elic_types"]])

  # Add data to the given round
  obj_data <- x[["data"]][[round]]
  if (round == 2 && is.null(x[["data"]][[1]])) {

    fn <- as.list(sys.call())[[1]]
    cli::cli_abort(c("{.val Round 1} is empty.",
                     "x" = "Wrong value provided for {.arg round}.",
                     " " = "Data can only be loaded in {.val Round 2} after \\
                     data was loaded in {.val Round 1}.",
                     "i" = "See {.fn elicitr::{fn}} for more information."))
  } else if ((!is.null(obj_data) && overwrite) || is.null(obj_data)) {

    if (round == 1) {

      x[["data"]][[round]] <- check_round_data(data, x[["experts"]], round)

    } else {
      # Omogenise data
      data <- check_round_data(data, x[["experts"]], round)
      ds <- omogenise_datasets(x, data)
      x[["data"]][["round_1"]] <- ds[["round_1"]]
      x[["data"]][["round_2"]] <- ds[["round_2"]]
    }

  } else {
    fn <- as.list(sys.call())[[1]]
    cli::cli_abort(c("Data for {.val Round {round}} already present:",
                     "x" = "Set {.code overwrite = TRUE} if you want to \\
                            overwrite them.",
                     "i" = "See {.fn elicitr::{fn}} for more information."))
  }

  if (verbose) {
    cli::cli_alert_success("Data added to {.val {paste(\"Round\", round)}} \\
                            from {.val {src}}")
  }

  x
}
