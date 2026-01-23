# List with column labels
var_labels <- list("1p" = "best",
                   "3p" = c("min", "max", "best"),
                   "4p" = c("min", "max", "best", "conf"))

#' Add data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `cont_add_data()` adds data to an [elic_cont] object from different sources.
#'
#' @param x an object of class [elic_cont].
#' @param data_source either a [`data.frame`][base::data.frame] or
#' [`tibble`][tibble::tibble], a string with the path to a _csv_ or _xlsx_ file,
#' or anything accepted by the [read_sheet][googlesheets4::read_sheet] function.
#' @param round integer indicating if the data belongs to the first or second
#' elicitation round.
#' @param ... Unused arguments, included only for future extensions of the
#' function.
#' @param sep character used as field separator, used only when `data_source` is
#' a path to a _csv_ file.
#' @param sheet integer or character to select the sheet. The sheet can be
#' referenced by its position with a number or by its name with a string. Used
#' only when `data_source` is a path to a _xlsx_ file or when data are imported
#' from _Google Sheets_.
#' @param overwrite logical, whether to overwrite existing data already added to
#' the [elic_cont] object.
#' @param verbose logical, if `TRUE` it prints informative messages.
#' @param anonymise logical, if `TRUE` expert names are anonymised before adding
#' the data to the [elic_cont] object.
#'
#' @section Data format:
#'
#' Data are expected to have the name of the expert always as first column. The
#' only exception is for data coming from _Google Sheet_ which can have an
#' additional column with a timestamp. This column is automatically removed
#' before the data are added to the [elic_cont] object (see "Data cleaning").
#' After the name there should be one or more blocks which follow the
#' specifications below:
#'
#' _One point elicitation_:
#'
#' * `var_best`: best estimate for the variable
#'
#' _Three points elicitation_:
#'
#' * `var_min`: minimum estimate for the variable
#' * `var_max`: maximum estimate for the variable
#' * `var_best`: best estimate for the variable
#'
#' _Four points elicitation_:
#'
#' * `var_min`: minimum estimate for the variable
#' * `var_max`: maximum estimate for the variable
#' * `var_best`: best estimate for the variable
#' * `var_conf`: confidence for the estimate
#'
#' The column with names is unique, the other columns are a block and can be
#' repeated for each variable.
#'
#' Moreover, the name of the columns is not important, `cont_add_data()` will
#' overwrite it according to the following convention:
#'
#' *variable_name*_*suffix*
#'
#' with _suffix_ being one of _min_, _max_, _best_, or _conf_. The information
#' to build the column names is taken from the metadata available in the
#' [elic_cont] object.
#'
#' `var_conf`, given as percentage, can be any number in the range (50, 100].
#' Any value smaller or equal to 50 would imply that the accuracy of the
#' estimates is only due to chance).
#'
#' @section Data cleaning:
#'
#' When data are added to the [elic_cont] object, first names are standardised
#' by converting capital letters to lower case, and by removing any whitespaces
#' and punctuation. Then, data are anonymised by converting names to short sha1
#' hashes. In this way, sensible information collected during the elicitation
#' process never reaches the [elic_cont] object. For three and four points
#' elicitation processes, the order of the values is checked for each expert. If
#' it is not _min-max-best_, the values are swapped accordingly and a
#' informative warn is raised.
#'
#' If the data are imported from _Google Sheets_, `cont_add_data()` performs
#' additional data cleaning operations. This is relevant when data are collected
#' with Google Forms because, for example, there could be multiple submission by
#' the same expert or a different decimal separator could be used. When data are
#' collected with Google Form, a column with the date and time is recorded.
#' First, the function checks for multiple submissions and if present, only the
#' last submission is retained. Second, the function removes the column with the
#' timestamp. Then it checks for consistency of the decimal separator, i.e.
#' commas _,_ are replaced with periods _._. Finally, all columns but the first
#' one (which contains the names) are forced to numeric.
#'
#' @return The provided object of class [elic_cont] updated with the data.
#' @export
#'
#' @family cont data helpers
#'
#' @author Sergio Vignali and Maude Vernet
#'
#' @examples
#' # Create the elic_cont object for an elicitation process that estimates 3
#' # variables, the first for a one point estimation of a positive integer, the
#' # second for three points estimation of a negative real, and the last for a
#' # four point estimation of a probability
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

  check_elic_obj(x, type = "cont")
  check_round(round)

  # Read data
  data <- read_data(data_source,
                    sep = sep,
                    sheet = sheet)

  # Prepare column names and set them
  col_names <- get_col_names(x[["var_names"]],
                             x[["elic_types"]])
  # Check if data has the correct number of columns
  check_columns(data, length(col_names))
  colnames(data) <- col_names

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
    cli::cli_abort(c("Data for {.val Round 1} are not present:",
                     "i" = "Data for {.val Round 2} can be added only after \\
                            those for {.val Round 1}."))
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
    cli::cli_abort(c("Data for {.val Round {round}} already present:",
                     "i" = "Set {.code overwrite = TRUE} if you want to \\
                            overwrite them."))
  }

  if (verbose) {
    cli::cli_alert_success("Data added to {.val {paste(\"Round\", round)}} \\
                            from {.val {src}}")
  }

  x
}
# Helpers----

#' Get column names
#'
#' `get_col_names()` combines the information provided with `var_names` and
#' `elic_types` to construct the column names.
#'
#' @inheritParams cont_start
#'
#' @return Character vector with the column names.
#' @noRd
#'
#' @author Sergio Vignali
get_col_names <- function(var_names,
                          elic_types) {
  n <- length(var_names)
  r <- gsub("p", "", elic_types, fixed = TRUE) |>
    as.integer()
  labels <- get_labels(n = n,
                       elic_types = elic_types)

  var_columns <- rep(var_names, r) |>
    paste0("_", labels)

  c("id", var_columns)
}

#' Get labels
#'
#' Get label to build column names.
#'
#' @param n integer, number of variables.
#' @inheritParams cont_start
#'
#' @return Character vector with the labels
#' @noRd
#'
#' @author Sergio Vignali
get_labels <- function(n,
                       elic_types) {

  if (n > 1L && length(elic_types) == 1L) {
    # Recycle variable type
    elic_types <- rep(elic_types, n)
  }

  # Labels are stored on the `var_labels` data object
  raw_labels <- unlist(var_labels[elic_types],
                       use.names = FALSE)

  raw_labels
}

#' Fix variable order
#'
#' Sometimes values are not in the correct order _min_ _max_ _best_. This
#' function checks the order for each variable and row that are a three or four
#' points estimation and, if necessary, reorders the values. It also raises a
#' warn with information on which variable/s and row/s that has/have been
#' reordered.
#'
#' @param x tibble with the data.
#' @param var_names character vector with the variable names.
#' @param elic_types character string with the elicitation types.
#'
#' @return The reordered tibble. Also a warn with information on which
#' variable/s and row/s that has/have been reordered.
#' @noRd
#'
#' @author Sergio Vignali
fix_var_order <- function(x,
                          var_names,
                          elic_types) {

  # Only 3p and 4p elicitation types should be checked
  idx_vars <- elic_types != "1p"
  vars <- var_names[idx_vars]

  for (i in seq_along(vars)) {
    idx_cols <- grepl(vars[i], colnames(x))

    # The confidence value should not be reordered
    if (sum(idx_cols) == 4) {
      idx_cols[length(idx_cols)] <- FALSE
    }

    idx_rows <- apply(x[, idx_cols], 1, is_not_min_max_best)

    if (sum(idx_rows) > 0) {
      x[, idx_cols] <- apply(x[, idx_cols], 1, min_max_best) |>
        t()

      ids <- dplyr::pull(x, 1)[which(idx_rows)]

      warn <- "{.field {vars[i]}} of {.cls id} {.val {ids}} reordered \\
                following the order {.val min-max-best}."
      info <- "Check raw data and if you want to update the dataset use
               {.fn elicitr::cont_add_data} with {.code overwrite = TRUE}."

      cli::cli_warn(c("!" = warn))
    }
  }
  x
}

#' Min max best
#'
#' Given a vector of three values, the function returns the same vector
#' reordered following the sequence min-max-best.
#'
#' @param x numeric vector of three elements with the values to be reordered.
#'
#' @return A vector with the reordered values.
#' @noRd
#'
#' @author Sergio Vignali
min_max_best <- function(x) {
  c(min(x), max(x), stats::median(x))
}

#' Is min max best
#'
#' The function checks if the elements of a vector are in the order
#' min-max-best.
#'
#' @param x numeric vector of three elements to be checked.
#'
#' @return A logical value with `TRUE` if the elements of the vector are in the
#' order min-max-best, `FALSE` otherwise.
#' @noRd
#'
#' @author Sergio Vignali
is_not_min_max_best <- function(x) {
  !all(x == min_max_best(x))
}

#' Add NAs to data
#'
#' Adds rows with NAs, used when datasets have less data than experts.
#'
#' @param data tibble with data belonging to the first round.
#' @param experts numeric providing the number of experts.
#'
#' @return Data to add to the Round with added NAs.
#' @noRd
#'
#' @author Sergio Vignali
add_nas_rows <- function(data, experts) {

  n <- experts - nrow(data)
  nas <- matrix(nrow = n, ncol = ncol(data)) |>
    as.data.frame() |>
    stats::setNames(colnames(data))

  rbind(data, nas)
}

#' Omogenise datasets
#'
#' `omogenise_datasets()` is used to omogenise the data in Round 1 and Round 2.
#'
#' @param x [elic_cont] object containing data from Round 1.
#' @param data tibble containing data from Round 2.
#'
#' @return A list with two elemnts, one containing the omogenised data for Round
#' 1, and the other containing the omogenised data for Round 2.
#' @noRd
#'
#' @author Sergio Vignali
omogenise_datasets <- function(x, data) {

  experts <- x[["experts"]]
  nrow_round_1 <- nrow(x[["data"]][["round_1"]])
  nrow_round_2 <- nrow(data)
  n_diff <- nrow_round_1 - nrow_round_2
  all_ids <- union(x[["data"]][["round_1"]][["id"]], data[["id"]])

  round_1_has_nas <- anyNA(x[["data"]][["round_1"]][["id"]])


  if (round_1_has_nas) {
    round_1_ids <- x[["data"]][["round_1"]][["id"]]
    round_2_ids <- data[["id"]]
    r1_diff_ids <- setdiff(round_2_ids, round_1_ids)
    r2_diff_ids <- setdiff(round_1_ids, round_2_ids) |>
      stats::na.omit()
    round_2_has_nas <- length(data[["id"]]) < x[["experts"]]

    # Check if all non NA id in round 1 are also in round 2
    if (all(stats::na.omit(round_1_ids) %in% round_2_ids)) {

      data <- dplyr::rows_upsert(x[["data"]][["round_1"]], data,
                                 by = "id") |>
        stats::na.omit()

      r1_na_idx <- which(is.na(round_1_ids))
      selection <- r1_na_idx[seq_along(r1_diff_ids)]
      x[["data"]][["round_1"]][["id"]][selection] <- r1_diff_ids

      if (round_2_has_nas) {
        data <- add_nas_rows(data,
                             x[["experts"]])
      }

      n <- length(r1_diff_ids)
      cli::cli_alert_info("The dataset in {.val Round 2} has {.val {n}} \\
                           {.cls id} not present in {.val Round 1}. \\
                           Th{?is/ese} {.cls id} ha{?s/ve} been added to \\
                           {.val Round 1} with {.val {NA}} values.")

      list(round_1 = x[["data"]][["round_1"]],
           round_2 = data)
    } else {
      r1_na_idx <- which(is.na(round_1_ids))
      n <- length(r1_diff_ids)
      r1_nas <- sum(is.na(round_1_ids))

      if (r1_nas >= length(r1_diff_ids)) {
        data <- dplyr::rows_upsert(x[["data"]][["round_1"]], data,
                                   by = "id") |>
          stats::na.omit()

        selection <- r1_na_idx[seq_along(r1_diff_ids)]
        x[["data"]][["round_1"]][["id"]][selection] <- r1_diff_ids
        r2_na_idx <- which(is.na(round_2_ids))
        data[data[["id"]] == r2_diff_ids, -1] <- NA
        data <- add_nas_rows(data,
                             x[["experts"]])

        warn <- "The dataset in {.val Round 2} has {.val {n}} {.cls id} not \\
                 present in {.val Round 1}. Th{?is/ese} {.cls id} ha{?s/ve} \\
                 been added to {.val Round 1} with {.val NA} values but \\
                 could be typo{?s} in the raw data."
        info <- "Check raw data and if you want to update the dataset in \\
                 {.val Round 2} use {.fn elicitr::cont_add_data} with \\
                 {.code overwrite = TRUE}."
        cli::cli_warn(c(warn, "i" = info))

        list(round_1 = x[["data"]][["round_1"]],
             round_2 = data)
      } else {
        text <- "Impossible to combine {.val Round 1} and {.val Round 2} \\
                 datasets:"
        error <- "{.val Round 2} has {.val {n}} {.cls id} not present in \\
                  {.val Round 1} which has only {.val {r1_nas}} {.val NA} \\
                  row{?s}."
        info <- "Check raw data and use {.fn elicitr::cont_add_data} to add \\
                 the dataset after manual corrections."
        cli::cli_abort(c(text, "x" = error, "i" = info),
                       call = rlang::caller_env())
      }
    }
  } else {
    # No NAs in Round 1 and Round 2 has one row for each expert
    n <- length(all_ids) - nrow_round_1

    # All id of Round 2 are also in Round 1 ==> reorder data in Round 2
    if (n == 0) {

      mis_in_round_2 <- setdiff(x[["data"]][["round_1"]][["id"]], data[["id"]])

      data <- dplyr::rows_upsert(x[["data"]][["round_1"]], data,
                                 by = "id")

      # Round 2 could have less entries than experts ==> Fill with NAs
      if (length(mis_in_round_2) > 0) {
        data[data[["id"]] %in% mis_in_round_2, -1] <- NA
      }

      list(round_1 = x[["data"]][["round_1"]],
           round_2 = data)

    } else if (n == 1) {
      # Same number of rows in Round 1 and Round 2 but one id is different ==>
      # Consider it as a typo and replace it with the one from Round 1. Also
      # raise a warning.
      mis_in_round_1 <- setdiff(data[["id"]], x[["data"]][["round_1"]][["id"]])
      mis_in_round_2 <- setdiff(x[["data"]][["round_1"]][["id"]], data[["id"]])

      # Round 2 could have less entries than experts ==> Impossible match, raise
      # an error
      if (length(mis_in_round_2) > 0 && n_diff > 0) {

        missing <- setdiff(data[["id"]], x[["data"]][["round_1"]][["id"]])
        text <- "Dataset for {.val Round 2} has {.val {1}} {.cls id} not \\
                 present in {.val Round 1} and {.val {n_diff}} entries with \\
                 NAs. Automatic match between the two datasets is not possible:"
        error <- "The {.cls id} not present in {.val Round 1} {?is/are} \\
                  {.val {missing}}."
        cli::cli_abort(c(text, "x" = error, "i" = "Check raw data."),
                       call = rlang::caller_env())
      }

      data[data[["id"]] == mis_in_round_1, "id"] <- mis_in_round_2
      data <- dplyr::rows_upsert(x[["data"]][["round_1"]], data,
                                 by = "id")

      warn <- "Dataset for {.val Round 2} has {.val {1}} {.cls id} not \\
               present in {.val Round 1}. This is considered a typo by \\
               expert {.val {mis_in_round_2}} in {.val Round 2} and the \\
               {.cls id} has been replaced."

      info <- "Check raw data and if you want to update the dataset in \\
               {.val Round 2} use {.fn elicitr::cont_add_data} with \\
               {.code overwrite = TRUE}."
      cli::cli_warn(c("!" = warn,
                      "i" = info))

      list(round_1 = x[["data"]][["round_1"]],
           round_2 = data)
    } else {
      # More than 1 id present in Round 2 is not in Round 1 ==> Raise an error
      missing <- setdiff(data[["id"]], x[["data"]][["round_1"]][["id"]])
      text <- "Dataset for {.val Round 2} has {.val {n}} {.cls id} not \\
               present in {.val Round 1}. Automatic match between the two \\
               datasets is not possible:"
      error <- "The {.cls id} not present in {.val Round 1} are \\
                {.val {missing}}."
      info <- "Check raw data to identify the mismatch."
      cli::cli_abort(c(text, "x" = error, "i" = info),
                     call = rlang::caller_env())
    }
  }
}

# Checkers----

#' Check round data
#'
#' Check if the number of rows on the dataset are the same as the number of
#' experts. If there are more data than experts, raises an error. If there are
#' more experts than data, raise a warn and eventually adds NAs only for
#' Round 1.
#'
#' @param data tibble with data belonging to the first round.
#' @param experts numeric providing the number of experts.
#' @param round integer indicating the elicitation round.
#'
#' @return An error or the data to add to the Round, eventually with added NAs.
#' @noRd
#'
#' @author Sergio Vignali
check_round_data <- function(data, experts, round) {

  if (nrow(data) != experts) {

    # Number of experts and rows in data are not the same
    if (nrow(data) > experts) {

      # More data than experts ==> Raise error
      error <- "The dataset for {.val Round {round}} contains \\
                {.val {nrow(data)}} rows but expects estimates from \\
                {.val {experts}} experts."

      if (round == 1) {
        info <- "Check raw data or modify the {.cls elic_cont} object by \\
                 setting the number of experts to {.val {nrow(data)}} with \\
                 {.code obj$experts = {nrow(data)}}."
      } else {
        info <- "Check raw data."
      }


      cli::cli_abort(c("Incorrect number of rows in dataset:",
                       "x" = error,
                       "i" = info),
                     call = rlang::caller_env())
    } else {
      # More experts than data ==> raise a warn
      n_round <- nrow(data)
      n_diff <- experts - n_round

      # Add NAs is delegated to omogenise_datasets() when round == 2
      if (round == 1) {
        # Add NAs
        data <- add_nas_rows(data, experts)
      }

      warn <- "The dataset for {.val Round {round}} has {.val {n_round}} rows \\
               but expects {.val {experts}} experts. {.val {NA}}s added to \\
               missing {.cls id}."
      info <- "Check raw data and if you want to update the dataset use \\
               {.fn elicitr::cont_add_data} with {.code overwrite = TRUE}."
      cli::cli_warn(c("!" = warn,
                      "i" = info))
    }
  }

  data
}

#' Check data type
#'
#' Check if the data types are correct according to the metadata stored in the
#' [elic_cont] object.
#'
#' @param x [elic_cont] object containing the metadata.
#' @param data [`tibble`][tibble::tibble] with the data to be checked.
#'
#' @returns An error if the data types are not correct.
#' @noRd
#'
#' @author Sergio Vignali
check_data_types <- function(x, data) {

  var_names <- x[["var_names"]]
  var_types <- x[["var_types"]]
  elic_types <- x[["elic_types"]]

  for (i in seq_along(var_names)) {

    idx <- grepl(var_names[[i]], colnames(data))

    df <- unlist(data[, idx])

    if (any(is.na(df))) {

      error <- "Variable {.val {var_names[[i]]}} contains {.val NA} values."

      cli::cli_abort(c("Invalid raw data:",
                       "x" = error,
                       "i" = "Check raw data."),
                     call = rlang::caller_env(n = 2))

    }

    if (var_types[i] == "Z") {

      check_is_integer(df, var_names[i])

    } else if (var_types[i] == "N") {

      check_is_positive_integer(df, var_names[i])

    } else if (var_types[i] == "z") {

      check_is_negative_integer(df, var_names[i])

    } else if (var_types[i] == "s") {

      check_is_positive_real(df, var_names[i])

    } else if (var_types[i] == "r") {

      check_is_negative_real(df, var_names[i])

    } else if (var_types[i] == "p") {

      check_is_probability(df, var_names[i])

    }

    if (elic_types[i]  == "4p") {
      check_conf(df, var_names[i])
    }
  }
}

#' Check if the argument `x` is a integer
#'
#' @param x numeric vector to be checked.
#' @param v character string with the name of the variable to be checked.
#'
#' @returns An error if `x` contains some non integer numbers.
#' @noRd
#'
#' @author Sergio Vignali
check_is_integer <- function(x, v) {

  idx <- grepl("conf", names(x), fixed = TRUE)
  x <- x[!idx]

  if (!all(x %% 1 == 0)) {

    error <- "Variable {.val {v}} contains some non integer numbers."

    cli::cli_abort(c("Invalid data type:",
                     "x" = error,
                     "i" = "Check raw data."),
                   call = rlang::caller_env(n = 2))
  }
}

#' Check if the argument `x` is a positive integer
#'
#' @param x numeric vector to be checked.
#' @param v character string with the name of the variable to be checked.
#'
#' @returns An error if `x` contains some non positive integer numbers.
#' @noRd
#'
#' @author Sergio Vignali
check_is_positive_integer <- function(x, v) {

  idx <- grepl("conf", names(x), fixed = TRUE)
  x <- x[!idx]

  if (!all(x %% 1 == 0) || any(x < 0)) {

    error <- "Variable {.val {v}} contains some non positive integer numbers."

    cli::cli_abort(c("Invalid data type:",
                     "x" = error,
                     "i" = "Check raw data."),
                   call = rlang::caller_env(n = 2))
  }
}

#' Check if the argument `x` is a negative integer
#'
#' @param x numeric vector to be checked.
#' @param v character string with the name of the variable to be checked.
#'
#' @returns An error if `x` contains some non negative integer numbers.
#' @noRd
#'
#' @author Sergio Vignali
check_is_negative_integer <- function(x, v) {

  idx <- grepl("conf", names(x), fixed = TRUE)
  x <- x[!idx]

  if (!all(x %% 1 == 0) || any(x) >= 0) {

    error <- "Variable {.val {v}} contains some non negative integer numbers."

    cli::cli_abort(c("Invalid data type:",
                     "x" = error,
                     "i" = "Check raw data."),
                   call = rlang::caller_env(n = 2))
  }
}

#' Check if the argument `x` is a positive real number
#'
#' @param x numeric vector to be checked.
#' @param v character string with the name of the variable to be checked.
#'
#' @returns An error if `x` contains some non positive numbers.
#' @noRd
#'
#' @author Sergio Vignali
check_is_positive_real <- function(x, v) {

  idx <- grepl("conf", names(x), fixed = TRUE)
  x <- x[!idx]

  if (any(x < 0)) {

    error <- "Variable {.val {v}} contains some non positive numbers."

    cli::cli_abort(c("Invalid data type:",
                     "x" = error,
                     "i" = "Check raw data."),
                   call = rlang::caller_env(n = 2))
  }
}

#' Check if the argument `x` is a negative real number
#'
#' @param x numeric vector to be checked.
#' @param v character string with the name of the variable to be checked.
#'
#' @returns An error if `x` contains some non negative numbers.
#' @noRd
#'
#' @author Sergio Vignali
check_is_negative_real <- function(x, v) {

  idx <- grepl("conf", names(x), fixed = TRUE)
  x <- x[!idx]

  if (any(x >= 0)) {

    error <- "Variable {.val {v}} contains some non negative numbers."

    cli::cli_abort(c("Invalid data type:",
                     "x" = error,
                     "i" = "Check raw data."),
                   call = rlang::caller_env(n = 2))
  }
}

#' Check if the argument `x` is a probability
#'
#' @param x numeric vector to be checked.
#' @param v character string with the name of the variable to be checked.
#'
#' @returns An error if `x` contains some values not in the range [0, 1].
#' @noRd
#'
#' @author Sergio Vignali
check_is_probability <- function(x, v) {

  idx <- grepl("conf", names(x), fixed = TRUE)
  x <- x[!idx]

  if (!all(x >= 0) || !all(x <= 1)) {

    error <- "Variable {.val {v}} contains some values not in the range [0, 1]."

    cli::cli_abort(c("Invalid data type:",
                     "x" = error,
                     "i" = "Check raw data."),
                   call = rlang::caller_env(n = 2))
  }
}

check_conf <- function(x, v) {

  idx <- grepl("conf", names(x), fixed = TRUE)
  x <- x[idx]

  if (!all(x > 50) || !all(x <= 100)) {

    error <- "Variable {.val {v}} contains confidence estimates not in the \\
              range (50, 100]."

    cli::cli_abort(c("Invalid data type:",
                     "x" = error,
                     "i" = "Check raw data."),
                   call = rlang::caller_env(n = 2))
  }
}
