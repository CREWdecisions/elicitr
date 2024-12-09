#' Add data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `elic_cont_add_data()` adds data to an `elic_cont` object from different
#' sources.
#'
#' @param x an object of class `elic_cont`.
#' @param data_source either a [`data.frame`][base::data.frame] or
#' [`tibble`][tibble::tibble], a string with the path to a _csv_ or _xlsx_ file,
#' or anything accepted by the [read_sheet()][googlesheets4::read_sheet]
#' function.
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
#' the `elic_cont` object.
#' @param verbose logical, if `TRUE` prints informative messages.
#'
#' @section Data Format:
#'
#' Data are expected to have the name of the expert always as first column. The
#' only exception is for data coming from _Google Sheet_ which can have an
#' additional column with a timestamp. This column is automatically removed
#' before the data are added to the `elic_cont` object (see "Data cleaning").
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
#' Moreover, the name of the columns is not important, `elic_cont_add_data()`
#' will overwrite it according to the following convention:
#'
#' *varname*_*suffix*
#'
#' with _suffix_ being one of _min_, _max_, _best_, or _conf_. The information
#' to build the column names is taken from the metadata available in the
#' `elic_cont` object.
#'
#' @section Data cleaning:
#'
#' When data are added to the `elic_cont` object, first names are standardised
#' by converting capital letters to lower case, and by removing any whitespaces
#' and punctuation. Then, data are anonymised by converting names to short sha1
#' hashes. In this way, sensible information collected during the elicitation
#' process never reaches the `elic_cont` object. For three and four points
#' elicitation processes, the order of the values is checked for each expert. If
#' it is not _min-max-best_, the values are swaped accordingly and a informative
#' warn is raised.
#'
#' If the data are imported from _Google Sheets_, `elic_cont_add_data()`
#' performs additional data cleaning operations. This is relevant when data are
#' collected with Google Forms because, for example, there could be multiple
#' submission by the same expert or a different decimal separator could be used.
#' When data are collected with Google Form, a column with the date and time is
#' recorded. First, the function checks for multiple submissions and if present,
#' only the last submission is retained. Second, the function removes the column
#' with the timestamp. Then it checks for consistency of the decimal separator,
#' i.e. commas _,_ are replaced with periods _._. Finally, all columns but the
#' first one (which contains the names) are forced to numeric.
#'
#' @return The provided object of class `elic_cont` updated with the data.
#' @export
#'
#' @family data helpers
#'
#' @author Sergio Vignali
#'
#' @examples
#' # Create the elict object for an elicitation process that estimates 3
#' # variables, the first for a one point estimation of a positive integer, the
#' # second for three points estimation of a negative real, and the last for a
#' # four point estimation of a probability
#' x <- elic_cont_start(var_names = c("var1", "var2", "var3"),
#'                      var_types = "ZNp",
#'                      elic_types = "134",
#'                      experts = 6)
#'
#' # Add data for the first and second round from a data.frame. Notice that the
#' # two commands can be piped
#' my_elicit <- elic_cont_add_data(x, data_source = round_1, round = 1) |>
#'   elic_cont_add_data(data_source = round_2, round = 2)
#' my_elicit
#'
#' # Add data for the first and second round from a csv file
#' files <- list.files(path = system.file("extdata", package = "elicitr"),
#'                     pattern = "round_",
#'                     full.names = TRUE)
#' my_elicit <- elic_cont_add_data(x, data_source = files[1], round = 1) |>
#'   elic_cont_add_data(data_source = files[2], round = 2)
#' my_elicit
#'
#' # Add data for the first and second round from a xlsx file with two sheets
#' file <- list.files(path = system.file("extdata", package = "elicitr"),
#'                    pattern = "rounds",
#'                    full.names = TRUE)
#' # Using the sheet index
#' my_elicit <- elic_cont_add_data(x, data_source = file,
#'                                 sheet = 1, round = 1) |>
#'   elic_cont_add_data(data_source = file, sheet = 2, round = 2)
#' my_elicit
#' # Using the sheet name
#' my_elicit <- elic_cont_add_data(x, data_source = file,
#'                            sheet = "Round 1", round = 1) |>
#'   elic_cont_add_data(data_source = file, sheet = "Round 2", round = 2)
#' my_elicit
#'
#' @examplesIf interactive()
#' # Add data for the first and second round from Google Sheets
#' googlesheets4::gs4_deauth()
#' gs1 <- "12lGIPa-jJOh3fogUDaERmkf04pVpPu9i8SloL2jAdqc"
#' gs2 <- "1wImcfJYnC9a423jlxZiU_BFKXZpTZ7AIsZSxFtEsBQw"
#' my_elicit <- elic_cont_add_data(x, data_source = gs1, round = 1) |>
#'   elic_cont_add_data(data_source = gs2, round = 2)
#' my_elicit
elic_cont_add_data <- function(x,
                               data_source,
                               round,
                               ...,
                               sep = ",",
                               sheet = 1,
                               overwrite = FALSE,
                               verbose = TRUE) {

  check_elic_cont(x)
  check_round(round)

  if (inherits(data_source, "data.frame")) {

    source <- "data.frame"

    # Make sure is a tibble
    data <- data_source |>
      tibble::as_tibble()

  } else if (inherits(data_source, "character")) {
    # When `data_source` contains the file extension at the end of the string,
    # it is assumed to be a file
    ext <- tools::file_ext(data_source)

    if (nzchar(ext)) {

      data <- read_file(data_source,
                        ext = ext,
                        sheet = sheet,
                        sep = sep)

    } else {
      # Assume that `data_source` is a valid Google Sheets file. Otherwise, the
      # error is handled by the read_sheet() function (this doesn't need to be
      # tested).
      source <- "Google Sheets"
      data <- googlesheets4::read_sheet(data_source, sheet = sheet) |>
        suppressMessages() |>
        clean_gs_data()
    }
  }

  col_1 <- colnames(data)[[1]]
  # Anonymise names
  data <- data |>
    dplyr::rename("id" = dplyr::all_of(col_1)) |>
    # Standardise names: remove capital letters, whitespaces, and punctuation
    dplyr::mutate("id" = stand_names(.data[["id"]])) |>
    # Hash names
    dplyr::mutate("id" = hash_names(.data[["id"]]))

  # Prepare column names and set them
  col_names <- get_col_names(x[["var_names"]],
                             x[["elic_types"]])
  check_columns(data, col_names)
  colnames(data) <- col_names

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
                            from {.val {source}}")
  }

  x
}
# Helpers----

#' Read file
#'
#' `read_file()` reads data from a file and returns a tibble.
#'
#' @param data_source character string with the path to the file.
#' @param ext character string with the file extension.
#' @param sheet integer or character to select the sheet.
#' @param sep character used as field separator.
#'
#' @return A tibble with the data or an error if the file doesn't exist or the
#' extension is not supported.
#' @noRd
#'
#' @author Sergio Vignali
read_file <- function(data_source,
                      ext,
                      sheet,
                      sep) {

  # Check if file exists
  if (!file.exists(data_source)) {
    cli::cli_abort(c("x" = "File {.file {data_source}} doesn't exist!"),
                   call = rlang::caller_env())
  }

  # Load data
  if (ext == "csv") {

    assign("source", "csv file", envir = rlang::caller_env())
    data <- utils::read.csv(data_source, sep = sep) |>
      tibble::as_tibble()

  } else if (ext == "xlsx") {

    assign("source", "xlsx file", envir = rlang::caller_env())
    data <- openxlsx::read.xlsx(data_source, sheet = sheet)

  } else {
    error <- "The extension of the provided file is {.val .{ext}}, supported \\
              are {.val .csv} or {.val .xlsx}."

    cli::cli_abort(c("Unsupported file extension:",
                     "x" = error,
                     "i" = "See {.fn elicitr::elic_cont_add_data}."),
                   call = rlang::caller_env())
  }

  data
}

#' Get column names
#'
#' `get_col_names()` combines the information provided with `var_names` and
#' `elic_types` to construct the column names.
#'
#' @inheritParams elic_cont_start
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
#' @inheritParams elic_cont_start
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
  raw_labels <- var_labels[elic_types] |>
    unlist(use.names = FALSE)

  return(raw_labels)
}

#' Standardise names
#'
#' `stand_names()` converts strings to lower case, removes all whitespaces, and
#' removes punctuation.
#'
#' @param x character vector with strings to be normalised.
#'
#' @return Character vector with normalised strings.
#' @noRd
#'
#' @author Sergio Vignali
stand_names <- function(x) {
  tolower(x) |>
    gsub(pattern = "(\\s|[[:punct:]])",
         replacement = "",
         x = _)
}

#' Hash names
#'
#' `hash_names()` converts names to short sha1 codes (7 characters), used to
#' create anonymous ids.
#'
#' @param x character vector with names.
#'
#' @return a vector with encoded names
#' @noRd
#'
#' @author Sergio Vignali
hash_names <- function(x) {

  to_hash <- Vectorize(digest::digest,
                       USE.NAMES = FALSE)

  to_hash(x,
          algo = "sha1",
          serialize = FALSE) |>
    substr(start = 1,
           stop = 7)
}

#' Clean Google Sheets data
#'
#' `clean_gs_data()` performs some data cleaning for data coming from
#' _Google Sheets_.
#'
#' @param x data imported form _Google Sheets_
#'
#' @details
#' 1. Remove column with timestamp, if present
#' 2. Converts columns with lists to character
#' 3. Standardise decimal separators by replacing commas to periods
#' 4. Forces all column but the first to be numeric
#'
#' @return The cleaned data
#' @noRd
#'
#' @author Sergio Vignali
clean_gs_data <- function(x) {

  clean <- \(x) gsub(pattern = ",", replacement = "\\.", x = x) # nolint
  n_cols <- ncol(x)

  if (inherits(x[[1]], "POSIXct")) {
    col_2 <- colnames(x)[[2]]
    x <- x |>
      # Keep only last submission from each expert
      dplyr::slice_tail(n = 1, by = dplyr::all_of(col_2)) |>
      # Remove column with timestamp (it should be the first column)
      dplyr::select(-1)
  }

  x |>
    # Columns with mixed integer and real numbers are imported as list
    dplyr::mutate(dplyr::across(dplyr::where(is.list), as.character)) |>
    # Some experts use a comma as decimal separator
    dplyr::mutate(dplyr::across(dplyr::everything(), clean)) |>
    # If there is a mix of integer and doubles, or if there are different
    # decimal separators, or if someone omit the leading zero on a decimal
    # number, these columns are imported as character
    dplyr::mutate(dplyr::across(!1, as.numeric))
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
               {.fn elicitr::elic_cont_add_data} with {.code overwrite = TRUE}."

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
#' @param x `elic_cont` object containing data from Round 1.
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
        data <- data |>
          add_nas_rows(x[["experts"]])
      }

      n <- length(r1_diff_ids)
      cli::cli_alert_info("The dataset in {.val Round 2} has {.val {n}} \\
                           {.cls id} not present in {.val Round 1}. \\
                           Th{?is/ese} {.cls id} ha{?s/ve} been added to \\
                           {.val Round 1} with {.val {NA}} values.")

      return(list(round_1 = x[["data"]][["round_1"]],
                  round_2 = data))
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
        data <- data |>
          add_nas_rows(x[["experts"]])

        warn <- "The dataset in {.val Round 2} has {.val {n}} {.cls id} not \\
                 present in {.val Round 1}. Th{?is/ese} {.cls id} ha{?s/ve} \\
                 been added to {.val Round 1} with {.val NA} values but \\
                 could be typo{?s} in the raw data."
        info <- "Check raw data and if you want to update the dataset in \\
                 {.val Round 2} use {.fn elicitr::elic_cont_add_data} with \\
                 {.code overwrite = TRUE}."
        cli::cli_warn(c(warn, "i" = info))

        return(list(round_1 = x[["data"]][["round_1"]],
                    round_2 = data))
      } else {
        text <- "Impossible to combine {.val Round 1} and {.val Round 2} \\
                 datasets:"
        error <- "{.val Round 2} has {.val {n}} {.cls id} not present in \\
                  {.val Round 1} which has only {.val {r1_nas}} {.val NA} \\
                  row{?s}."
        info <- "Check raw data and use {.fn elicitr::elic_cont_add_data} to \\
                 add the dataset after manual corrections."
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

      return(list(round_1 = x[["data"]][["round_1"]],
                  round_2 = data))

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
               {.val Round 2} use {.fn elicitr::elic_cont_add_data} with \\
               {.code overwrite = TRUE}."
      cli::cli_warn(c("!" = warn,
                      "i" = info))

      return(list(round_1 = x[["data"]][["round_1"]],
                  round_2 = data))
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

#' Check columns
#'
#' Check whether the number of columns correspond to those expected.
#'
#' @param x data.frame or tibble with the imported data.
#' @param col_names character vector with the expected column names.
#'
#' @noRd
#'
#' @author Sergio Vignali
check_columns <- function(x,
                          col_names) {
  # Check number of columns
  if (ncol(x) != length(col_names)) {
    error <- "The imported dataset has {.val {ncol(x)}} column{?s} but \\
              {.val {length(col_names)}} are expected."
    info <- "See Data Format in {.fn elicitr::elic_cont_add_data}."
    cli::cli_abort(c("Unexpected number of columns:",
                     "x" = error,
                     "i" = info),
                   call = rlang::caller_env())
  }
}

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
               {.fn elicitr::elic_cont_add_data} with {.code overwrite = TRUE}."
      cli::cli_warn(c("!" = warn,
                      "i" = info))
    }
  }

  data

}
