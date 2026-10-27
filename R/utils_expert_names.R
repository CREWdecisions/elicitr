#' Anonymise names
#'
#' Converts names to anonymous ids from the names of the experts.
#'
#' @param x [tibble][tibble::tibble] with the data collected during the
#' elicitation process.
#'
#' @returns A [tibble][tibble::tibble] with names converted to anonymous ids.
#' @noRd
#'
#' @author Sergio Vignali
anonimise_names <- function(x) {

  col_1 <- colnames(x)[[1]]

  x |>
    dplyr::rename("id" = dplyr::all_of(col_1)) |>
    # Standardise names: remove capital letters, whitespaces, and punctuation
    dplyr::mutate("id" = stand_names(.data[["id"]]),
                  "id" = hash_names(.data[["id"]]))
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

# Rename experts----

#' Rename experts
#' Check if the new names vector is of the correct length
#' Rename experts in elic_cont object
#' @param x the object to be plotted
#' @param data tibble with the elicitation data
#' @param expert_names character vector with the new names for the experts
#'
#' @return the object with renamed experts
#' @noRd
#'
#' @author Maude Vernet
cont_rename_experts <- function(x,
                                data,
                                expert_names) {

  n_experts <- length(unique(data[["id"]]))

  if (length(expert_names) != n_experts) {
    fn <- as.list(sys.call(-1))[[1]]
    error <- "You provided {.val {length(expert_names)}} expert names but the \\
              elicitation process has {.val {n_experts}} experts."
    cli::cli_abort(c("Incorrect length of {.arg expert_names}:",
                     "x" = error,
                     "i" = "Provide a vector of length {.val {n_experts}}.",
                     " " = "See {.fn elicitr::{fn}} for more information."),
                   call = rlang::caller_env())
  }

  if (anyDuplicated(expert_names) > 0) {
    fn <- as.list(sys.call(-1))[[1]]
    error <- "Multiple experts have the same name in {.arg expert_names}."
    cli::cli_abort(c("Invalid value for {.arg expert_names}:",
                     "x" = error,
                     "i" = "Please provide unique names for each expert.",
                     " " = "See {.fn elicitr::{fn}} for more information."),
                   call = rlang::caller_env())
  }

  if (any(expert_names == "Group") || any(expert_names == "Truth")) {
    fn <- as.list(sys.call(-1))[[1]]
    error <- "The names {.val Group} and {.val Truth} are reserved and cannot \\
              be used in {.arg expert_names}."
    cli::cli_abort(c("Invalid value for {.arg expert_names}:",
                     "x" = error,
                     "i" = "Please provide different names for the experts.",
                     " " = "See {.fn elicitr::{fn}} for more information."),
                   call = rlang::caller_env())
  }

  if (class(x)[1] == "elic_cont") {
    expert_names <- as.character(expert_names)
    data[["id"]] <- expert_names
  }

  if (class(data)[1] == "cont_sample") {
    names(expert_names) <- unique(data$id)
    data$id <- expert_names[data$id]
    data
  }

  data
}
