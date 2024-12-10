set.seed(25)
n_experts <- 6

random_names <- randomNames::randomNames(n_experts,
                                         which.names = "both",
                                         name.order = "first.last",
                                         name.sep = " ")


levels <- c("level_1", "level_2", "level_3", "level_4", "level_5")
n_levels <- length(levels)

sites <- c("site_1", "site_2", "site_3", "site_4")
n_sites <- length(sites)

# Generate random values for each level that sum up to 100
get_values <- function() {
  rand_data <- Surrogate::RandVec(a = 0, b = 1, s = 1,
                                  n = n_levels, m = n_experts * n_sites)[[1]] |>
    round(2)

  diff <- colSums(rand_data) - 1

  # Correct last elements to sum up to 1
  rand_data[n_levels, ] <- rand_data[n_levels, ] - diff

  rand_data |>
    as.vector()
}

mechanism_1 <- data.frame(name = rep(random_names, each = n_levels * n_sites),
                          level = rep(levels, times = n_experts * n_sites),
                          site = rep(rep(sites, each = n_levels),
                                     times = n_experts),
                          confidence = rep(sample(seq(0, 100, by = 5),
                                                  size = n_experts * n_sites,
                                                  replace = TRUE),
                                           each = n_levels),
                          estimate = get_values()) |>
  tibble::as_tibble()

mechanism_2 <- data.frame(name = rep(random_names, each = n_levels * n_sites),
                          level = rep(levels, times = n_experts * n_sites),
                          site = rep(rep(sites, each = n_levels),
                                     times = n_experts),
                          confidence = rep(sample(seq(0, 100, by = 5),
                                                  size = n_experts * n_sites,
                                                  replace = TRUE),
                                           each = n_levels),
                          estimate = get_values()) |>
  dplyr::filter(name != random_names[[1]]) |>
  tibble::as_tibble()

mechanism_3 <- data.frame(name = rep(random_names, each = n_levels * n_sites),
                          level = rep(levels, times = n_experts * n_sites),
                          site = rep(rep(sites, each = n_levels),
                                     times = n_experts),
                          confidence = rep(sample(seq(0, 100, by = 5),
                                                  size = n_experts * n_sites,
                                                  replace = TRUE),
                                           each = n_levels),
                          estimate = get_values()) |>
  dplyr::filter(site != sites[[4]]) |>
  tibble::as_tibble()

usethis::use_data(mechanism_1, mechanism_2, mechanism_3,
                  internal = FALSE,
                  overwrite = TRUE,
                  version = 3)

# Save datasets into extdata folder
# 3 csv files, one for each dataset
write.csv(mechanism_1,
          file = file.path("inst", "extdata", "mechanism_1.csv"),
          row.names = FALSE)
write.csv(mechanism_2,
          file = file.path("inst", "extdata", "mechanism_2.csv"),
          row.names = FALSE)
write.csv(mechanism_3,
          file = file.path("inst", "extdata", "mechanism_3.csv"),
          row.names = FALSE)

# 1 xlsx file with two sheets
openxlsx::write.xlsx(list("Mechanism 1" = mechanism_1,
                          "Mechanism 2" = mechanism_2,
                          "Mechanism 3" = mechanism_3),
                     file = file.path("inst", "extdata", "mechanisms.xlsx"),
                     overwrite = TRUE)

# Save dataset in Google Sheets
# The following code needs to authorise the package to access the correct
# account. Uncomment the code below if you need to create a new file, if not,
# just use the code strings.
# gs <- googlesheets4::gs4_create("mechanisms",
#                                 sheets = c("Mechanism 1",
#                                            "Mechanism 2",
#                                            "Mechanism 3"))
gs <- "18VHeHB89P1s-6banaVoqOP-ggFmQZYx-z_31nMffAb8"
googlesheets4::sheet_write(mechanism_1,
                           ss = gs,
                           sheet = 1)
googlesheets4::sheet_write(mechanism_2,
                           ss = gs,
                           sheet = 2)
googlesheets4::sheet_write(mechanism_3,
                           ss = gs,
                           sheet = 3)

# !IMPORTANT! Go to Google Drive and change the Time column format to Date Time
# Once the files are created, go to Google Drive and make them public for view
# only
