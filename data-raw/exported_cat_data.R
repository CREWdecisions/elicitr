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
  Surrogate::RandVec(a = 0, b = 100, s = 100,
                     n = n_levels, m = n_experts * n_sites)[[1]] |>
    as.vector()
}


mechanism_1 <- data.frame(expert = rep(random_names, each = n_levels * n_sites),
                          level = rep(levels, times = n_experts * n_sites),
                          site = rep(rep(sites, each = n_levels),
                                     times = n_experts),
                          uncertatnty = rep(sample(1:100, n_experts * n_sites),
                                            each = n_levels),
                          value = get_values())

mechanism_2 <- data.frame(expert = rep(random_names, each = n_levels * n_sites),
                          level = rep(levels, times = n_experts * n_sites),
                          site = rep(rep(sites, each = n_levels),
                                     times = n_experts),
                          uncertatnty = rep(sample(1:100, n_experts * n_sites),
                                            each = n_levels),
                          value = get_values())

usethis::use_data(mechanism_1, mechanism_2,
                  internal = FALSE,
                  overwrite = TRUE,
                  version = 3)

# Save datasets into extdata folder
# 2 csv files, one for each dataset
write.csv(mechanism_1,
          file = file.path("inst", "extdata", "mechanism_1.csv"),
          row.names = FALSE)
write.csv(mechanism_2,
          file = file.path("inst", "extdata", "mechanism_2.csv"),
          row.names = FALSE)

# 1 xlsx file with two sheets
openxlsx::write.xlsx(list("Mechanism 1" = mechanism_1,
                          "Mechanism 2" = mechanism_2),
                     file = file.path("inst", "extdata", "mechanisms.xlsx"),
                     overwrite = TRUE)

# Save dataset in Google Sheets
# The following code needs to authorise the package to access the correct
# account. Uncomment the code below if you need to create a new file, if not,
# just use the code strings.
# gs <- googlesheets4::gs4_create("mechanisms",
#                                 sheets = c("Mechanism 1", "Mechanism 2"))
gs <- "18VHeHB89P1s-6banaVoqOP-ggFmQZYx-z_31nMffAb8"
googlesheets4::sheet_write(mechanism_1,
                           ss = gs,
                           sheet = 1)
googlesheets4::sheet_write(mechanism_2,
                           ss = gs,
                           sheet = 2)

# !IMPORTANT! Go to Google Drive and change the Time column format to Date Time
# Once the files are created, go to Google Drive and make them public for view
# only
