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
    # The following function rounds preserving the sum of the values on each
    # row, so data needs to be transposed twice, before and after the function
    t() |>
    miceadds::sumpreserving.rounding(digits = 2, preserve = TRUE) |>
    t()

  rand_data |>
    as.vector()
}

topic_1 <- data.frame(name = rep(random_names, each = n_levels * n_sites),
                      level = rep(levels, times = n_experts * n_sites),
                      site = rep(rep(sites, each = n_levels),
                                 times = n_experts),
                      confidence = rep(sample(seq(0, 100, by = 5),
                                              size = n_experts * n_sites,
                                              replace = TRUE),
                                       each = n_levels),
                      estimate = get_values()) |>
  tibble::as_tibble()

topic_2 <- data.frame(name = rep(random_names, each = n_levels * n_sites),
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

topic_3 <- data.frame(name = rep(random_names, each = n_levels * n_sites),
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

usethis::use_data(topic_1, topic_2, topic_3,
                  internal = FALSE,
                  overwrite = TRUE,
                  version = 3)

# Save datasets into extdata folder
# 3 csv files, one for each dataset
write.csv(topic_1,
          file = file.path("inst", "extdata", "topic_1.csv"),
          row.names = FALSE)
write.csv(topic_2,
          file = file.path("inst", "extdata", "topic_2.csv"),
          row.names = FALSE)
write.csv(topic_3,
          file = file.path("inst", "extdata", "topic_3.csv"),
          row.names = FALSE)

# 1 xlsx file with two sheets
openxlsx::write.xlsx(list("Topic 1" = topic_1,
                          "Topic 2" = topic_2,
                          "Topic 3" = topic_3),
                     file = file.path("inst", "extdata", "topics.xlsx"),
                     overwrite = TRUE)

# Save dataset in Google Sheets
# The following code needs to authorise the package to access the correct
# account. Uncomment the code below if you need to create a new file, if not,
# just use the code strings.
# gs <- googlesheets4::gs4_create("topics",
#                                 sheets = c("Topic 1",
#                                            "Topic 2",
#                                            "Topic 3"))
gs <- "18VHeHB89P1s-6banaVoqOP-ggFmQZYx-z_31nMffAb8"
googlesheets4::sheet_write(topic_1,
                           ss = gs,
                           sheet = 1)
googlesheets4::sheet_write(topic_2,
                           ss = gs,
                           sheet = 2)
googlesheets4::sheet_write(topic_3,
                           ss = gs,
                           sheet = 3)

# !IMPORTANT! Go to Google Drive and change the Time column format to Date Time
# Once the files are created, go to Google Drive and make them public for view
# only
