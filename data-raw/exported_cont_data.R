set.seed(25)
n <- 6

random_names <- randomNames::randomNames(n,
                                         which.names = "both",
                                         name.order = "first.last",
                                         name.sep = " ")

round_1 <- data.frame(name = random_names,
                      # 1 point elicitation with integers
                      var1_best = mc2d::rpert(n,
                                              min = -10,
                                              mode = -1,
                                              max = 10) |>
                        as.integer(),
                      # 3 points elicitation with positive integers
                      var2_best = mc2d::rpert(n,
                                              min = 0,
                                              mode = 20,
                                              max = 50) |>
                        as.integer(),
                      # 4 points elicitation with probabilities
                      var3_best = mc2d::rpert(n,
                                              min = 0.6,
                                              mode = 0.7,
                                              max = 1)) |>
  dplyr::mutate(var2_min = var2_best - sample(1:6,
                                              size = n,
                                              replace = TRUE),
                var2_max = var2_best + sample(1:6,
                                              size = n,
                                              replace = TRUE),
                var3_min = var3_best - sample(c(0.1, 0.2, 0.3),
                                              size = n,
                                              replace = TRUE),
                var3_max = var3_best + sample(c(0.1, 0.2),
                                              size = n,
                                              replace = TRUE),
                var3_conf = sample(50:100,
                                   size = n,
                                   replace = TRUE)) |>
  dplyr::select(name,
                var1_best,
                var2_min,
                var2_max,
                var2_best,
                var3_min,
                var3_max,
                var3_best,
                var3_conf) |>
  dplyr::mutate(dplyr::across(var3_min:var3_best, \(x) round(x, digits = 2))) |>
  tibble::as_tibble()

# Narrow values closer to the mode
round_2 <- data.frame(name = random_names,
                      # 1 point elicitation with integers
                      var1_best = mc2d::rpert(n,
                                              min = -5,
                                              mode = -1,
                                              max = 5) |>
                        as.integer(),
                      # 3 points elicitation with positive integers
                      var2_best = mc2d::rpert(n,
                                              min = 0,
                                              mode = 20,
                                              max = 25) |>
                        as.integer(),
                      # 4 points elicitation with probabilities
                      var3_best = mc2d::rpert(n,
                                              min = 0.6,
                                              mode = 0.7,
                                              max = 0.9)) |>
  dplyr::mutate(var2_min = var2_best - sample(1:4,
                                              size = n,
                                              replace = TRUE),
                var2_max = var2_best + sample(1:4,
                                              size = n,
                                              replace = TRUE),
                var3_min = var3_best - sample(c(0.1, 0.2),
                                              size = n,
                                              replace = TRUE),
                var3_max = var3_best + sample(c(0.1, 0.2),
                                              size = n,
                                              replace = TRUE),
                var3_conf = sample(70:100,
                                   size = n,
                                   replace = TRUE)) |>
  dplyr::select(name,
                var1_best,
                var2_min,
                var2_max,
                var2_best,
                var3_min,
                var3_max,
                var3_best,
                var3_conf) |>
  dplyr::mutate(dplyr::across(var3_min:var3_best, \(x) round(x, digits = 2))) |>
  dplyr::slice_sample(n = n) |>
  tibble::as_tibble()

usethis::use_data(round_1, round_2,
                  internal = FALSE,
                  overwrite = TRUE,
                  version = 3)

# Save datasets into extdata folder
# 2 csv files, one for each dataset
write.csv(round_1,
          file = file.path("inst", "extdata", "round_1.csv"),
          row.names = FALSE)
write.csv(round_2,
          file = file.path("inst", "extdata", "round_2.csv"),
          row.names = FALSE)

# 1 xlsx file with two sheets
openxlsx::write.xlsx(list("Round 1" = round_1,
                          "Round 2" = round_2),
                     file = file.path("inst", "extdata", "rounds.xlsx"),
                     overwrite = TRUE)

# Save dataset in Google Sheets
# Generate random timestamps simulating answers given within 30 seconds and 3
# minutes (when data are collected with a Google Forms, they are saved in
# Google Sheets and the timestamps is added as first column)
times <- runif(n, min = 30, max = 180) |>
  as.POSIXct(origin = "2024-11-24 14:00:00")
new_time <- (times[[3]] + 66) |>
  format("%d-%m-%Y %H:%M:%S")
times <- times |>
  format("%d-%m-%Y %H:%M:%S")

rnd_1 <- round_1 |>
  dplyr::mutate("Time" = times, .before = 1)
rnd_2 <- round_2 |>
  dplyr::mutate("Time" = times, .before = 1)
# The following code needs to authorise the package to access the correct
# account. Uncomment the code below if you need to create a new file, if not,
# just use the code strings.
# gs1 <- googlesheets4::gs4_create("elicitation_round_1",
#                                 sheets = "Round 1")
gs1 <- "12lGIPa-jJOh3fogUDaERmkf04pVpPu9i8SloL2jAdqc"
googlesheets4::sheet_write(rnd_1,
                           ss = gs1,
                           sheet = 1)
# gs2 <- googlesheets4::gs4_create("elicitation_round_2",
#                                  sheets = "Round 2")
gs2 <- "1wImcfJYnC9a423jlxZiU_BFKXZpTZ7AIsZSxFtEsBQw"
googlesheets4::sheet_write(rnd_2,
                           ss = gs2,
                           sheet = 1)
# This is used only for testing
rnd <- rnd_1 |>
  dplyr::select(1:2) |>
  dplyr::mutate("var1_best" = c("0.1", ".2", "0.3", "0,4", "0.5", "0,6"),
                "var2_best" = c("0.1", "1", "0.3", "0.4", "0.5", "0.6")) |>
  dplyr::add_row(Time = new_time,
                 name = rnd_1[[3, 2]],
                 var1_best = "0.4",
                 var2_best = "0.3")
# gs3 <- googlesheets4::gs4_create("test_file") nolint
gs3 <- "1broW_vnD1qDbeXqWxcuijOs7386m2zXNM7yw9mh5RJg"
googlesheets4::sheet_write(rnd,
                           ss = gs3,
                           sheet = 1)
# !IMPORTANT! Go to Google Drive and change the Time column format to Date Time
# Once the files are created, go to Google Drive and make them public for view
# only
