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
          file = "inst/extdata/round_1.csv",
          row.names = FALSE)
write.csv(round_2,
          file = "inst/extdata/round_2.csv",
          row.names = FALSE)

# 1 xlsx file with two sheets
openxlsx::write.xlsx(list("Round 1" = round_1,
                          "Round 2" = round_2),
                     file = "inst/extdata/rounds.xlsx",
                     overwrite = TRUE)

# Save dataset in Google Sheets
# The following code needs to authorise the package to access the correct
# account. Uncomment the code below if you need to create a new file, if not,
# just use the code strings.
# ss1 <- googlesheets4::gs4_create("elicitation_round_1",
#                                 sheets = "Round 1")
gs1 <- "12lGIPa-jJOh3fogUDaERmkf04pVpPu9i8SloL2jAdqc"
googlesheets4::sheet_write(round_1,
                           ss = ss1,
                           sheet = 1)
# ss2 <- googlesheets4::gs4_create("elicitation_round_2",
#                                  sheets = "Round 2")
gs2 <- "1wImcfJYnC9a423jlxZiU_BFKXZpTZ7AIsZSxFtEsBQw"
googlesheets4::sheet_write(round_2,
                           ss = ss2,
                           sheet = 1)
# Once the files are created, go to google drive and make them public for view
# only
