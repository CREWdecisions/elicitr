# Internal data----
var_labels <- list("1p" = "best",
                   "3p" = c("min", "max", "best"),
                   "4p" = c("min", "max", "best", "conf"))

usethis::use_data(var_labels,
                  internal = TRUE,
                  overwrite = TRUE,
                  version = 3)

# Exported data----
set.seed(25)
n <- 6
elicit <- data.frame(id = 1:n,
                     # 1 point estimate with integers
                     var1_best = mc2d::rpert(n,
                                             min = -10,
                                             mode = -1,
                                             max = 10) |>
                       as.integer(),
                     # 3 points estimate with positive integers
                     var2_best = mc2d::rpert(n,
                                             min = 0,
                                             mode = 20,
                                             max = 50) |>
                       as.integer(),
                     # 4 points estimate with probabilities
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
                var3_max = var3_best + sample(c(0.1, 0.2, 0.3),
                                              size = n,
                                              replace = TRUE),
                var3_conf = sample(50:100,
                                   size = n,
                                   replace = TRUE)) |>
  dplyr::select(id,
                var1_best,
                var2_min,
                var2_max,
                var2_best,
                var3_min,
                var3_max,
                var3_best,
                var3_conf) |>
  dplyr::mutate(dplyr::across(var3_min:var3_best, \(x) round(x, digits = 1))) |>
  tibble::as_tibble()

usethis::use_data(elicit,
                  internal = FALSE,
                  overwrite = TRUE,
                  version = 3)

# Save dataset into extdata folder
write.csv(elicit,
          file = "inst/extdata/elicit.csv",
          row.names = FALSE)

openxlsx::write.xlsx(elicit,
                     file = "inst/extdata/elicit.xlsx")

# Save dataset in Google Sheets
