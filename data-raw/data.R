# Exported data----
set.seed(25)
n <- 6
elicit <- data.frame(id = 1:n,
                     # 1 point estimate with percentages
                     var1_best = runif(n,
                                       min = 0.7,
                                       max = 1),
                     # 3 points estimate with positive numbers
                     var2_best = truncnorm::rtruncnorm(n,
                                                       a = 0,
                                                       b = 100,
                                                       mean = 50,
                                                       sd = 10),
                     # 4 points estimate with natural numbers
                     var3_best = truncnorm::rtruncnorm(n,
                                                       a = -20,
                                                       b = 20,
                                                       mean = 0,
                                                       sd = 10)) |>
  dplyr::mutate(var2_min = var2_best - runif(n,
                                             min = 0.1,
                                             max = 0.3),
                var2_max = var2_best + runif(n,
                                             min = 0.1,
                                             max = 0.3),
                var3_min = var3_best - truncnorm::rtruncnorm(n,
                                                             a = -10,
                                                             b = 10,
                                                             mean = 0,
                                                             sd = 5),
                var3_max = var3_best + truncnorm::rtruncnorm(n,
                                                             a = -10,
                                                             b = 10,
                                                             mean = 0,
                                                             sd = 5),
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
  dplyr::mutate(var1_best = round(var1_best, digits = 1),
                dplyr::across(var2_min:var3_best, round))

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
