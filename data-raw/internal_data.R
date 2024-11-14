var_labels <- list("1p" = "best",
                   "3p" = c("min", "max", "best"),
                   "4p" = c("min", "max", "best", "conf"))

usethis::use_data(var_labels,
                  internal = TRUE,
                  overwrite = TRUE,
                  version = 3)
