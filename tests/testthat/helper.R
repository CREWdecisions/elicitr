create_cont_obj <- function() {
  cont_start(var_names = c("var1", "var2", "var3"),
             var_types = "ZNp",
             elic_types = "134",
             experts = 6,
             verbose = FALSE) |>
    elic_cont_add_data(x, data_source = round_1, round = 1, verbose = FALSE) |>
    elic_cont_add_data(data_source = round_2, round = 2, verbose = FALSE)
}

create_cat_obj <- function() {
  my_levels <- c("level_1", "level_2", "level_3", "level_4", "level_5")
  my_sites <- c("site_1", "site_2", "site_3", "site_4")
  my_mechanisms <- c("mechanism_1", "mechanism_2", "mechanism_3")
  cat_start(levels = my_levels,
            sites = my_sites,
            experts = 6,
            mechanisms = my_mechanisms,
            verbose = FALSE) |>
    cat_add_data(data_source = mechanism_1,
                 mechanism = "mechanism_1",
                 verbose = FALSE) |>
    cat_add_data(data_source = mechanism_2,
                 mechanism = "mechanism_2",
                 verbose = FALSE) |>
    cat_add_data(data_source = mechanism_3,
                 mechanism = "mechanism_3",
                 verbose = FALSE)
}
