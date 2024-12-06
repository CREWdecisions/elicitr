create_elicit_obj <- function() {
  elic_cont_start(var_names = c("var1", "var2", "var3"),
                  var_types = "ZNp",
                  elic_types = "134",
                  experts = 6,
                  verbose = FALSE) |>
    elic_cont_add_data(x, data_source = round_1, round = 1, verbose = FALSE) |>
    elic_cont_add_data(data_source = round_2, round = 2, verbose = FALSE)
}
