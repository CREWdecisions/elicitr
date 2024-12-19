create_cont_obj <- function() {
  cont_start(var_names = c("var1", "var2", "var3"),
             var_types = "ZNp",
             elic_types = "134",
             experts = 6,
             verbose = FALSE) |>
    cont_add_data(x, data_source = round_1, round = 1, verbose = FALSE) |>
    cont_add_data(data_source = round_2, round = 2, verbose = FALSE)
}

create_cat_obj <- function() {
  my_categories <- c("category_1", "category_2", "category_3",
                     "category_4", "category_5")
  my_sites <- c("site_1", "site_2", "site_3", "site_4")
  my_topics <- c("topic_1", "topic_2", "topic_3")
  cat_start(categories = my_categories,
            sites = my_sites,
            experts = 6,
            topics = my_topics,
            verbose = FALSE) |>
    cat_add_data(data_source = topic_1,
                 topic = "topic_1",
                 verbose = FALSE) |>
    cat_add_data(data_source = topic_2,
                 topic = "topic_2",
                 verbose = FALSE) |>
    cat_add_data(data_source = topic_3,
                 topic = "topic_3",
                 verbose = FALSE)
}
