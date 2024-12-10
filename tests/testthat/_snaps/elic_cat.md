# elic_cat object

    structure(list(levels = c("level_1", "level_2"), sites = c("site_1", 
    "site_2", "site_3"), experts = 8, data = list(mechanism_1 = NULL, 
        mechanism_2 = NULL)), class = "elic_cat", title = "Title")

# Print elicit object

    Code
      new_elic_cat(levels = c("level_1", "level_2"), sites = c("site_1", "site_2",
        "site_3"), experts = 8, mechanisms = c("mechanism_1", "mechanism_2"), title = "Title")
    Message
      
      -- Title --
      
      * Levels: "level_1" and "level_2"
      * Sites: "site_1", "site_2", and "site_3"
      * Number of experts: 8
      * Mechanismss: "mechanism_1" and "mechanism_2"
      * Data available for 0 mechanisms
