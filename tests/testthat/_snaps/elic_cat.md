# elic_cat object

    structure(list(levels = c("level_1", "level_2"), sites = c("site_1", 
    "site_2", "site_3"), experts = 8, data = list(topic_1 = NULL, 
        topic_2 = NULL)), class = "elic_cat", title = "Title")

# Print elicit object

    Code
      new_elic_cat(levels = c("level_1", "level_2"), sites = c("site_1", "site_2",
        "site_3"), experts = 8, topics = c("topic_1", "topic_2"), title = "Title")
    Message
      
      -- Title --
      
      * Levels: "level_1" and "level_2"
      * Sites: "site_1", "site_2", and "site_3"
      * Number of experts: 8
      * Topics: "topic_1" and "topic_2"
      * Data available for 0 topics

---

    Code
      create_cat_obj()
    Message
      
      -- Elicitation --
      
      * Levels: "level_1", "level_2", "level_3", "level_4", and "level_5"
      * Sites: "site_1", "site_2", "site_3", and "site_4"
      * Number of experts: 6
      * Topics: "topic_1", "topic_2", and "topic_3"
      * Data available for topics "topic_1", "topic_2", and "topic_3"

