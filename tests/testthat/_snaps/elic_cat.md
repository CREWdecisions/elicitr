# elic_cat object

    structure(list(categories = c("category_1", "category_2"), sites = c("site_1", 
    "site_2", "site_3"), experts = 8, data = list(topic_1 = NULL, 
        topic_2 = NULL)), class = "elic_cat", title = "Title")

# Print elicit object

    Code
      new_elic_cat(categories = c("category_1", "category_2"), sites = c("site_1",
        "site_2", "site_3"), experts = 8, topics = c("topic_1", "topic_2"), title = "Title")
    Message
      
      -- Title --
      
      * Categories: "category_1" and "category_2"
      * Sites: "site_1", "site_2", and "site_3"
      * Number of experts: 8
      * Topics: "topic_1" and "topic_2"
      * Data available for 0 topics

---

    Code
      create_cat_obj()
    Message
      
      -- Elicitation --
      
      * Categories: "category_1", "category_2", "category_3", "category_4", and
      "category_5"
      * Sites: "site_1", "site_2", "site_3", and "site_4"
      * Number of experts: 6
      * Topics: "topic_1", "topic_2", and "topic_3"
      * Data available for topics "topic_1", "topic_2", and "topic_3"

