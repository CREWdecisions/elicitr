# elic_cat object

    structure(list(categories = c("category_1", "category_2"), options = c("option_1", 
    "option_2", "option_3"), experts = 8, data = list(topic_1 = NULL, 
        topic_2 = NULL)), class = "elic_cat", title = "Title")

# Print elicit object

    Code
      new_elic_cat(categories = c("category_1", "category_2"), options = c("option_1",
        "option_2", "option_3"), experts = 8, topics = c("topic_1", "topic_2"),
      title = "Title")
    Message
      
      -- Title --
      
      * Categories: "category_1" and "category_2"
      * Options: "option_1", "option_2", and "option_3"
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
      * Options: "option_1", "option_2", "option_3", and "option_4"
      * Number of experts: 6
      * Topics: "topic_1", "topic_2", and "topic_3"
      * Data available for topics "topic_1", "topic_2", and "topic_3"

