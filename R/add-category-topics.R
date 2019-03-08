library(dplyr)
library(stringr)
library(rio)
library(here)

# load data
q_data <- import(here("data", "jeopardy_questions_master_copy.csv"))

# save table to classify categories by hand
tibble(
  category = unique(q_data$category),
  topic = ""
) %>% 
  export(here("data", "add_topics_blank.csv"))

# load edited topics file
topics <- import(here("data", "topics.csv"))