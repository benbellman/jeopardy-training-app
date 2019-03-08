library(dplyr)
library(stringr)
library(lubridate)
library(rio)
library(here)

# load data
q_data <- import(here::here("data", "jeopardy_questions.csv"))

# categories
q_data$category <- str_remove(q_data$category, "</a>")
q_data$category <- str_remove(q_data$category, '<a.*">')
q_data$category <- str_replace_all(q_data$category, '"{2,}', '"')

# questions
q_data$question <- str_replace_all(q_data$question, "<br />", " ")
q_data$question <- str_replace_all(q_data$question, '"{2,}', '"')
#q_data$question <- str_replace(q_data$question, '<a href=".*wmv">', '(video) ')

# answers
q_data$answer <- str_replace_all(q_data$answer, '"{2,}', '"')

# re-format air dates
q_data$date_noday <- str_remove(q_data$date_aired, "^[A-Za-z]*day, ")
q_data$date_fmt <- mdy(q_data$date_noday)

# save data
export(q_data, here::here("data", "jeopardy_questions.csv"))
