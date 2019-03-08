library(dplyr)
library(purrr)
library(stringr)
library(rio)
library(here)


#### set functions ####

# higher level function that takes url
get_questions <- function(url){
  
  # load page
  page_text <- suppressWarnings(readLines(url))
  
  # scrape the question information from that game
  categories <- page_text %>% 
    .[str_detect(., "category_name")] %>% 
    str_extract('category_name\">.*</td') %>% 
    str_remove('category_name\">') %>% 
    str_remove('</td') %>% 
    str_replace_all('&quot;', '"') %>% 
    str_replace_all('&amp;', '&') %>% 
    str_remove_all('<em class="underline">') %>% 
    str_remove_all('</em>')
  
  questions <- page_text %>% 
    .[str_detect(., "clue_text")] %>% 
    str_extract('>.*<') %>% 
    str_replace('<a href=\".*\" target=\"_blank\">', '(image) ') %>% 
    str_remove_all("</a>") %>% 
    str_remove_all("\\\\") %>% 
    str_remove_all("&lt;i&gt;") %>% 
    str_remove_all("&lt;/i&gt;") %>% 
    str_replace_all('&quot;', '"') %>% 
    str_replace_all('&amp;', '&') %>% 
    str_remove("^>") %>% 
    str_remove("<$")
  
  answers <- page_text %>% 
    str_extract('correct_response&quot;&gt;.*&lt;/em') %>% 
    .[is.na(.) == F] %>% 
    str_remove("correct_response&quot;&gt;") %>% 
    str_remove("&lt;/em") %>% 
    str_remove_all("\\\\") %>% 
    str_remove_all("&lt;i&gt;") %>% 
    str_remove_all("&lt;/i&gt;") %>% 
    str_replace_all('&quot;', '"') %>% 
    str_replace_all('&amp;', '&')
  
  final_answer <- page_text %>% 
    str_extract('correct_response\\\\&quot;&gt;.*&lt;/em&gt;') %>% 
    .[is.na(.) == F] %>% 
    str_remove("correct_response\\\\&quot;&gt;") %>% 
    str_remove("&lt;/em&gt;") %>% 
    str_remove_all("\\\\") %>% 
    str_remove_all("&lt;i&gt;") %>% 
    str_remove_all("&lt;/i&gt;") %>% 
    str_replace_all('&quot;', '"') %>% 
    str_replace_all('&amp;', '&')
  
  answers <- c(answers, final_answer)
  
  date_aired <- page_text[22] %>% 
    str_extract('<h1>.*</h1>') %>% 
    str_remove_all("</?h1>") %>% 
    str_split(" - ", simplify = T) %>% 
    .[1,2]
  
  # organize into a single data table
  output <- process_questions(categories, questions, answers)
  output$date_aired <- date_aired
  
  # return table of questions
  return(output)
}


# function for processing Q information into tabular data
process_questions <- function(categories, questions, answers){
  
  # create proper vector of categories to match order of questions
  categories_vec <- c(rep(categories[1:6], 5),
                      rep(categories[7:12], 5),
                      categories[13])
  
  # create vector of dollar amounts
  dollars <- c(rep(200, 6),
               rep(400, 6),
               rep(600, 6),
               rep(800, 6),
               rep(1000, 6))
  
  amounts <- c(dollars, 2*dollars, NA)
  
  # create vector of rounds
  rounds <- c(rep("Jeopardy! Round", 30),
             rep("Double Jeopardy! Round", 30),
             "Final Jeopardy! Round")
  
  tibble(
    question = questions,
    answer = answers,
    category = categories_vec,
    amount = amounts,
    round = rounds
  )
}

# safe version of readLines in case server query fails
get_questions_safe <- possibly(get_questions, otherwise = NA)

#### Do the scraping ####

# set up object to hold results
list_of_shows <- list()

# set up loop through 2000 some pages
for(a in 943:2000){
  
  # set up url
  game_id <- a + 4000
  url <- paste0("http://www.j-archive.com/showgame.php?game_id=", game_id)
  
  # scrape questions from page
  list_of_shows[[a]] <- get_questions_safe(url)
  
  # wait 3 seconds to ease load on server
  Sys.sleep(3)
}


# combine and fix dollar amounts
# create vector of dollar amounts
#dollars <- c(rep(200, 6),
#             rep(400, 6),
#             rep(600, 6),
#             rep(800, 6),
#             rep(1000, 6))

q_data <- list_of_shows[is.na(list_of_shows) == F] %>% 
  #map(mutate, amount = c(dollars, 2*dollars, NA)) %>% 
  bind_rows()


# save scraped question data
export(q_data, here("data", "jeopardy_questions_master_copy.csv"))





