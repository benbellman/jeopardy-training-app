#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(rio)
library(shiny)
library(DT)
library(here)

#### Load data ####

# load questions and join with topics defined accordiing to category
topic_table <- import(here::here("data", "add_topics.csv"))
q_data <- import(here::here("data", "jeopardy_questions.csv")) %>% 
  left_join(topic_table)

# set lists of options
round_list <- c("Jeopardy! Round", "Double Jeopardy! Round", "Final Jeopardy! Round")
topic_list <- c("Any", unique(topic_table$topic))


# Define UI 
ui <- fluidPage(
   
   # Application title
   titlePanel("Jeopardy! Training App"),
   a("Questions from j-archive.com", href = "http://www.j-archive.com/"),
   
   # Sidebar with all interactive options 
   sidebarLayout(
      sidebarPanel(
        # Dropdown for round type
        selectInput(inputId = "round",
                    label = "Select Round",
                    choices = round_list,
                    selected = round_list[1]),
        br(),
        # Dropdown for category topic
        selectInput(inputId = "topic",
                    label = "Select Topic",
                    choices = topic_list, 
                    selected = topic_list[1]),
        br(),
        # Action button to get new question
        actionButton(inputId = "new_q",
                     label = "Get New Question")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        # first row contains question and answer panels
        fluidRow(h4("Category"),
                 verbatimTextOutput('q_category_amount'),   # REACTIVE
                 verbatimTextOutput('q_airdate')),  # REACTIVE, paste "Aired: " together in server section
                 
         
        # second row contains clue panel
        fluidRow(h4("Clue"), 
                 verbatimTextOutput('q_clue')),
        
        # third row contains answer panel
        fluidRow(h4("Answer"), 
                 conditionalPanel("input.show_answer % 2 == 1", 
                                  verbatimTextOutput('q_answer'))),


        # fourth row contains the button to show answer
        fluidRow(actionButton(inputId = "show_answer",
                              label = "Show/Hide Answer"))
        )
           
      )
)

# Define server logic 
server <- function(input, output) {
  
  # filter set of questions for selected round
  current_round <- reactive({
    req(input$round)
    filter(q_data, round == input$round)
  })
  
  # filter set of questions for selected topic
  current_topic <- reactive({
    req(input$topic)
    if(input$topic == "Any"){
      current_round()
    } else {
      filter(current_round(), topic == input$topic)
    }
  })
  
  ## define responses to "new question" action button
  # choose a new question, but return with empty answer (answer is saved in second list element)
  current_question <- eventReactive(input$new_q, {
    q <- sample_n(current_topic(), 1)
    #a <- q$answer[1]
    #q$answer[1] <- ""
    #list(info = q, answer = a)
    list(info = q)
  })
  
  
  #output$show_panel <- eventReactive(input$new_q, FALSE, ignoreInit = TRUE)
  
  #output$show_panel <- eventReactive(input$show_answer, TRUE, ignoreInit = TRUE)


  
  ## define response to "show answer" action button
  #current_question <- eventReactive(input$show_answer, {
  #  q <- current_question()$info
  #  q[1,"answer"] <- current_question()$answer
  #  list(info = q)
  #})

  
  ## define all display outputs
  # category and amount of question
  output$q_category_amount <- renderText({
    paste0(current_question()$info[1,"category"], " - $", current_question()$info[1,"amount"])
  })
  
  # category of question
  output$q_airdate <- renderText({
    paste0("Aired: ", current_question()$info[1,"date_noday"])
  })
  
  # clue of question
  output$q_clue <- renderText({
    current_question()$info[1,"question"]
  })
  
  # answer of question
  output$q_answer <- renderText({
    current_question()$info[1,"answer"]
  })
  



  # option for conditinal panel hiding output
  #outputOptions(output, "show_panel", suspendWhenHidden = FALSE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)



