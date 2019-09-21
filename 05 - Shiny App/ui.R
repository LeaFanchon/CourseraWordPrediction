
library(shiny)

# Define UI 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Data Science Capstone Project - Word Prediction"),
  
  # Model parameters
  sidebarLayout(
    sidebarPanel(
      
       textInput("sentence",
                 "Type your sentence:",
                 value = "Please wait a"),
      
       sliderInput("ALPHA",
                   "Alpha (Stupid Back-Off parameter):",
                   min = 0,
                   max = 1,
                   value = 0.4),
       
       sliderInput("WEIGHT_NGRAMS",
                   "Weight of the Stupid Back Off model:",
                   min = 0,
                   max = 1,
                   value = 1),
       
       sliderInput("WEIGHT_CONTEXT",
                   "Weight of the context:",
                   min = 0,
                   max = 1,
                   value = 1),
       
       sliderInput("NUMBER_RESULTS",
                   "Number of suggested outputs:",
                   min = 1,
                   max = 20,
                   value = 5),
       
       checkboxInput("STOPWORDS", "Keep stopwords", value=TRUE)
    ),
    
    mainPanel(
      tableOutput("prediction")
    )
  )
))
