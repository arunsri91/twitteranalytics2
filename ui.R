#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(theme = "boot.css",
  
  titlePanel("Sentiment Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      
      textInput("Trend","Enter The Trend:","#sample"),
      
      sliderInput("i", "Select no. of Tweets:", 0, 4000, 100, step = 50, round = FALSE, format = NULL, locale = NULL, ticks = TRUE, animate = FALSE, width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL, timezone = NULL, dragRange = TRUE),
      
      
      radioButtons("pType", "Select a Plot type:",
                   list("Emotion Across Time"='a', "Emotion Score"='b', "Frequent Topics"='c')),
      
      
      
      
      submitButton("Track!"),
      
      
      print(h6("  Under Construction!"))
      
      
      
    ),
    
    
    
    mainPanel(
      dataTableOutput("table"),
      plotOutput("plot")
    )
  ) )
)