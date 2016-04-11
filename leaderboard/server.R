#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(googleAuthR)
library(googlesheets)
library(lubridate)
library(httr)

source('leaderboard.R')

jsonFile <- 'ShazamLeaderboard-5a8728fa6ef0.json'
secrets <- jsonlite::fromJSON(jsonFile)

service_token <- oauth_service_token(
  endpoint = oauth_endpoints("google"),
  secrets = secrets,
  scope = 
    "https://spreadsheets.google.com/feeds")

try(gs_auth(service_token))

doc <- gs_url('https://docs.google.com/spreadsheets/d/1D-T8NGO8gFWqbg2jGi5XPSRARCo03UgOLQYNZRiwz8k/edit?usp=sharing')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  # output$distPlot <- renderPlot({
  #   
  #   # generate bins based on input$bins from ui.R
  #   x    <- faithful[, 2] 
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #   
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
  #   
  # })
  
  output$score <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    score = getScore(inFile$datapath)
    gs_add_row(doc, ws = 'Scores', input = c('Toto', score, now()))
    data.frame(Score = score)
  })
  
})
