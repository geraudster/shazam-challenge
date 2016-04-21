#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(radarchart)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel('«Shazam» de la ville - Leaderboard'),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = '!output.authenticated',
                       h2('Please login')),
      conditionalPanel(condition = 'output.authenticated',
                       p('Hello', textOutput('userName', inline = TRUE))),

      loginOutput('loginButton')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # loginOutput("loginButton"),
      # p(textOutput('condition')),
      conditionalPanel(condition = 'output.authenticated',
                       fileInput('file1', 'Choose submission file',
                                 accept = c(
                                   'text/csv',
                                   'text/comma-separated-values',
                                   'text/tab-separated-values',
                                   'text/plain',
                                   '.csv',
                                   '.tsv'
                                 )),
                       conditionalPanel(condition = 'output.showScore',
                                        h2('Your score: '),
                                        tableOutput('score'),
                                        h2('Balanced accuracy by class: '),
                                        div(
                                          chartJSRadarOutput('radar'), width = "200", height = "200"))),
      width = 7
      # textOutput('userName'),
      # actionButton('submit', 'Get name')
    )
  )
))
