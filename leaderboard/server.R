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
library(radarchart)

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

source('google_secret.R')
options(googleAuthR.webapp.client_id = google_id,
        googleAuthR.webapp.client_secret = google_secret,
        googleAuthR.scopes.selected = c('https://www.googleapis.com/auth/userinfo.profile'))

getUserInfos <- function() {
  f <- gar_api_generator('https://www.googleapis.com/oauth2/v1/userinfo',
                                    'GET',
                                    data_parse_function = function(x) x$name)
  f()  
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  ## Get auth code from return URL
  access_token  <- reactiveAccessToken(session)
  
  ## Make a loginButton to display using loginOutput
  output$loginButton <- renderLogin(session, access_token())
  
  api_output <- eventReactive(access_token, {
    ## with_shiny() wraps your your_api_function to provide the arguments
    ## requires you to pass "shiny_access_token"
    if(is.null(access_token())) {
      return(NULL)
    }
    username <- with_shiny(getUserInfos, 
                        shiny_access_token = access_token())
    print(paste("I'm"), str(username))
    username
#    gs_add_row(doc, ws = 'Scores', input = c('Toto', score, now()))
#    data.frame(Score = score)
  })

  getScoreReactive <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    getScore(inFile$datapath)
  })
  
  output$score <- renderTable({
    scores <- getScoreReactive()
    if(is.null(scores)) {
      return(NULL)
    }
    
    data.frame(Score = scores$Accuracy)
  })

  output$radar <- renderChartJSRadar({
    scores <- getScoreReactive()
    
    if(is.null(scores)) {
      return(NULL)
    }
    chartJSRadar(scores = scores$ByClass, maxScale = 1, scaleStepWidth = 0.25, responsive = TRUE, maintainAspectRatio = FALSE)
  })
  
  user <- reactive({
    curUser <- api_output()
    print('coucou &*****************')
    curUser
  })
  
  output$userName <- reactive({
    user()
  })
  
  output$showScore <- reactive({
    inFile <- input$file1
    
    !is.null(inFile)
  })
  outputOptions(output, 'showScore', suspendWhenHidden=FALSE)

  output$authenticated <- reactive({
    currentUser <- api_output()
    print(paste('Current user is ', currentUser))
    !is.null(currentUser)
  })
  outputOptions(output, 'authenticated', suspendWhenHidden=FALSE)
  
})
