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
library(dplyr)

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

creds <- jsonlite::fromJSON('google_secret.json')
options(googleAuthR.webapp.client_id = creds$google_id,
        googleAuthR.webapp.client_secret = creds$google_secret,
        googleAuthR.scopes.selected = c('https://www.googleapis.com/auth/userinfo.profile',
                                        'https://www.googleapis.com/auth/userinfo.email'))

getUserInfos <- function() {
  f <- gar_api_generator('https://www.googleapis.com/oauth2/v1/userinfo',
                                    'GET',
                                    data_parse_function = function(x) {
                                      list(username = x$name,
                                           email = x$email)})
  f()
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  ## Get auth code from return URL
  access_token  <- reactiveAccessToken(session)
  
  ## Make a loginButton to display using loginOutput
  output$loginButton <- renderLogin(session, access_token())
  
  makeReactiveBinding('userInfos')
  
  api_output <- eventReactive(access_token, {
    ## with_shiny() wraps your your_api_function to provide the arguments
    ## requires you to pass "shiny_access_token"
    if(is.null(access_token())) {
      return(NULL)
    }
    userinfos <- with_shiny(getUserInfos, 
                        shiny_access_token = access_token())
    print(paste("I'm"), str(userinfos$username))
    userinfos
  })
  
  getScoreReactive <- eventReactive(input$submitButton, {
    isolate(inFile <- input$file1)
    
    if (is.null(inFile))
      return(NULL)
    
    isolate(comment <- input$comment)
    userInfos <- api_output()
    scores <- getScore(inFile$datapath)
    gs_add_row(doc, ws = 'Scores', input = data.frame(userInfos$email,
                                                      as.numeric(scores$Accuracy),
                                                      now(),
                                                      comment))
    scores
  })

  output$radar <- renderChartJSRadar({
    print('In renderChartJSRadar')
    # input$showDetail
    scores <- getScoreReactive()
    
    if(is.null(scores)) {
      return(NULL)
    }
    chartJSRadar(scores = scores$ByClass, maxScale = 1, scaleStepWidth = 0.25, responsive = TRUE, maintainAspectRatio = FALSE)
  })
  
  output$score <- renderTable({
    print('In renderTable')
    scores <- getScoreReactive()
    if(is.null(scores)) {
      return(NULL)
    }

    data.frame(Score = scores$Accuracy)
  })
  
  user <- reactive({
    curUser <- api_output()
    curUser$username
  })
  
  output$userName <- reactive({
    user()
  })
  
  output$email <- reactive({
    curUser <- api_output()
    curUser$email
  })
  
  output$showScore <- eventReactive(input$submitButton, {
    isolate(inFile <- input$file1)

    !is.null(inFile)
  })
  outputOptions(output, 'showScore', suspendWhenHidden=FALSE)

  output$authenticated <- reactive({
    currentUser <- api_output()
    print(paste('Current user is ', currentUser))
    !is.null(currentUser)
  })
  outputOptions(output, 'authenticated', suspendWhenHidden=FALSE)
  
  output$leaderboardTable <- renderTable({
    scores <- gs_read(doc, ws = 'Scores')
    by_user <- group_by(scores, User)
    scores_by_user <- summarise(by_user, max(Score))
    results <- scores_by_user[order(scores_by_user$`max(Score)`, decreasing = TRUE)]
    colnames(results) <- c('User', 'Score')
    results$User <- paste0(substr(results$User,1, 8), '...')
    results
  })
})
