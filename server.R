server <- function(session, input, output) {
  
  today <- "2021-07-01 23:00:00"
  dataIn <- readResults(today)
  allCompetitors <- unique(dataIn$games$competitor)
  allKnockOutOptions <- c("8th final","Quarter final","Semi final","Final","Winner")
  updateSelectInput(session,"knockOutStageTeamsLeft", choices = allKnockOutOptions, selected = allKnockOutOptions[1])
  updateSelectInput(session,"knockOutStage", choices = allKnockOutOptions, selected = allKnockOutOptions[1])
  updateSelectInput(session, 'competitor', choices = allCompetitors, selected = allCompetitors)
  
  # today <- Sys.time()
  # today <- format(today, format="%Y-%m-%d %H:%M:%S")
  # today <- "2021-07-20 23:00:00"
  todayPlot <- paste(substr(today,1,10),"00:00:00")
  lastPlotDate <- min(todayPlot, "2021-07-19 00:00:00")
  print(lastPlotDate)
  
  
  scoring <- dataIn$scoring
  games <- dataIn$games
  results <- dataIn$results
  yellowCards <- dataIn$yellowCards
  topScorer <- dataIn$topScorer
  topScorerActual <- dataIn$topScorerActual
  todayRowInds <- max(which(results$Date<=today))
  print(todayRowInds)
  
  
  
  currentScore <- calcCurrentScore(games, results, scoring, yellowCards, topScorer, today)
  
  observeEvent(input$fillAllCompetitors,{
    allCompetitors <- unique(games$competitor)
    print(allCompetitors)
    updateSelectInput(session,"competitor",choices = allCompetitors, selected = allCompetitors)
  })
  observeEvent(input$removeAllCompetitors,{
    allCompetitors <- unique(games$competitor)
    updateSelectInput(session,"competitor",choices = allCompetitors, selected = NULL)
  })
  
  observeEvent(input$simulate,{
    results <- if(!is.null(input$simulator)){hot_to_r(input$simulator)}
    results$Date <- as.POSIXct(results$Date)
    noOfSimDays <- input$noOfSimDays
    todaySim <- format(as.POSIXct(today)+noOfSimDays*24*3600,format="%Y-%m-%d %H:%M:%S")
    lastPlotDateSim <- min(todaySim, "2021-07-15 00:00:00")
    currentScore <- calcCurrentScore(games, results, scoring, yellowCards, topScorer, todaySim)
    
    leaderBoardDataSim <- reactive({
      currentScore$leaderBoard[which(currentScore$leaderBoard$competitor %in% input$competitor),]
    })
    groupMatches_score_playedSim <- reactive({
      currentScore$groupMatches[which(currentScore$groupMatches$competitor %in% input$competitor & currentScore$groupMatches$Date<=todaySim),]
    })
    groupMatches_score_unplayedSim <- reactive({
      currentScore$groupMatches[which(currentScore$groupMatches$competitor %in% input$competitor & currentScore$groupMatches$Date>todaySim),c("competitor","Date","Group","Home_Team","Away_Team","Home_guess","Away_guess")]
    })
    scoreGraphDataSim <- reactive({
      currentScore$scoringTable[which(currentScore$scoringTable$competitor %in% input$competitor),]
    })
    print(scoreGraphDataSim())
    
    output$leaderBoard <- renderDataTable(leaderBoardDataSim(), options = list(pageLength = 15),rownames = FALSE)
    output$group_matches_played <- renderDataTable(datatable(groupMatches_score_playedSim(), options = list(pageLength = 25,order = list(list(1, 'desc'), list(0, 'asc'))),rownames = FALSE) %>% 
                                                     formatStyle("score",target="row", backgroundColor = styleInterval(c(1),c("","lightgray"))))
    
    output$group_matches_unplayed <- renderDataTable(datatable(groupMatches_score_unplayedSim(), options = list(pageLength = 25,order = list(list(1, 'asc'), list(0, 'asc'))),rownames = FALSE)) 
    output$allscores <- renderDataTable(currentScore$scoringTable[which(currentScore$scoringTable$competitor %in% input$competitor),],rownames = FALSE,options = list(pageLength = 25))
    
    
    output$scoreGraph <- renderPlotly(plot_ly(scoreGraphDataSim(),x=~Date, y=~totScore, type = "scatter", mode="lines", color = ~competitor)%>%
                                        layout(height = 700, legend = list(orientation = "h", xanchor = "center", y=-0.15, x=0.5), title = "Aggregated scores", yaxis = list(title = "Score")
                                               , xaxis = list(title = "Score as at date", range = c("2021-06-10 00:00:00",lastPlotDateSim))))
    
    updateTabsetPanel(session, "scoring_summaries", selected = "leaderboard")
  })
  
  
  groupMatches_score_played <- reactive({
    currentScore$groupMatches[which(currentScore$groupMatches$competitor %in% input$competitor & currentScore$groupMatches$Date<=today),]
  })
  groupMatches_score_unplayed <- reactive({
    currentScore$groupMatches[which(currentScore$groupMatches$competitor %in% input$competitor & currentScore$groupMatches$Date>today),c("competitor","Date","Group","Home_Team","Away_Team","Home_guess","Away_guess")]
  })
  final8_score <- reactive({
    currentScore$allFinalMatches[which(currentScore$allFinalMatches$competitor %in% input$competitor & currentScore$allFinalMatches$Stage == "8th final"),]
  })
  final8_scoreActual <- reactive({
    currentScore$allFinalMatchesActual[which(currentScore$allFinalMatchesActual$Stage == "8th final"),]
  })
  final4_score <- reactive({
    currentScore$allFinalMatches[which(currentScore$allFinalMatches$competitor %in% input$competitor & currentScore$allFinalMatches$Stage == "Quarter final"),]
  })
  final4_scoreActual <- reactive({
    currentScore$allFinalMatchesActual[which(currentScore$allFinalMatchesActual$Stage == "Quarter final"),]
  })
  final2_score <- reactive({
    currentScore$allFinalMatches[which(currentScore$allFinalMatches$competitor %in% input$competitor & currentScore$allFinalMatches$Stage == "Semi final"),]
  })
  final2_scoreActual <- reactive({
    currentScore$allFinalMatchesActual[which(currentScore$allFinalMatchesActual$Stage == "Semi final"),]
  })
  final1_score <- reactive({
    currentScore$allFinalMatches[which(currentScore$allFinalMatches$competitor %in% input$competitor & currentScore$allFinalMatches$Stage == "Final"),]
  })
  final1_scoreActual <- reactive({
    currentScore$allFinalMatchesActual[which(currentScore$allFinalMatchesActual$Stage == "Final"),]
  })
  scoreGraphData <- reactive({
    currentScore$scoringTable[which(currentScore$scoringTable$competitor %in% input$competitor),]
  })
  leaderBoardData <- reactive({
    currentScore$leaderBoard[which(currentScore$leaderBoard$competitor %in% input$competitor),]
  })
  knockOutData <- reactive({
    currentScore$allFinalMatchesFreq[which(currentScore$allFinalMatchesFreq$Stage == input$knockOutStage),]
  })
  aveGoalsData <- reactive({
    currentScore$aveGoals[which(currentScore$aveGoals$competitor %in% input$competitor),]
  })
  teamsLeftData <- reactive({
    currentScore$teamsPerStage[which(currentScore$teamsPerStage$Stage == input$knockOutStageTeamsLeft),]
  })
  
  
  output$group_matches_played <- renderDataTable(datatable(groupMatches_score_played(), options = list(pageLength = 25,order = list(list(1, 'desc'), list(0, 'asc'))),rownames = FALSE) %>% 
                                                   formatStyle("score",target="row", backgroundColor = styleInterval(c(1),c("","lightgray"))))
  output$group_matches_unplayed <- renderDataTable(datatable(groupMatches_score_unplayed(), options = list(pageLength = 25,order = list(list(1, 'asc'), list(0, 'asc'))),rownames = FALSE)) 
  
  output$final8 <- renderDataTable(datatable(final8_score(), filter = "top", options = list(pageLength = 25),rownames = FALSE) %>% formatStyle("score",target="row", backgroundColor = styleInterval(c(1),c("","lightgray"))))
  output$final8Actual <- renderDataTable(final8_scoreActual(), options = list(pageLength = 16),rownames = FALSE)
  output$final4 <- renderDataTable(datatable(final4_score(), filter = "top", options = list(pageLength = 25),rownames = FALSE) %>% formatStyle("score",target="row", backgroundColor = styleInterval(c(1),c("","lightgray"))))
  output$final4Actual <- renderDataTable(final4_scoreActual(),rownames = FALSE)
  output$final2 <- renderDataTable(datatable(final2_score(), filter = "top", options = list(pageLength = 25),rownames = FALSE) %>% formatStyle("score",target="row", backgroundColor = styleInterval(c(1),c("","lightgray"))))
  output$final2Actual <- renderDataTable(final2_scoreActual(),rownames = FALSE)
  output$final1 <- renderDataTable(datatable(final1_score(), filter = "top", options = list(pageLength = 25),rownames = FALSE) %>% formatStyle("score",target="row", backgroundColor = styleInterval(c(1),c("","lightgray"))))
  output$final1Actual <- renderDataTable(final1_scoreActual(),rownames = FALSE)
  output$winner <- renderDataTable(currentScore$allWinners, options = list(pageLength = 25), filter = "top",rownames = FALSE)
  output$winnerActual <- renderDataTable(currentScore$winnerActual,rownames = FALSE)
  output$allscores <- renderDataTable(currentScore$scoringTable[which(currentScore$scoringTable$competitor %in% input$competitor),],rownames = FALSE,options = list(pageLength = 25))
  output$scoreGraph <- renderPlotly(plot_ly(scoreGraphData(),x=~Date, y=~totScore, type = "scatter", mode="lines", color = ~competitor)%>%
                                      layout(height = 700, legend = list(orientation = "h", xanchor = "center", y=-0.15, x=0.5), title = "Aggregated scores", yaxis = list(title = "Score")
                                             , xaxis = list(title = "Score as at date", range = c("2021-06-10 00:00:00",lastPlotDate))))
  
  output$leaderBoard <- renderDataTable(leaderBoardData(), options = list(pageLength = 15),rownames = FALSE)
  output$yellowCardsActual <- renderDataTable(currentScore$yellowCardsActual, colnames = c(" ", "competitor", "Yellow cards until today"))
  output$topScorerActual <- renderDataTable(topScorerActual, colnames = c("Player", "Number of goals"),rownames = FALSE)
  
  
  rendertext <- paste0("function (instance, td, row, col, prop, value, cellProperties) {Handsontable.renderers.NumericRenderer.apply(this, arguments);
                                if (row >= ",todayRowInds," & col >=7 & col <= 8) {     td.style.background = 'lightgrey';}}")
  print(rendertext)
  output$simulator <- renderRHandsontable(rhandsontable(cbind(results[,c("Group","Game_No","Round_Number")],
                                                              "Date" = as.character(results[,c("Date")]),
                                                              results[,c("Location","Home_Team","Away_Team","Home_actual","Away_actual","competitor")],
                                                              "finishDate" = as.character(results[,c("finishDate")])),rowHeaders = NULL
  ) %>% hot_col(c(1:7), readOnly = TRUE) %>% 
    hot_cols(renderer =  rendertext))



           #     paste0()))

  output$yellowcards <- renderDataTable(currentScore$yellowCards, options = list(pageLength = 25),rownames = FALSE, colnames = c("competitor", "yellow cards guess", " current difference"))
  output$topscorers <- renderDataTable(currentScore$topScorer, options = list(pageLength = 25),rownames = FALSE)
  
  output$aveGoals <- renderPlotly(plot_ly(aveGoalsData(), x=~competitor, y=~Mean_Goals, type="bar") %>% layout(height = 800, legend = list(orientation = "h", xanchor = "center", y=-0.15, x=0.5), 
                                                                                                               
                                                                                                               xaxis = list(title = ""),margin = list(b=160)))
  # output$teamsPerStage <- renderPlotly({p <- plot_ly(teamsLeftData(),x=~competitor, y=~noOfTeams, type="bar")%>%layout(xaxis = list(title=""), margin = list(b=160))})
  observe({
    
    dataIn <- teamsLeftData()
    if(is.null(dataIn)){
      return(NULL)
    }
    dataIn <- remove.factors(dataIn)
    dataIn$Teamsort <- factor(dataIn$competitor, levels = unique(dataIn$competitor)[order(dataIn$noOfTeams, decreasing = TRUE)])
    output$teamsPerStage <- renderPlotly({
      p <- plot_ly(dataIn,x=~Teamsort, y=~noOfTeams, type="bar")%>%layout(xaxis = list(title=""), margin = list(b=160))
    })
  })
  
  
  observe({
    dataIn <- knockOutData()
    if(is.null(dataIn)){
      return(NULL)
    }
    dataIn <- remove.factors(dataIn)
    dataIn$Teamsort <- factor(dataIn$Team, levels = unique(dataIn$Team)[order(dataIn$Freq, decreasing = TRUE)])
    # dataIn <- dataIn[order(dataIn$Freq),]
    output$knockOutStats <- renderPlotly({
      p <- plot_ly(dataIn,x=~Teamsort, y=~Freq, type="bar")%>%layout(xaxis = list(title=""), margin = list(b=160))
    })
  })
  
  
}