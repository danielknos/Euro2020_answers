calcCurrentScore <- function(games, results, scoring, yellowCards, topScorer, today){
  # 
  # today <- Sys.time()
  # today <- format(today, format="%Y-%m-%d %H:%M:%S")
  # today <- "2019-06-30 23:59:00"
  
  games$Date <- as.POSIXct(games$Date,tz="",format = "%d-%m-%Y %H:%M:%S")
  results$Date <- as.POSIXct(results$Date,tz="",format = "%d-%m-%Y %H:%M:%S")
  
  yellowCards <- remove.factors(yellowCards)
  
  # Reads the scoring from the scoring file
  indCorrectSign <- which(scoring$Stage =="Group" & scoring$category == "correct_sign")
  indCorrectResult <- which(scoring$Stage =="Group" & scoring$category == "correct_score")
  indCorrectDiff <- which(scoring$Stage =="Group" & scoring$category == "correct_goal_diff")
  indCorrectTopScorer<- which(scoring$Stage =="Top_scorer")
  indCorrectYellowCards<- which(scoring$Stage =="Yellow_cards")
  final8scoreInd <- which(scoring$Stage =="8th_final" & scoring$category == "per_correct_team")
  final4scoreInd <- which(scoring$Stage =="Quarter_final" & scoring$category == "per_correct_team")
  final2scoreInd <- which(scoring$Stage =="Semi_final" & scoring$category == "per_correct_team")
  final1scoreInd <- which(scoring$Stage =="Final" & scoring$category == "per_correct_team")
  winnerInd <- which(scoring$Stage =="Winner" & scoring$category == "correct")
  
  correctDiffScore <- scoring$points[indCorrectDiff]
  correctSignScore <- scoring$points[indCorrectSign]
  correctResultScore <- scoring$points[indCorrectResult]
  correctTopScorerScore <- scoring$points[indCorrectTopScorer]
  correctYellowCardsScore <- scoring$points[indCorrectYellowCards]
  final8Score <- scoring$points[final8scoreInd]
  final4Score <- scoring$points[final4scoreInd]
  final2Score <- scoring$points[final2scoreInd]
  final1Score <- scoring$points[final1scoreInd]
  winnerScore <- scoring$points[winnerInd]
  
  
  # Combining the group matches with the guessed numbers
  competitorMerge <- merge(games, results, by.x=c("Game_No", "Home_Team", "Away_Team", "Group", "Date", "Round_Number"), by.y = c("Game_No", "Home_Team", "Away_Team", "Group", "Date", "Round_Number"),all.x=TRUE)
  competitorMerge$competitor.y <- NULL
  colnames(competitorMerge)[which(colnames(competitorMerge)=="competitor.x")] <- "competitor"
  groupMatches <- competitorMerge[which(competitorMerge$Round_Number <=3),]
  groupMatches$score <- 0
  groupMatches$finishDate <- NULL
  
  # Scoring the group matches with the correct sign
  indCorrectSign1 <- which(groupMatches$Home>groupMatches$Away & groupMatches$Home_actual>groupMatches$Away_actual & groupMatches$Round_Number <=3)
  indCorrectSignX <- which(groupMatches$Home==groupMatches$Away & groupMatches$Home_actual==groupMatches$Away_actual & groupMatches$Round_Number <=3)
  indCorrectSign2 <- which(groupMatches$Home<groupMatches$Away & groupMatches$Home_actual<groupMatches$Away_actual & groupMatches$Round_Number <=3)
  indCorrectSign <- cbind(t(indCorrectSign1), t(indCorrectSignX), t(indCorrectSign2))
  
  groupMatches$score[indCorrectSign] <- correctSignScore
  
  # Scoring the group matches with correct diff
  indCorrectDiff <- which((groupMatches$Home-groupMatches$Away)==(groupMatches$Home_actual - groupMatches$Away_actual) & groupMatches$Round_Number <=3)
  groupMatches$score[indCorrectDiff] <- correctDiffScore
  
  #correcting for the matches with X should not be rewarded for correct diff
  groupMatches$score[indCorrectSignX] <- correctSignScore
  
  # Scoring the matches with correct result
  indCorrectResult <- which(groupMatches$Home == groupMatches$Home_actual & groupMatches$Away == groupMatches$Away_actual & groupMatches$Round_Number <=3)
  groupMatches$score[indCorrectResult] <- correctResultScore
  groupMatches <- groupMatches[order(groupMatches$Date, groupMatches$competitor),]
  
  # For all unplayed matches the score is set back to 0
  indUnplayedGames <- which(groupMatches$Date>today)
  groupMatches$score[indUnplayedGames] <- 0
  
  
  #Calculating stats for the group matches
  tmp <- groupMatches[,c("Home","Away","competitor")]
  tmp$totGoals <- tmp$Home + tmp$Away
  tmp$Home <- NULL
  tmp$Away <- NULL
  aveGoals <- aggregate(.~competitor, data=tmp, mean)
  aveGoals <- aveGoals[order(aveGoals$totGoals),]
  colnames(aveGoals)[2] <- "Mean_Goals"

  # Selecting all the knock out matches and merges with the correct ones
  
  knockOutGames <- games[which(games$Round_Number %in% c("4","5","6","Final match")),]
  knockOutGamesMerged <- merge(knockOutGames, results, by.x=c("Game_No", "Round_Number", "Group"), by.y = c("Game_No", "Round_Number","Group"),all.x=TRUE)
  knockOutGamesMerged$Location.x <- NULL
  knockOutGamesMerged$Location <- knockOutGamesMerged$Location.y
  knockOutGamesMerged$Location.y <- NULL
  
  
  knockOutGamesMerged$competitor <- knockOutGamesMerged$competitor.x
  knockOutGamesMerged$competitor.x <- NULL
  knockOutGamesMerged$competitor.y <- NULL
  
  knockOutGamesMerged$Home_Team <- knockOutGamesMerged$Home_Team.x
  knockOutGamesMerged$Home_Team.x <- NULL
  knockOutGamesMerged$Home_Team.y <- NULL
  
  knockOutGamesMerged$Away_Team <- knockOutGamesMerged$Away_Team.x
  knockOutGamesMerged$Away_Team.x <- NULL
  knockOutGamesMerged$Away_Team.y <- NULL
  
  
  # Calculating scores for 8th final
  final8Teams <- knockOutGamesMerged[which(knockOutGamesMerged$Round_Number ==4),c("competitor","Home_Team","Away_Team","finishDate")]
  final8Teams <- melt(final8Teams, id=c("competitor","finishDate"))
  final8Teams$Stage <- "8th final"
  final8Teams$variable <- NULL
  final8TeamsActual <- results[which(results$Round_Number ==4),c("competitor","Home_Team","Away_Team")]
  final8TeamsActual <- melt(final8TeamsActual, id=c("competitor"))
  final8TeamsActual$Stage <- "8th final"
  final8TeamsActual$variable <- NULL
  correctFinal8Ind <- which(final8Teams$value %in% final8TeamsActual$value )
  final8Teams$score <- 0
  final8Teams$score[correctFinal8Ind] <- final8Score
  
  #Calculating score for quarter final
  final4Teams <- knockOutGamesMerged[which(knockOutGamesMerged$Round_Number ==5),c("competitor","Home_Team","Away_Team","finishDate")]
  final4Teams <- melt(final4Teams, id=c("competitor","finishDate"))
  final4Teams$Stage <- "Quarter final"
  final4Teams$variable <- NULL
  final4TeamsActual <- results[which(results$Round_Number ==5),c("competitor","Home_Team","Away_Team")]
  final4TeamsActual <- melt(final4TeamsActual, id=c("competitor"))
  final4TeamsActual$Stage <- "Quarter final"
  final4TeamsActual$variable <- NULL
  correctFinal4Ind <- which(final4Teams$value %in% final4TeamsActual$value )
  final4Teams$score <- 0
  final4Teams$score[correctFinal4Ind] <- final4Score
  
  #Calculating score for semi final
  final2Teams <- knockOutGamesMerged[which(knockOutGamesMerged$Round_Number ==6),c("competitor","Home_Team","Away_Team","finishDate")]
  final2Teams <- melt(final2Teams, id=c("competitor","finishDate"))
  final2Teams$Stage <- "Semi final"
  final2Teams$variable <- NULL
  final2TeamsActual <- results[which(results$Round_Number ==6),c("competitor","Home_Team","Away_Team")]
  final2TeamsActual <- melt(final2TeamsActual, id=c("competitor"))
  final2TeamsActual$Stage <- "Semi final"
  final2TeamsActual$variable <- NULL
  correctFinal2Ind <- which(final2Teams$value %in% final2TeamsActual$value )
  final2Teams$score <- 0
  final2Teams$score[correctFinal2Ind] <- final2Score

  #Calculating score for  final

  final1Teams <- knockOutGamesMerged[which(knockOutGamesMerged$Group %in% c(51)),c("competitor","Home_Team","Away_Team","finishDate")]
  final1Teams <- melt(final1Teams, id=c("competitor","finishDate"))
  final1Teams$Stage <- "Final"
  final1Teams$variable <- NULL
  final1TeamsActual <- results[which(results$Group %in% c(51)),c("competitor","Home_Team","Away_Team")]
  final1TeamsActual <- melt(final1TeamsActual, id=c("competitor"))
  final1TeamsActual$Stage <- "Final"
  final1TeamsActual$variable <- NULL
  correctFinal1Ind <- which(final1Teams$value %in% final1TeamsActual$value )
  final1Teams$score <- 0
  final1Teams$score[correctFinal1Ind] <- final1Score
  
  
  
  
  # Combinng all the knockout teams to the same data frame, both for the guesses and fr the actual numbers
  allFinalMatches <- rbind(final8Teams, final4Teams, final2Teams, final1Teams)
  allFinalMatchesActual <- rbind(final8TeamsActual, final4TeamsActual, final2TeamsActual, final1TeamsActual)
  colnames(allFinalMatches)[which(colnames(allFinalMatches)=="value")] <- "Team"
  colnames(allFinalMatchesActual)[which(colnames(allFinalMatchesActual)=="value")] <- "Team"
  allFinalMatches <- allFinalMatches[order(allFinalMatches$Stage, allFinalMatches$competitor),]
  
  allFinalMatches <- remove.factors(allFinalMatches)
  
  #Calculating number of teams left in tournament
  
  teamsPerStage <- allFinalMatches[which(allFinalMatches$score>0),]
  if(nrow(teamsPerStage) >0){
    teamsPerStage <- aggregate(score ~competitor+Stage,data=teamsPerStage,FUN=length)
    colnames(teamsPerStage) <- c("competitor","Stage","noOfTeams")
    teamsPerStage <- teamsPerStage[order(teamsPerStage$Stage, -teamsPerStage$noOfTeams),]  
  } else {
    teamsPerStage <- NULL
  }
  
  
  # Selecting all the winners
  allFinals <- competitorMerge[which(competitorMerge$Round_Number =="Final match"),c("competitor","Home_Team","Away_Team","Home","Away")]
  indHomeWinner <- which(allFinals$Home >= allFinals$Away)
  indAwayWinner <- which(allFinals$Away > allFinals$Home)
  allFinals$Winner <- ""
  allFinals$Winner[indHomeWinner] <- allFinals$Home_Team[indHomeWinner]
  allFinals$Winner[indAwayWinner] <- allFinals$Away_Team[indAwayWinner]
  
  
  allFinalMatchesFreq <- allFinalMatches[,c("Stage","Team")]
  allFinalMatchesFreq <- rbind(allFinalMatchesFreq, data.frame("Stage" = "Winner", "Team" = allFinals$Winner))
  allFinalMatchesFreq <- data.frame(table(allFinalMatchesFreq[,c("Stage","Team")],dnn="Stage"))
  allFinalMatchesFreq <- allFinalMatchesFreq[which(allFinalMatchesFreq$Freq >0),]
  allFinalMatchesFreq <- allFinalMatchesFreq[order(allFinalMatchesFreq$Stage, -allFinalMatchesFreq$Freq),]
  
  
  allFinalsActual <- results[which(results$Round_Number =="Final match"),c("Home_Team","Away_Team","Home_actual","Away_actual")]

  if(allFinalsActual$Home_actual >= allFinalsActual$Away_actual){
    allFinalsActual$Winner <- allFinalsActual$Home_Team
  }else{
    allFinalsActual$Winner <- allFinalsActual$Away_Team
  }
  winnerActual <- data.frame("competitor" = "result","winner"="unknown")
  winnerActual <- data.frame("competitor" = "result","winner"=allFinalsActual$Winner)
  
  indCorrectWinner <- which(allFinals$Winner == allFinalsActual$Winner)
  allFinals$score <- 0
  allFinals$score[indCorrectWinner] <- winnerScore
  allFinals <- allFinals[,c("competitor","Winner", "score")]
  allFinals <- allFinals[order(allFinals$competitor),]

  # Selecting top scorers
  correctTopScorerInd <- which(topScorer$competitor == "results")
  correctTopScorer <- topScorer$topScorer[correctTopScorerInd]
  competitorTopScorerInd <- which(topScorer$competitor != "results")
  topScorer <- topScorer[competitorTopScorerInd,]
  correctTopScorerScoreInd <- which(topScorer$topScorer == correctTopScorer)
  topScorer$score <- 0
  topScorer$score[correctTopScorerScoreInd] <- correctTopScorerScore
  
  # Selecting yellow cards
  correctYellowCardsInd <- which(yellowCards$competitor == "results")
  correctYellowCards <- yellowCards$yellowCards[correctYellowCardsInd]
  competitorYellowCardsInd <- which(yellowCards$competitor != "results")
  yellowCards <- yellowCards[competitorYellowCardsInd,]
  closetYellowCardsInd <- which.min(abs(yellowCards$yellowCards - correctYellowCards))
  yellowCards$score <- abs(yellowCards$yellowCards - correctYellowCards)
  yellowCardsActual <- data.frame("competitor" = "result", score = correctYellowCards)
  
 # Saves all calculated scores to allScores data
  allScores <- NULL
  allScores$groupMatches <- groupMatches[,c("competitor","Date","Group","Home_Team","Away_Team", "Home", "Away", "Home_actual", "Away_actual","score")]
  colnames(allScores$groupMatches) <- c("competitor","Date","Group","Home_Team","Away_Team", "Home_guess", "Away_guess", "Home_actual", "Away_actual","score")
  allScores$yellowCards <- yellowCards
  allScores$topScorer <- topScorer
  allScores$allFinalMatches <- allFinalMatches[,c("competitor", "Stage", "Team", "score")]
  allScores$allFinalMatchesFreq <- allFinalMatchesFreq
  allScores$allFinalMatchesActual <- allFinalMatchesActual
  allScores$allWinners <- allFinals
  allScores$winnerActual <- winnerActual
  allScores$teamsPerStage <- teamsPerStage

  groupSummary <- allScores$groupMatches[,c("Date","competitor","score")]
  groupSummary$Stage <- "Group stage"
  knockOutSummary <- allFinalMatches[,c("competitor","Stage","score","finishDate")]
  knockOutSummary$Date <- knockOutSummary$finishDate
  knockOutSummary$finishDate <- NULL
  
  allFinals$Date <- as.POSIXct("2021-07-11 20:00:00")
  allFinals$Stage <- "Winner"
  allFinals$Winner <- NULL
 
  scoringTable <- rbind(groupSummary, knockOutSummary, allFinals)
  scoringTable$Date <- as.Date(scoringTable$Date)
  scoringTableAgg <- aggregate(score~competitor+Date,data=scoringTable, sum)
  scoringTableAgg <- mutate(group_by(scoringTableAgg,competitor),totScore=cumsum(score))
  allScores$scoringTable <- scoringTableAgg
  
  leaderBoard <- aggregate(score~competitor,data=scoringTable, sum)
  leaderBoard <- leaderBoard[order(-leaderBoard$score),]
  leaderBoard$rank <- 1:nrow(leaderBoard)
  leaderBoard <- leaderBoard[,c("rank","competitor","score")]
  
  allScores$leaderBoard <- leaderBoard
  allScores$yellowCardsActual <-  yellowCardsActual
  allScores$aveGoals <- aveGoals
  
  return(allScores)
}


