readResults <- function(today){
  
  folder <- "./Data/"
  allFiles <- list.files(folder, pattern = ".csv")
  noOfResults <- length(allFiles)
  games <- NULL
  topScorer <- NULL
  yellowCards <- NULL
  retData <- NULL
  
  
  for(i in 1:noOfResults){
    fileName <- paste(folder,allFiles[i],sep="")
    if(fileName != "./Data/scoring.csv" & fileName != "./Data/topscorers.csv"){
      
      if(fileName == "./Data/results.csv"){
        
        thisResult <- read.csv(fileName, stringsAsFactors = FALSE,encoding="latin", sep=",")
      }else{
        thisResult <- read.csv(fileName, stringsAsFactors = FALSE,encoding="latin")
      }
      competitorInd <- which(thisResult$Location == "Competitor")
      competitor <- thisResult[competitorInd,"Home_Team"]
      
      colnames(thisResult)[1] <- "X"
      thisResult$X <- as.integer(thisResult$X)
      gameInd <- which(thisResult$X<=64)
      gamesTmp <- thisResult[gameInd,]
      gamesTmp$competitor <- competitor
      
      topScorerInd <- which(thisResult$Location == "TopScorer")
      topScorerTmp <- thisResult[topScorerInd,"Home_Team"]
      topScorerTmp <- data.frame(competitor, "topScorer" = topScorerTmp)
      
      yellowCardInd <- which(thisResult$Location == "Yellow Cards")
      yellowCardsTmp <- thisResult[yellowCardInd,"Home_Team"]
      yellowCardsTmp <- data.frame(competitor, "yellowCards" = yellowCardsTmp)
      
      games <- rbind(games, gamesTmp)
      topScorer <- rbind(topScorer, topScorerTmp)
      yellowCards <- rbind(yellowCards, yellowCardsTmp)
    }
  }
  
  games$Date <- as.POSIXct(games$Date,tz="",format = "%Y-%m-%d %H:%M")
   
  games$Home <- as.numeric(games$Home)
  games$Away <- as.numeric(games$Away)

  scoring <- read.csv("./Data/scoring.csv", sep=";")
  topScorerActual <- read.csv("./Data/topscorers.csv", sep=",")
  topScorerActual <- topScorerActual[order(-topScorerActual$Goals),]
  
  
  correctGamesInd <- which(games$competitor == "results")
  correctGames <- games[correctGamesInd,]
  correctGames$Date <- as.POSIXct(correctGames$Date,tz="",format = "%d-%m-%Y %H:%M:%S")
  
  
  ind <- which(correctGames$Date>today & correctGames$Round.Number %in% c("4","5","6","Final match"))
  Game_No <- c(37:51)
  finishDate <- as.POSIXct(c("2020-06-27 21:00:00","2020-06-27 23:00:00",
                  "2020-06-28 23:00:00","2020-06-28 21:00:00",
                  "2020-06-29 21:00:00","2020-06-29 23:00:00",
                  "2020-06-30 23:00:00","2020-06-30 21:00:00",
                  "2020-07-03 21:00:00","2020-07-03 23:00:00",
                  "2020-07-04 23:00:00","2020-07-04 21:00:00",
                  "2020-07-07 23:00:00","2020-07-08 23:00:00",
                  "2020-07-12 23:00:00"))
  group_finishDate <- data.frame(Game_No, finishDate, stringsAsFactors = FALSE)
  
  correctGames <- merge(correctGames, group_finishDate, all.x = TRUE)
  ind <- which(correctGames$finishDate>today)
  correctGames$Home_Team[ind] <- ""
  correctGames$Away_Team[ind] <- ""
  correctGames <- correctGames[order(correctGames$Date),]
  
  # correctGames$finishDate <- NULL
  
  competitorsInd <- which(games$competitor != "results")
  competitorGames <- games[competitorsInd,]
  
  
  colnames(correctGames)[which(colnames(correctGames)=="Home")] <- "Home_actual"
  colnames(correctGames)[which(colnames(correctGames)=="Away")] <- "Away_actual"
  correctGames$Home_actual <- as.integer(correctGames$Home_actual)
  correctGames$Away_actual <- as.integer(correctGames$Away_actual)
  
  yellowCards <- remove.factors(yellowCards)
  yellowCards$yellowCards <- as.numeric(yellowCards$yellowCards)
  
  retData$scoring <- scoring
  retData$games <- competitorGames
  retData$results <- correctGames
  retData$yellowCards <- yellowCards
  retData$topScorer <- topScorer
  retData$topScorerActual <- topScorerActual
  
  return(retData)
  
}