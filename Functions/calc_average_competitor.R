today <- "2021-06-01 23:00:00"
dataIn <- readResults(today)
all_games = as.data.table(dataIn$games)
all_group_games = all_games[Group %in% c('A', 'B', 'C', 'D', 'E', 'F')]
average_player = all_group_games[, .(Home = round(median(Home)), Away = round(median(Away))),
    by = c('Game_No', 'Round_Number', 'Date', 'Location', 'Home_Team', 'Away_Team', 'Group')]

all_knockout_games = all_games[!(Group %in% c('A', 'B', 'C', 'D', 'E', 'F'))]

#######################################################################
## Round 4
###########################################################################
round4_teams = data.table(Team = all_knockout_games[Round_Number == 4]$Home_Team)
round4_teams = rbind(round4_teams, 
    data.table(Team = all_knockout_games[Round_Number == 4]$Away_Team))
round4_teams = round4_teams[, .(.N), by = Team][order(-N)]
teams_to_keep = round4_teams[1:16]$Team
round4_games = all_knockout_games[Round_Number == 4 & competitor == 'Arin']
round4_games$Home_Team = teams_to_keep[1:8]
round4_games$Away_Team = teams_to_keep[9:16]
round4_games[, competitor := NULL]
round4_games[, Home := 0]
round4_games[, Away := 0]


#######################################################################
## Round 5
###########################################################################
round5_teams = data.table(Team = all_knockout_games[Round_Number == 5]$Home_Team)
round5_teams = rbind(round5_teams, 
    data.table(Team = all_knockout_games[Round_Number == 5]$Away_Team))
round5_teams = round5_teams[, .(.N), by = Team][order(-N)]
teams_to_keep = round5_teams[1:8]$Team
round5_games = all_knockout_games[Round_Number == 5 & competitor == 'Arin']
round5_games$Home_Team = teams_to_keep[1:4]
round5_games$Away_Team = teams_to_keep[5:8]
round5_games[, competitor := NULL]
round5_games[, Home := 0]
round5_games[, Away := 0]

#######################################################################
## Round 6
###########################################################################
round6_teams = data.table(Team = all_knockout_games[Round_Number == 6]$Home_Team)
round6_teams = rbind(round6_teams, 
    data.table(Team = all_knockout_games[Round_Number == 6]$Away_Team))
round6_teams = round6_teams[, .(.N), by = Team][order(-N)]
teams_to_keep = round6_teams[1:4]$Team
round6_games = all_knockout_games[Round_Number == 6 & competitor == 'Arin']
round6_games$Home_Team = teams_to_keep[1:2]
round6_games$Away_Team = teams_to_keep[3:4]
round6_games[, competitor := NULL]
round6_games[, Home := 0]
round6_games[, Away := 0]

#######################################################################
## Final
###########################################################################
round7_teams = data.table(Team = all_knockout_games[Round_Number == 'Final match']$Home_Team)
round7_teams = rbind(round7_teams, 
    data.table(Team = all_knockout_games[Round_Number == 'Final match']$Away_Team))
round7_teams = round7_teams[, .(.N), by = Team][order(-N)]
teams_to_keep = round7_teams[1:2]$Team
round7_games = all_knockout_games[Round_Number == 'Final match' & competitor == 'Arin']
round7_games$Home_Team = teams_to_keep[1]
round7_games$Away_Team = teams_to_keep[2]
round7_games[, competitor := NULL]
round7_games[, Home := 0]
round7_games[, Away := 0]

#####################################################
## Yellow cards and topscorer
########################################################
yellowCards = as.data.table(dataIn$yellowCards)
average_number_of_yellow_cards = round(mean(yellowCards[competitor != 'results']$yellowCards))
topScorer = as.data.table(dataIn$topScorer)
average_topScorer = topScorer[competitor != 'results', .(.N), by = 'topScorer'][order(-N)]$topScorer[1]

nonmatch_dt = data.table(
    Game_No = c('','', '',''),
    Round_Number = c('','', '',''),
    Date = c(Sys.time(),Sys.time(),Sys.time(), Sys.time()),
    Location = c('TopScorer', 'Yellow Cards', 'Competitor', 'Email'),
    Home_Team = c(average_topScorer, average_number_of_yellow_cards, 'Average Joe', ''),
    Away_Team = c('','', '',''),
    Group = c('','', '',''),
    Home = c('','', '',''),
    Away = c('','', '','')
)

average_competitor = rbind(
    average_player, 
    round4_games, 
    round5_games, 
    round6_games, 
    round7_games,
    nonmatch_dt)

getwd()
average_competitor[, Date := as.character(Date)]
filename = gsub(':','_',paste0('./Data/Average_Joe_', Sys.time(), '.csv'))
fwrite(average_competitor, filename, row.names = FALSE)
