header <- dashboardHeader(title = ' Euro 2020')



sidebar <- dashboardSidebar(
  width = 300,
  tags$img(src='https://www.initialvip.com/wp-content/uploads/2019/06/Euro-2020.png', height = 180, width = 180),
  sidebarMenu(
    # selectInput("competitor", "Select competitor", multiple = TRUE, choices = NULL, selected = NULL),
    shinyWidgets::pickerInput(
      "competitor", 
      "Select competitor", 
      multiple = TRUE, 
      choices = NULL, 
      selected = NULL, 
      options = list(`actions-box` = TRUE)
    ),
    id = 'tabs',
    menuItem('Scoring summary', icon = icon("table"),
             menuSubItem('Leaderboard', tabName = 'leaderboard'),
             menuSubItem('Daily Scores', tabName = 'daily_scores')),
    menuItem('Group matches', icon = icon("table"),
             menuSubItem('Played Games', tabName = 'played_games'),
             menuSubItem('Unplayed Games', tabName = 'unplayed_games'),
             menuSubItem('Simulator', tabName = 'group_simulator')),
    menuItem('Knockout games', icon = icon("table"),
             menuSubItem('Eight finals', tabName = 'eight_final'),
             menuSubItem('Quarter finals', tabName = 'quarter_final'),
             menuSubItem('Semi finals', tabName = 'semi_final'),
             menuSubItem('Final', tabName = 'final'),
             menuSubItem('Winner', tabName = 'winner')),
    menuItem('Other', icon = icon("table"),
             menuSubItem('Top Scorers', tabName = 'top_scorers'),
             menuSubItem('Yellow Cards', tabName = 'yellow_cards'),
             menuSubItem('Average goals by competitor', tabName = 'average_goals_by_competitor'),
             menuSubItem('Number of correct teams per stage', tabName = 'number_of_correct_teams_per_stage'),
             menuSubItem('Knock out stats', tabName = 'knock_out_stats'))
  )
)

body <- dashboardBody(
  
  
  tags$head(includeCSS("./www/custom.css")),
  tabItems(
    tabItem(tabName = "leaderboard",
            widgetUserBox(
              title = 'Leaderboard',
              color = 'navy',
              type = 2,
              width = 3,
              collapsible = FALSE,
              boxToolSize = 'xs',
              dataTableOutput("leaderBoard")
            ),
            widgetUserBox(
              title = 'Leaderboard over time',
              color = 'navy',
              type = 2,
              width = 8,
              collapsible = FALSE,
              boxToolSize = 'xs',
              plotlyOutput("scoreGraph")
            )),
    tabItem(tabName = 'daily_scores',
            widgetUserBox(
              title = 'Daily scores',
              color = 'navy',
              type = 2,
              width = 4,
              collapsible = FALSE,
              boxToolSize = 'xs',
              dataTableOutput("allscores")
            ),
            widgetUserBox(
              title = 'Daily score',
              color = 'navy',
              type = 2,
              width = 8,
              collapsible = FALSE,
              boxToolSize = 'xs',
              plotlyOutput("daily_score_graph")
            )),
    tabItem(tabName = 'played_games',
            widgetUserBox(
              title = 'All scores from played games',
              color = 'navy',
              type = 2,
              width = 12,
              collapsible = FALSE,
              boxToolSize = 'xs',
              dataTableOutput("group_matches_played")
            )),
    tabItem(tabName = 'unplayed_games',
            widgetUserBox(
              title = 'All guessed scores from unplayed groum matches',
              color = 'navy',
              type = 2,
              width = 12,
              collapsible = FALSE,
              boxToolSize = 'xs',
              dataTableOutput("group_matches_unplayed")
            )),
    tabItem(tabName = 'group_simulator',
            widgetUserBox(
              title = 'Simulate group matches',
              color = 'navy',
              type = 2,
              width = 12,
              collapsible = FALSE,
              boxToolSize = 'xs',
              splitLayout(cellWidths = c("25%", "25%"),
                actionButton("simulate","Simulate"),
                numericInput("noOfSimDays","Simulate days forward", value = 1)
              ),
              rHandsontableOutput("simulator")
            )),
    tabItem(tabName = 'eight_final',
            widgetUserBox(
              title = 'Guess',
              color = 'navy',
              type = 2,
              width = 7,
              collapsible = FALSE,
              boxToolSize = 'xs',
              dataTableOutput("final8")
            ),
            widgetUserBox(
              title = 'Actual teams',
              color = 'navy',
              type = 2,
              width = 4,
              collapsible = FALSE,
              boxToolSize = 'xs',
              dataTableOutput("final8Actual")
            )),
    tabItem(tabName = 'quarter_final',
            widgetUserBox(
              title = 'Guess',
              color = 'navy',
              type = 2,
              width = 7,
              collapsible = FALSE,
              boxToolSize = 'xs',
              dataTableOutput("final4")
            ),
            widgetUserBox(
              title = 'Actual teams',
              color = 'navy',
              type = 2,
              width = 4,
              collapsible = FALSE,
              boxToolSize = 'xs',
              dataTableOutput("final4Actual")
            )),
    tabItem(tabName = 'semi_final',
            widgetUserBox(
              title = 'Guess',
              color = 'navy',
              type = 2,
              width = 7,
              collapsible = FALSE,
              boxToolSize = 'xs',
              dataTableOutput("final2")
            ),
            widgetUserBox(
              title = 'Actual teams',
              color = 'navy',
              type = 2,
              width = 4,
              collapsible = FALSE,
              boxToolSize = 'xs',
              dataTableOutput("final2Actual")
            )),
    tabItem(tabName = 'final',
            widgetUserBox(
              title = 'Guess',
              color = 'navy',
              type = 2,
              width = 7,
              collapsible = FALSE,
              boxToolSize = 'xs',
              dataTableOutput("final1")
            ),
            widgetUserBox(
              title = 'Actual teams',
              color = 'navy',
              type = 2,
              width = 4,
              collapsible = FALSE,
              boxToolSize = 'xs',
              dataTableOutput("final1Actual")
            )),
    tabItem(tabName = 'winner',
            widgetUserBox(
              title = 'Guessed winners',
              color = 'navy',
              type = 2,
              width = 7,
              collapsible = FALSE,
              boxToolSize = 'xs',
              dataTableOutput("winner")
            ),
            widgetUserBox(
              title = 'Actual winner',
              color = 'navy',
              type = 2,
              width = 4,
              collapsible = FALSE,
              boxToolSize = 'xs',
              dataTableOutput("winnerActual")
            )),
    tabItem(tabName = 'top_scorers',
            widgetUserBox(
              title = 'Guessed top scorers',
              color = 'navy',
              type = 2,
              width = 7,
              collapsible = FALSE,
              boxToolSize = 'xs',
              dataTableOutput("topscorers")
            ),
            widgetUserBox(
              title = 'Current standing',
              color = 'navy',
              type = 2,
              width = 4,
              collapsible = FALSE,
              boxToolSize = 'xs',
              dataTableOutput("topScorerActual")
            )),
    tabItem(tabName = 'yellow_cards',
            widgetUserBox(
              title = 'Guessed number of yellow cards',
              color = 'navy',
              type = 2,
              width = 7,
              collapsible = FALSE,
              boxToolSize = 'xs',
              dataTableOutput("yellowcards")
            ),
            widgetUserBox(
              title = 'Yellow cards until now',
              color = 'navy',
              type = 2,
              width = 4,
              collapsible = FALSE,
              boxToolSize = 'xs',
              dataTableOutput("yellowCardsActual")
            )),
    tabItem(tabName = 'average_goals_by_competitor',
            widgetUserBox(
              title = 'Guessed average number of goals',
              color = 'navy',
              type = 2,
              width = 12,
              collapsible = FALSE,
              boxToolSize = 'xs',
              plotlyOutput("aveGoals")
            )),
    tabItem(tabName = 'number_of_correct_teams_per_stage',
            widgetUserBox(
              title = 'Number of correct teams',
              color = 'navy',
              type = 2,
              width = 12,
              collapsible = FALSE,
              boxToolSize = 'xs',
              selectInput("knockOutStageTeamsLeft", "Select Stage", choices = 1, multiple = FALSE, selected = 1),
              plotlyOutput("teamsPerStage",width = 1500, height=800)
            )),
    tabItem(tabName = 'knock_out_stats',
            widgetUserBox(
              title = 'Stats for knockout phase',
              color = 'navy',
              type = 2,
              width = 12,
              collapsible = FALSE,
              boxToolSize = 'xs',
              selectInput("knockOutStage", "Select Stage", choices = NULL, multiple = FALSE, selected = NULL),
              plotlyOutput("knockOutStats", width = 1500, height = 800)
            ))
    
    
    
    

    
    )
)