# Trade Analyzer

library(shiny)
library(DT)
library(qpcR)
library(shinyBS)



ssdata <- read.csv("startsitdata.csv")
totalssdata <- read.csv("totalssdata.csv")
players <- read.csv("players.csv")
schedule <- read.csv("schedule.csv")
colnames(schedule) <- c("Team","Bye","1","2","3","4","5","6","7","8","9","10","11","12",
                        "13","14","15","16","17","1","2","3","4","5","6","7","8",
                        "9","10","11","12","13","14","15","16","17")
schedule_bye <- schedule[,1:2]
schedule_team <- schedule[,3:19]
schedule_home <- schedule[,20:36]
rownames(schedule_team) <- schedule$Team
rownames(schedule_home) <- schedule$Team
rownames(schedule_bye) <- schedule$Team


ui <- fluidPage(tags$head(tags$style(HTML('

                                          .modal-lg {
                                          width: 1200px;
                                          
                                          }
                                          '))),
   
  titlePanel(title = NULL),
  h3("Redraft Trade Analyzer"),
  br(),
  
  sidebarLayout(
    sidebarPanel(width = 3, fluid = TRUE,
                 strong("Welcome"),
                 br(),
                 ("Welcome to the FFStatistics Redraft Trade Analyzer! Use this tool to help you
                  with your redraft league trades! It is super easy to use and quickly analyze any 
                  trade you offer or have been offered!"),
                 br(),
                 br(),
                 strong("Directions:"),
                 br(),
                 ("The tool is broken up into two sections. First, is the overall trade itself.
                  In both columns, input the player(s) received and player(s) traded, simple as that.
                  The next section is very unique. How your team and starting lineup is affected by 
                  a trade is very important to your overall team success. In both columns, you can enter
                  your starting lineup pre-trade and then post-trade to get an all-encompassing analysis 
                  of how each trade affects your team. When you are done, simply hit the 'ANALYZE TRADE'
                  button to see a complete analysis of your trade, including rest of season projections
                  and starting lineup strength pre and post-trade."),
                 br(),
                 br(),
                 strong("NOTE:"),
                 (" The tool may take a few seconds to completely update when quickly changing settings and
                  recomparing players. Do not be alarmed if the results do not immediately change.")
                 ),
    
    mainPanel(
      fluidRow(
        h3("Input League Settings"),
        column(3, numericInput("tc_numberA", "PP Pass Attempt:",
                               value = 0, min = 0, max = 1, step = 0.01)),
        column(3, numericInput("tc_numberB", "PP Pass Compl:",
                               value = 0, min = 0, max = 1, step = 0.01)),
        column(3, numericInput("tc_numberC", "PP Pass Incompl:",
                               value = 0, min = -2, max = 0, step = 0.01)),
        column(3, numericInput("tc_numberD", "PP Pass Yard:",
                               value = 0.04, min = 0, max = 1, step = 0.01)),
        column(3, numericInput("tc_numberE", "PP Pass TD:",
                               value = 4, min = 0, max = 10, step = 1)),
        column(3, numericInput("tc_numberF", "PP INT:",
                               value = -2, min = -5, max = 5, step = 1)),
        column(3, numericInput("tc_numberG", "PP Carry:",
                               value = 0, min = 0, max = 1, step = 0.01)),
        column(3, numericInput("tc_numberH", "PP Reception:",
                               value = 1, min = 0, max = 5, step = 0.1)),
        column(3, numericInput("tc_numberI", "TE PP Reception:",
                               value = 1, min = 0, max = 5, step = 0.1)),
        column(3, numericInput("tc_numberJ", "PP Rush/Rec Yard:",
                               value = 0.1, min = 0, max = 5, step = 0.1)),
        column(3, numericInput("tc_numberK", "PP Rush/Rec TD:",
                               value = 6, min = 0, max = 10, step = 1)),
        column(3, numericInput("ssnumber", "Select Points Needed:", min = 0, max = 50,
                               step = 0.1, value = 20))
      ),
      
      br(),
      br(),
      
      h3("Input Trade"),
      fluidRow(
        column(5, selectInput("tradeA1", "Select Player(s) Received:",
                              choices = c("None",unique(as.character(players$Player))),
                              selected = "None")),
        column(5, selectInput("tradeB1", "Select Player(s) Traded:",
                              choices = c("None",unique(as.character(players$Player))),
                              selected = "None"))
      ),
      fluidRow(
        column(5, selectInput("tradeA2", NULL,
                              choices = c("None",unique(as.character(players$Player))),
                              selected = "None")),
        column(5, selectInput("tradeB2", NULL,
                              choices = c("None",unique(as.character(players$Player))),
                              selected = "None"))
      ),
      fluidRow(
        column(5, selectInput("tradeA3", NULL,
                              choices = c("None",unique(as.character(players$Player))),
                              selected = "None")),
        column(5, selectInput("tradeB3", NULL,
                              choices = c("None",unique(as.character(players$Player))),
                              selected = "None"))
      ),
      fluidRow(
        column(5, selectInput("tradeA4", NULL,
                              choices = c("None",unique(as.character(players$Player))),
                              selected = "None")),
        column(5, selectInput("tradeB4", NULL,
                              choices = c("None",unique(as.character(players$Player))),
                              selected = "None"))
      ),
      fluidRow(
        column(5, selectInput("tradeA5", NULL,
                              choices = c("None",unique(as.character(players$Player))),
                              selected = "None")),
        column(5, selectInput("tradeB5", NULL,
                              choices = c("None",unique(as.character(players$Player))),
                              selected = "None"))
      ),
      
      br(),
      br(),
      br(),
      
      h3("Input Starting Lineup"),
      fluidRow(
        column(5, h4("Lineup Pre-Trade")),
        column(5, h4("Lineup Post-Trade")),
        column(5, selectInput("qbA", "Select Starting QB:",
                              choices = c("None",unique(as.character(subset(players$Player, players$Position == "QB")))),
                              selected = "None")),
        column(5, selectInput("qbB", "Select Starting QB:",
                              choices = c("None",unique(as.character(subset(players$Player, players$Position == "QB")))),
                              selected = "None")),
        column(5, selectInput("rbA", "Select Starting RBs:",
                              choices = c("None",unique(as.character(subset(players$Player, players$Position == "RB")))),
                              selected = "None")),
        column(5, selectInput("rbB", "Select Starting RBs:",
                              choices = c("None",unique(as.character(subset(players$Player, players$Position == "RB")))),
                              selected = "None")),
        column(5, selectInput("rbC", NULL,
                              choices = c("None",unique(as.character(subset(players$Player, players$Position == "RB")))),
                              selected = "None")),
        column(5, selectInput("rbD", NULL,
                              choices = c("None",unique(as.character(subset(players$Player, players$Position == "RB")))),
                              selected = "None")),
        column(5, selectInput("wrA", "Select Starting WRs:",
                              choices = c("None",unique(as.character(subset(players$Player, players$Position == "WR")))),
                              selected = "None")),
        column(5, selectInput("wrB", "Select Starting WRs:",
                              choices = c("None",unique(as.character(subset(players$Player, players$Position == "WR")))),
                              selected = "None")),
        column(5, selectInput("wrC", NULL,
                              choices = c("None",unique(as.character(subset(players$Player, players$Position == "WR")))),
                              selected = "None")),
        column(5, selectInput("wrD", NULL,
                              choices = c("None",unique(as.character(subset(players$Player, players$Position == "WR")))),
                              selected = "None")),
        column(5, selectInput("teA", "Select Starting TE:",
                              choices = c("None",unique(as.character(subset(players$Player, players$Position == "TE")))),
                              selected = "None")),
        column(5, selectInput("teB", "Select Starting TE:",
                              choices = c("None",unique(as.character(subset(players$Player, players$Position == "TE")))),
                              selected = "None")),
        column(5, selectInput("flexA", "Select Starting Flex(s):",
                              choices = c("None",unique(as.character(subset(players$Player, players$Position == c("RB","WR","TE"))))),
                              selected = "None")),
        column(5, selectInput("flexB", "Select Starting Flex(s):",
                              choices = c("None",unique(as.character(subset(players$Player, players$Position == c("RB","WR","TE"))))),
                              selected = "None")),
        column(5, selectInput("flexC", NULL,
                              choices = c("None",unique(as.character(subset(players$Player, players$Position == c("RB","WR","TE"))))),
                              selected = "None")),
        column(5, selectInput("flexD", NULL,
                              choices = c("None",unique(as.character(subset(players$Player, players$Position == c("RB","WR","TE"))))),
                              selected = "None")),
        column(5, selectInput("flexE", NULL,
                              choices = c("None",unique(as.character(subset(players$Player, players$Position == c("RB","WR","TE"))))),
                              selected = "None")),
        column(5, selectInput("flexF", NULL,
                              choices = c("None",unique(as.character(subset(players$Player, players$Position == c("RB","WR","TE"))))),
                              selected = "None"))
      ),
      
      
      
      fluidRow(
        column(4),
        column(2, actionButton("tradego", "ANALYZE TRADE!"))
      ),
      
      br(),
      br(),
      br(),
      br(),
      
      bsModal("trademodal", "Trade Analysis", "tradego", size = "large",
              
              tags$head(tags$style("#traderecall{font-size:14px; font-weight: bold}")),
              tags$head(tags$style("#tradedecision{font-size:14px; font-weight: bold}")),
              
              strong("Trade:"),
              fluidRow(column(12, verbatimTextOutput("traderecall"))),
              br(),
              br(),
              strong("Trade Decision:"),
              fluidRow(column(12, verbatimTextOutput("tradedecision"))),
              br(),
              br(),
              strong("Player Projection ROS:"),
              fluidRow(column(12,dataTableOutput("tradetable")))
              
              
              
      )
    )
  )
  
)


server <- function(input, output) {
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

