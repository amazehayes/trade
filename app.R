# Trade Analyzer

library(shiny)
library(DT)
library(qpcR)
library(rapport)
library(shinyBS)
library(Matrix)


ssdata <- read.csv("startsitdata.csv")
totalssdata <- read.csv("totalssdata.csv")
players <- read.csv("players.csv")
schedule <- read.csv("schedule.csv")
colnames(schedule) <- c("Team","Bye","1","2","3","4","5","6","7","8","9","10","11","12",
                        "13","14","15","16","17","1","2","3","4","5","6","7","8",
                        "9","10","11","12","13","14","15","16","17")
schedule_bye <- schedule[,1:2]
schedule_team <- schedule[,3:19]
schedule_home <- schedule[,c(20:36)]
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

  ### Trade Analyzer
  output$traderecall <- renderText({
    
    a <- paste("",
               if(input$tradeB1 != "None") {input$tradeB1},
               if(input$tradeB2 != "None") {paste("&",input$tradeB2)},
               if(input$tradeB3 != "None") {paste("&",input$tradeB3)},
               if(input$tradeB4 != "None") {paste("&",input$tradeB4)},
               if(input$tradeB5 != "None") {paste("&",input$tradeB5)}, "\n","for","\n",
               if(input$tradeA1 != "None") {input$tradeA1},
               if(input$tradeA2 != "None") {paste("&",input$tradeA2)},
               if(input$tradeA3 != "None") {paste("&",input$tradeA3)},
               if(input$tradeA4 != "None") {paste("&",input$tradeA4)},
               if(input$tradeA5 != "None") {paste("&",input$tradeA5)}
    )
    
  })
  
  output$tradedecision <- renderText({
    
    pretrade <- c(input$qbA, input$rbA, input$rbC, input$wrA, input$wrC, input$teA,
                  input$flexA, input$flexC, input$flexE)
    posttrade <- c(input$qbB, input$rbB, input$rbD, input$wrB, input$wrD, input$teB,
                   input$flexB, input$flexD, input$flexF)
    
    avgs1 = NULL
    for(i in 1:length(pretrade)) {
      p <- na.omit(head(ssdata[ssdata$Player == pretrade[i],],16))
      
      px <- as.vector(p$PPR4 + (p$PassAtt*input$tc_numberA) + (p$Comp*input$tc_numberB) + 
                        ((p$PassAtt-p$Comp)*input$tc_numberC) +
                        ((p$PassYards*input$tc_numberD)-(p$PassYards/25)) +
                        ((p$PassTDs*input$tc_numberE)-(p$PassTDs*4)) +
                        ((p$INT*input$tc_numberF)+(p$INT*2)) +
                        (p$RushAtt*input$tc_numberG) + ((p$RushYards*input$tc_numberJ)-(p$RushYards*0.1)) +
                        ((p$RushTDs*input$tc_numberK)-(p$RushTDs*6)) + 
                        ((p$Receptions*input$tc_numberH)-p$Receptions) +
                        ((p$RecYards*input$tc_numberJ)-(p$RecYards*0.1)) +
                        ((p$RecTDs*input$tc_numberK)-(p$RecTDs*6)))
      
      points <- round(mean(px),2)
      avgs1 <- rbind(avgs1, points)
    }
    avgs1
    rownames(avgs1) <- pretrade
    avgs1 <- replace(avgs1, avgs1=="NaN", NA)
    
    avgs2 = NULL
    for(i in 1:length(posttrade)) {
      p <- na.omit(head(ssdata[ssdata$Player == posttrade[i],],16))
      
      px <- as.vector(p$PPR4 + (p$PassAtt*input$tc_numberA) + (p$Comp*input$tc_numberB) + 
                        ((p$PassAtt-p$Comp)*input$tc_numberC) +
                        ((p$PassYards*input$tc_numberD)-(p$PassYards/25)) +
                        ((p$PassTDs*input$tc_numberE)-(p$PassTDs*4)) +
                        ((p$INT*input$tc_numberF)+(p$INT*2)) +
                        (p$RushAtt*input$tc_numberG) + ((p$RushYards*input$tc_numberJ)-(p$RushYards*0.1)) +
                        ((p$RushTDs*input$tc_numberK)-(p$RushTDs*6)) + 
                        ((p$Receptions*input$tc_numberH)-p$Receptions) +
                        ((p$RecYards*input$tc_numberJ)-(p$RecYards*0.1)) +
                        ((p$RecTDs*input$tc_numberK)-(p$RecTDs*6)))
      
      
      points <- round(mean(px),2)
      avgs2 <- rbind(avgs2, points)
    }
    
    avgs2
    rownames(avgs2) <- posttrade
    avgs2 <- replace(avgs2, avgs2=="NaN", NA)
    
    avgs1 <- as.numeric(avgs1[,1])
    avgs2 <- as.numeric(avgs2[,1])
    
    sum <- c(sum(avgs1, na.rm = TRUE),sum(avgs2, na.rm = TRUE))
    difference <- sum[2] - sum[1]
    
    if(difference <= -5){
      a <- paste("","Average PPG Pre-Trade:", sum[1], "\n",
                 "Average PPG Post-Trade:", sum[2], "\n", "\n",
                 "Yikes... no bueno here.")
    }
    
    if(difference >= 5){
      a <- paste("","Average PPG Pre-Trade:", sum[1], "\n",
                 "Average PPG Post-Trade:", sum[2], "\n", "\n",
                 "Take it and run man!")
    }
    
    if(abs(difference) <= 1 & difference != 0){
      a <- paste("","Average PPG Pre-Trade:", sum[1], "\n",
                 "Average PPG Post-Trade:", sum[2], "\n", "\n",
                 "This trade has no real affect on your overall starting lineup. Compare the pieces in the table below.")
    }
    
    if(difference < 5 & difference > 1){
      a <- paste("","Average PPG Pre-Trade:", sum[1], "\n",
                 "Average PPG Post-Trade:", sum[2], "\n", "\n",
                 "This trade improves your overall starting lineup and would be advised!")
    }
    
    if(difference > -5 & difference < -1){
      a <- paste("","Average PPG Pre-Trade:", sum[1], "\n",
                 "Average PPG Post-Trade:", sum[2], "\n", "\n",
                 "This trade hurts your overall starting lineup and would not be advised.")
    }
    
    if(difference == 0){
      a <- paste("","Average PPG Pre-Trade:", sum[1], "\n",
                 "Average PPG Post-Trade:", sum[2], "\n", "\n",
                 "Either you didn't want to use this option or you are trading bench players. If you are trading bench players, compare them in the table below.")
    }
    
    a
    
  })
  
  output$tradetable <- renderDataTable({
    
    d = NULL
    p1 <- players[players$Player == input$tradeA1,2]
    p2 <- players[players$Player == input$tradeA2,2]
    p3 <- players[players$Player == input$tradeA3,2]
    p4 <- players[players$Player == input$tradeA4,2]
    p5 <- players[players$Player == input$tradeA5,2]
    p6 <- players[players$Player == input$tradeB1,2]
    p7 <- players[players$Player == input$tradeB2,2]
    p8 <- players[players$Player == input$tradeB3,2]
    p9 <- players[players$Player == input$tradeB4,2]
    p10 <- players[players$Player == input$tradeB5,2]
    
    allplayers <- c(as.character(p1),as.character(p2),as.character(p3),
                    as.character(p4),as.character(p5),as.character(p6),
                    as.character(p7),as.character(p8),as.character(p9),
                    as.character(p10))
    allplayers <- replace(allplayers, allplayers=="None", NA)
    
    allplayers <- allplayers[!is.na(allplayers)]
    
    for(i in 1:length(allplayers)) {
      
      playerA <- as.character(allplayers[i])
      
      for (i in 1:17) {
        
        p1 <- na.omit(head(ssdata[ssdata$Player == playerA,],20))
        position <- as.character(players[players$Player == playerA,3+i])
        team <- as.character(players[players$Player == playerA,3])
        p1team <- na.omit(head(ssdata[ssdata$Team == team & ssdata$PositionTier == position,],4))
        home <- na.omit(head(ssdata[ssdata$Home == as.character(schedule_home[team,i]) & ssdata$Player == playerA,],4))
        p1opp <- ssdata[ssdata$Opponent == as.character(schedule_team[team,i]) & ssdata$Player == playerA,]
        def <- na.omit(head(totalssdata[totalssdata$Team == as.character(schedule_team[team,i]) & totalssdata$PositionTier == position,],4))
        
        
        if(position == "TE1" | position == "TE2") {
          
          p1x <- as.vector(p1$PPR4 + 
                             (p1$RushAtt*input$tc_numberG) + ((p1$RushYards*input$tc_numberJ)-(p1$RushYards*0.1)) +
                             ((p1$RushTDs*input$tc_numberK)-(p1$RushTDs*6)) + 
                             ((p1$Receptions*input$tc_numberI)-p1$Receptions) +
                             ((p1$RecYards*input$tc_numberJ)-(p1$RecYards*0.1)) +
                             ((p1$RecTDs*input$tc_numberK)-(p1$RecTDs*6)))
          
          defx <- as.vector(def$PPR4 + 
                              (def$RushAtt*input$tc_numberG) + ((def$RushYards*input$tc_numberJ)-(def$RushYards*0.1)) +
                              ((def$RushTDs*input$tc_numberK)-(def$RushTDs*6)) + 
                              ((def$Receptions*input$tc_numberI)-def$Receptions) +
                              ((def$RecYards*input$tc_numberJ)-(def$RecYards*0.1)) +
                              ((def$RecTDs*input$tc_numberK)-(def$RecTDs*6)))
          
          teamx <- as.vector(p1team$PPR4 + 
                               (p1team$RushAtt*input$tc_numberG) + ((p1team$RushYards*input$tc_numberJ)-(p1team$RushYards*0.1)) +
                               ((p1team$RushTDs*input$tc_numberK)-(p1team$RushTDs*6)) + 
                               ((p1team$Receptions*input$tc_numberI)-p1team$Receptions) +
                               ((p1team$RecYards*input$tc_numberJ)-(p1team$RecYards*0.1)) +
                               ((p1team$RecTDs*input$tc_numberK)-(p1team$RecTDs*6)))
          
          homex <- as.vector(home$PPR4 + 
                               (home$RushAtt*input$tc_numberG) + ((home$RushYards*input$tc_numberJ)-(home$RushYards*0.1)) +
                               ((home$RushTDs*input$tc_numberK)-(home$RushTDs*6)) + 
                               ((home$Receptions*input$tc_numberI)-home$Receptions) +
                               ((home$RecYards*input$tc_numberJ)-(home$RecYards*0.1)) +
                               ((home$RecTDs*input$tc_numberK)-(home$RecTDs*6)))
          
          p1oppx <- as.vector(p1opp$PPR4 + 
                                (p1opp$RushAtt*input$tc_numberG) + ((p1opp$RushYards*input$tc_numberJ)-(p1opp$RushYards*0.1)) +
                                ((p1opp$RushTDs*input$tc_numberK)-(p1opp$RushTDs*6)) + 
                                ((p1opp$Receptions*input$tc_numberI)-p1opp$Receptions) +
                                ((p1opp$RecYards*input$tc_numberJ)-(p1opp$RecYards*0.1)) +
                                ((p1opp$RecTDs*input$tc_numberK)-(p1opp$RecTDs*6)))
          
        }
        
        if(position == "RB1" | position == "RB2" | position == "RB3") {
          
          p1x <- as.vector(p1$PPR4 + 
                             (p1$RushAtt*input$tc_numberG) + ((p1$RushYards*input$tc_numberJ)-(p1$RushYards*0.1)) +
                             ((p1$RushTDs*input$tc_numberK)-(p1$RushTDs*6)) + 
                             ((p1$Receptions*input$tc_numberH)-p1$Receptions) +
                             ((p1$RecYards*input$tc_numberJ)-(p1$RecYards*0.1)) +
                             ((p1$RecTDs*input$tc_numberK)-(p1$RecTDs*6)))
          
          defx <- as.vector(def$PPR4 + 
                              (def$RushAtt*input$tc_numberG) + ((def$RushYards*input$tc_numberJ)-(def$RushYards*0.1)) +
                              ((def$RushTDs*input$tc_numberK)-(def$RushTDs*6)) + 
                              ((def$Receptions*input$tc_numberH)-def$Receptions) +
                              ((def$RecYards*input$tc_numberJ)-(def$RecYards*0.1)) +
                              ((def$RecTDs*input$tc_numberK)-(def$RecTDs*6)))
          
          teamx <- as.vector(p1team$PPR4 + 
                               (p1team$RushAtt*input$tc_numberG) + ((p1team$RushYards*input$tc_numberJ)-(p1team$RushYards*0.1)) +
                               ((p1team$RushTDs*input$tc_numberK)-(p1team$RushTDs*6)) + 
                               ((p1team$Receptions*input$tc_numberH)-p1team$Receptions) +
                               ((p1team$RecYards*input$tc_numberJ)-(p1team$RecYards*0.1)) +
                               ((p1team$RecTDs*input$tc_numberK)-(p1team$RecTDs*6)))
          
          homex <- as.vector(home$PPR4 + 
                               (home$RushAtt*input$tc_numberG) + ((home$RushYards*input$tc_numberJ)-(home$RushYards*0.1)) +
                               ((home$RushTDs*input$tc_numberK)-(home$RushTDs*6)) + 
                               ((home$Receptions*input$tc_numberH)-home$Receptions) +
                               ((home$RecYards*input$tc_numberJ)-(home$RecYards*0.1)) +
                               ((home$RecTDs*input$tc_numberK)-(home$RecTDs*6)))
          
          p1oppx <- as.vector(p1opp$PPR4 + 
                                (p1opp$RushAtt*input$tc_numberG) + ((p1opp$RushYards*input$tc_numberJ)-(p1opp$RushYards*0.1)) +
                                ((p1opp$RushTDs*input$tc_numberK)-(p1opp$RushTDs*6)) + 
                                ((p1opp$Receptions*input$tc_numberH)-p1opp$Receptions) +
                                ((p1opp$RecYards*input$tc_numberJ)-(p1opp$RecYards*0.1)) +
                                ((p1opp$RecTDs*input$tc_numberK)-(p1opp$RecTDs*6)))
          
        }
        
        if(position == "WR1" | position == "WR2" | position == "WR3" | position == "WR4") {
          
          p1x <- as.vector(p1$PPR4 + 
                             (p1$RushAtt*input$tc_numberG) + ((p1$RushYards*input$tc_numberJ)-(p1$RushYards*0.1)) +
                             ((p1$RushTDs*input$tc_numberK)-(p1$RushTDs*6)) + 
                             ((p1$Receptions*input$tc_numberH)-p1$Receptions) +
                             ((p1$RecYards*input$tc_numberJ)-(p1$RecYards*0.1)) +
                             ((p1$RecTDs*input$tc_numberK)-(p1$RecTDs*6)))
          
          defx <- as.vector(def$PPR4 + 
                              (def$RushAtt*input$tc_numberG) + ((def$RushYards*input$tc_numberJ)-(def$RushYards*0.1)) +
                              ((def$RushTDs*input$tc_numberK)-(def$RushTDs*6)) + 
                              ((def$Receptions*input$tc_numberH)-def$Receptions) +
                              ((def$RecYards*input$tc_numberJ)-(def$RecYards*0.1)) +
                              ((def$RecTDs*input$tc_numberK)-(def$RecTDs*6)))
          
          teamx <- as.vector(p1team$PPR4 + 
                               (p1team$RushAtt*input$tc_numberG) + ((p1team$RushYards*input$tc_numberJ)-(p1team$RushYards*0.1)) +
                               ((p1team$RushTDs*input$tc_numberK)-(p1team$RushTDs*6)) + 
                               ((p1team$Receptions*input$tc_numberH)-p1team$Receptions) +
                               ((p1team$RecYards*input$tc_numberJ)-(p1team$RecYards*0.1)) +
                               ((p1team$RecTDs*input$tc_numberK)-(p1team$RecTDs*6)))
          
          homex <- as.vector(home$PPR4 + 
                               (home$RushAtt*input$tc_numberG) + ((home$RushYards*input$tc_numberJ)-(home$RushYards*0.1)) +
                               ((home$RushTDs*input$tc_numberK)-(home$RushTDs*6)) + 
                               ((home$Receptions*input$tc_numberH)-home$Receptions) +
                               ((home$RecYards*input$tc_numberJ)-(home$RecYards*0.1)) +
                               ((home$RecTDs*input$tc_numberK)-(home$RecTDs*6)))
          
          p1oppx <- as.vector(p1opp$PPR4 + 
                                (p1opp$RushAtt*input$tc_numberG) + ((p1opp$RushYards*input$tc_numberJ)-(p1opp$RushYards*0.1)) +
                                ((p1opp$RushTDs*input$tc_numberK)-(p1opp$RushTDs*6)) + 
                                ((p1opp$Receptions*input$tc_numberH)-p1opp$Receptions) +
                                ((p1opp$RecYards*input$tc_numberJ)-(p1opp$RecYards*0.1)) +
                                ((p1opp$RecTDs*input$tc_numberK)-(p1opp$RecTDs*6)))
          
        }
        
        if(position == "QB1") {
          
          p1x <- as.vector(p1$PPR4 + (p1$PassAtt*input$tc_numberA) + (p1$Comp*input$tc_numberB) + 
                             ((p1$PassAtt-p1$Comp)*input$tc_numberC) +
                             ((p1$PassYards*input$tc_numberD)-(p1$PassYards/25)) +
                             ((p1$PassTDs*input$tc_numberE)-(p1$PassTDs*4)) +
                             ((p1$INT*input$tc_numberF)+(p1$INT*2)) +
                             (p1$RushAtt*input$tc_numberG) + ((p1$RushYards*input$tc_numberJ)-(p1$RushYards*0.1)) +
                             ((p1$RushTDs*input$tc_numberK)-(p1$RushTDs*6)))
          
          defx <- as.vector(def$PPR4 + (def$PassAtt*input$tc_numberA) + (def$Comp*input$tc_numberB) + 
                              ((def$PassAtt-def$Comp)*input$tc_numberC) +
                              ((def$PassYards*input$tc_numberD)-(def$PassYards/25)) +
                              ((def$PassTDs*input$tc_numberE)-(def$PassTDs*4)) +
                              ((def$INT*input$tc_numberF)+(def$INT*2)) +
                              (def$RushAtt*input$tc_numberG) + ((def$RushYards*input$tc_numberJ)-(def$RushYards*0.1)) +
                              ((def$RushTDs*input$tc_numberK)-(def$RushTDs*6)))
          
          teamx <- as.vector(p1team$PPR4 + (p1team$PassAtt*input$tc_numberA) + (p1team$Comp*input$tc_numberB) + 
                               ((p1team$PassAtt-p1team$Comp)*input$tc_numberC) +
                               ((p1team$PassYards*input$tc_numberD)-(p1team$PassYards/25)) +
                               ((p1team$PassTDs*input$tc_numberE)-(p1team$PassTDs*4)) +
                               ((p1team$INT*input$tc_numberF)+(p1team$INT*2)) +
                               (p1team$RushAtt*input$tc_numberG) + ((p1team$RushYards*input$tc_numberJ)-(p1team$RushYards*0.1)) +
                               ((p1team$RushTDs*input$tc_numberK)-(p1team$RushTDs*6)))
          
          homex <- as.vector(home$PPR4 + (home$PassAtt*input$tc_numberA) + (home$Comp*input$tc_numberB) + 
                               ((home$PassAtt-home$Comp)*input$tc_numberC) +
                               ((home$PassYards*input$tc_numberD)-(home$PassYards/25)) +
                               ((home$PassTDs*input$tc_numberE)-(home$PassTDs*4)) +
                               ((home$INT*input$tc_numberF)+(home$INT*2)) +
                               (home$RushAtt*input$tc_numberG) + ((home$RushYards*input$tc_numberJ)-(home$RushYards*0.1)) +
                               ((home$RushTDs*input$tc_numberK)-(home$RushTDs*6)))
          
          p1oppx <- as.vector(p1opp$PPR4 + (p1opp$PassAtt*input$tc_numberA) + (p1opp$Comp*input$tc_numberB) + 
                                ((p1opp$PassAtt-p1opp$Comp)*input$tc_numberC) +
                                ((p1opp$PassYards*input$tc_numberD)-(p1opp$PassYards/25)) +
                                ((p1opp$PassTDs*input$tc_numberE)-(p1opp$PassTDs*4)) +
                                ((p1opp$INT*input$tc_numberF)+(p1opp$INT*2)) +
                                (p1opp$RushAtt*input$tc_numberG) + ((p1opp$RushYards*input$tc_numberJ)-(p1opp$RushYards*0.1)) +
                                ((p1opp$RushTDs*input$tc_numberK)-(p1opp$RushTDs*6)))
          
        }
        
        if(position == "") {
          total <- NA
        }
        else {
          total <- c(p1x,p1x,p1oppx,defx, teamx, homex)
        }
        
        length(total) <- 300
        
        d = as.matrix(rbind(d, total))
        
      }
    }
    
    z = NULL
    for (i in 1:nrow(d)) {
      x <- mean(na.omit(d[i,]))
      z = rbind(z,x)
    }
    
    repnames = NULL
    for(i in 1:length(allplayers)){
      repnames1 <- rep(allplayers[i],17)
      repnames = c(repnames, repnames1)
    }
    
    
    z2 <- as.data.frame(cbind(repnames,z))
    z3 <- t(as.matrix(z2))
    
    proj <- round(as.numeric(z3[2,1:length(z3[2,])]),2)
    
    proj1 <- proj[1:17]
    proj2 <- proj[18:34]
    proj3 <- proj[35:51]
    proj4 <- proj[52:68]
    proj5 <- proj[69:85]
    proj6 <- proj[86:102]
    proj7 <- proj[103:119]
    proj8 <- proj[120:136]
    proj9 <- proj[137:153]
    proj10 <- proj[154:170]
    
    projA <- as.data.frame(rbind(proj1,proj2,proj3,proj4,proj5,proj6,proj7,proj8,proj9,proj10))
    projA <- projA[rowSums(is.na(projA)) != ncol(projA), ]
    projA <- replace(projA, projA=="NaN", NA)
    Sum <- rowSums(projA, na.rm = TRUE)
    projA <- cbind(projA, Sum)
    colnames(projA) <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","Total")
    rownames(projA) <- allplayers
    
    projA
    
  }, options = list(dom = "t"))
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

