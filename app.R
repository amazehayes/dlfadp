# DLF ADP for Ryan

library(shiny)
library(DT)
library(dplyr)
options(stringsAsFactors = FALSE)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
mround <- function(x,base){ 
  base*ceiling(x/base) 
}
adpvalues <- read.csv("adpvalues.csv")
fwd <- read.csv("finalWeeklyData.csv")
fwd <- fwd %>% filter(Year >= 2013)
adp <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSQloxI27OHMMgEOpWlGLaeItRXD2cGppirBxj2czNU5yXjDRRkkDUnwvY0J04Pxb8cADLkuyqqzlgH/pub?output=csv")
adp$Average <- round(rowMeans(adp[,9:14], na.rm = TRUE),2)
minadp <- c()
maxadp <- c()
stdadp <- c()
for(i in 1:nrow(adp)){
  loopv <- adp[i,10:15]
  minadp <- c(minadp,min(loopv, na.rm = TRUE))
  maxadp <- c(maxadp,max(loopv, na.rm = TRUE))
  stdadp <- c(stdadp,round(sd(loopv, na.rm = TRUE),2))
}
adp$Min <- minadp
adp$Max <- maxadp
adp$StdDev <- stdadp
adp$Player2 <- paste0(adp$Player,", ",adp$Position)
adp$YM <- paste0(substr(adp$Month,1,3),substrRight(adp$Year,2))


ui <- fluidPage(tags$head(includeScript("analystics.js")),
   
  titlePanel(title = NULL),
  h3("Dynasty League Football ADP"),
  br(),
  
  tabsetPanel(
    
    tabPanel("ADP Data", fluid = TRUE,
             br(),
             dataTableOutput("adpdata")
    ),
    
    tabPanel("ADP vs. Rank", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("adprank_player","Select Player:",choices = sort(unique(as.character(adp$Player2))),
                      selected = "Russell Wilson, QB"),
          dataTableOutput("adpranktable")
        ),
        mainPanel(plotOutput("adprankgraph"))
      )
    ),
    
    tabPanel("ADP by Year", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(width = 3,
          radioButtons("adpyear_tog","Select ADP:", choices = c("Overall","Positional"),
                       selected = "Overall", inline = TRUE),
          selectInput("adpyear_playerA","Select Player:",
                      choices = c("None",sort(unique(as.character(adp$Player2)))),
                      selected = "Russell Wilson, QB"),
          selectInput("adpyear_playerB","Select Player:",
                      choices = c("None",sort(unique(as.character(adp$Player2)))),
                      selected = "Cam Newton, QB"),
          selectInput("adpyear_playerC","Select Player:",
                      choices = c("None",sort(unique(as.character(adp$Player2)))),
                      selected = "None")
        ),
        mainPanel(plotOutput("adpyeargraph"))
      ),
      br(),
      dataTableOutput("adpyeartable")
    ),

    tabPanel("ADP by Month", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(width = 3,
          radioButtons("adpmonth_tog","Select ADP:", choices = c("Overall","Positional"),
                       selected = "Overall", inline = TRUE),
          selectInput("adpmonth_playerA","Select Player:",c("None",sort(unique(as.character(adp$Player2)))),
                      selected = "Russell Wilson, QB"),
          selectInput("adpmonth_playerB","Select Player:",c("None",sort(unique(as.character(adp$Player2)))),
                      selected = "Cam Newton, QB"),
          selectInput("adpmonth_playerC","Select Player:",c("None",sort(unique(as.character(adp$Player2)))),
                      selected = "None"),
          selectInput("adpmonth","Select Month(s):", choices = adp$YM,
                      multiple = TRUE,
                      selected = tail(rev(adp$YM),10))
        ),
        mainPanel(plotOutput("adpmonthgraph"))
      ),
      br(),
      dataTableOutput("adpmonthtable")
    ),

    tabPanel("Change in ADP", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          selectInput("delta_monthA","Select Oldest Month:",choices = unique(as.character(adp$Month)),
                      selected = "January"),
          selectInput("delta_yearA","Select Oldest Year:",choices = unique(as.character(adp$Year)),
                      selected = 2018),
          br(),
          selectInput("delta_monthB","Select Newest Month:",choices = unique(as.character(adp$Month)),
                      selected = "November"),
          selectInput("delta_yearB","Select Newest Year:",choices = unique(as.character(adp$Year)),
                      selected = 2018),
          br(),
          strong("Please be sure to input the most recent month and year into the third and fourth boxes.")
        ),
        mainPanel(dataTableOutput("deltatable"))
      ),
      br(),
      br(),
      div(img(src = 'adpvaluetable.PNG')),
      br(),
      br()
    )
    
    
  )
  
)


server <- function(input, output) {

  output$adpdata <- renderDataTable({
    
    adp[,1:18]
    
  }, rownames = FALSE, filter = "top", options = list(lengthMenu = c(50,100,200), pageLength = 50))
  
  output$adprankgraph <- renderPlot({
    
    player <- input$adprank_player
    
    p1df <- adp %>% filter(Player2 == player)
    years <- unique(p1df$Year)
    
    yearlydf <- fwd %>% select(Year, Player2, Player, Position, Team, PPR) %>%
      group_by(Year, Player2) %>%
      summarise(PPR_Total = sum(PPR))
    positions <- unique(fwd %>% select(Year, Player2, Position))
    
    yearlydf <- merge(yearlydf, positions, by = c("Year","Player2"))
    yearlydf <- yearlydf %>% 
      group_by(Year,Position) %>%
      mutate(PPR_Rank = rank(-PPR_Total, ties.method = "first"))
    yearlydf <- yearlydf %>% select(Year,Player2,Position,PPR_Rank)
    
    adprank <- matrix(ncol = 2, nrow = length(years))
    rownames(adprank) <- years
    colnames(adprank) <- c("AvgPosADP","PosRank")
    for(i in 1:nrow(adprank)){
      dfloop1 <- p1df %>% filter(Year == years[i])
      a <- round(mean(dfloop1$PosADP, na.rm = TRUE),2)
      dfloop2 <- yearlydf %>% filter(Player2 == player, Year == years[i])
      r <- dfloop2$PPR_Rank
      adprank[i,] <- c(a,r)
    }
    adprank <- as.data.frame(adprank)
    adprank$AvgPosADP <- as.numeric(adprank$AvgPosADP)
    adprank <- adprank[nrow(adprank):1,]
    pmax <- mround(max(adprank$AvgPosADP,adprank$PosRank),5) + 5
    
    plot(adprank$AvgPosADP, ylim = rev(c(1,pmax)), type = "o", axes = FALSE, xlab = "Year", pch = 19,
         ylab = "ADP/Rank",main = paste("Positional ADP vs. Rank for", player),col = c("red"))
    par(new = TRUE)
    plot(adprank$PosRank, ylim = rev(c(1,pmax)), type = "o", axes = FALSE, xlab = "Year", pch = 19,
         ylab = "",col = c("blue"))
    axis(1, at=1:length(years), lab=years)
    axis(2, at=c(1,1:pmax*5))
    legend("bottom",cex = 0.7, horiz = TRUE, bty = "n", c("ADP","Rank"), col = c("red","blue"), lwd = 8)
    
  })
  
  output$adpranktable <- renderDataTable({
    
    player <- input$adprank_player
    
    p1df <- adp %>% filter(Player2 == player)
    years <- unique(p1df$Year)
    
    yearlydf <- fwd %>% select(Year, Player2, Player, Position, Team, PPR) %>%
      group_by(Year, Player2) %>%
      summarise(PPR_Total = sum(PPR))
    positions <- unique(fwd %>% select(Year, Player2, Position))
    
    yearlydf <- merge(yearlydf, positions, by = c("Year","Player2"))
    yearlydf <- yearlydf %>% 
      group_by(Year,Position) %>%
      mutate(PPR_Rank = rank(-PPR_Total, ties.method = "first"))
    yearlydf <- yearlydf %>% select(Year,Player2,Position,PPR_Rank)
    
    adprank <- matrix(ncol = 2, nrow = length(years))
    rownames(adprank) <- years
    colnames(adprank) <- c("AvgPosADP","PosRank")
    for(i in 1:nrow(adprank)){
      dfloop1 <- p1df %>% filter(Year == years[i])
      a <- round(mean(dfloop1$PosADP, na.rm = TRUE),2)
      dfloop2 <- yearlydf %>% filter(Player2 == player, Year == years[i])
      r <- dfloop2$PPR_Rank
      adprank[i,] <- c(a,r)
    }
    adprank <- as.data.frame(adprank)
    adprank$AvgPosADP <- as.numeric(adprank$AvgPosADP)
    
    adprank
    
  }, options = list(dom = "t"))
  
  
  
  output$adpyeargraph <- renderPlot({
    
    playerA <- input$adpyear_playerA
    playerB <- input$adpyear_playerB
    playerC <- input$adpyear_playerC
    players <- c(playerA,playerB,playerC)
    years <- rev(unique(adp$Year))
    tog <- input$adpyear_tog
    
    
    if(tog == "Overall"){
      adpyear <- matrix(ncol = length(years), nrow = 3)
      colnames(adpyear) <- years
      rownames(adpyear) <- players
      for(i in 1:length(players)){
        v <- c()
        if(players[i] == "None"){
          v <- c(rep(NA,length(years)))
        }
        if(players[i] != "None"){
          for(j in 1:length(years)){
            dfloop <- adp %>% filter(Year == years[j], Player2 == players[i])
            if(nrow(dfloop) == 0){
              a <- NA
            }
            if(nrow(dfloop) > 0){
              a <- round(mean(dfloop$OverallADP,na.rm = TRUE),2)
            }
            v <- c(v,a)
          }
        }
        adpyear[i,] <- v
      }
    }
    if(tog == "Positional"){
      adpyear <- matrix(ncol = length(years), nrow = 3)
      colnames(adpyear) <- years
      rownames(adpyear) <- players
      for(i in 1:length(players)){
        v <- c()
        if(players[i] == "None"){
          v <- c(rep(NA,length(years)))
        }
        if(players[i] != "None"){
          for(j in 1:length(years)){
            dfloop <- adp %>% filter(Year == years[j], Player2 == players[i])
            if(nrow(dfloop) == 0){
              a <- NA
            }
            if(nrow(dfloop) > 0){
              a <- round(mean(dfloop$OverallADP,na.rm = TRUE),2)
            }
            v <- c(v,a)
          }
        }
        adpyear[i,] <- v
      }
    }
    
    
    if(playerA != "None" & playerB == "None" & playerC == "None"){
      m <- paste(tog,"Yearly ADP for", playerA)
      t <- playerA
      cc <- "red"
    }
    if(playerA == "None" & playerB != "None" & playerC == "None"){
      m <- paste(tog,"Yearly ADP for", playerB)
      t <- playerB
      cc <- "blue"
    }
    if(playerA == "None" & playerB == "None" & playerC != "None"){
      m <- paste(tog,"Yearly ADP for", playerC)
      t <- playerC
      cc <- rgb(0,1,0,0.5)
    }
    if(playerA != "None" & playerB != "None" & playerC == "None"){
      m <- paste(tog,"Yearly ADP for", playerA, "&", playerB)
      t <- c(playerA,playerB)
      cc <- c("red","blue")
    }
    if(playerA != "None" & playerB == "None" & playerC != "None"){
      m <- paste(tog,"Yearly ADP for", playerA, "&", playerC)
      t <- c(playerA,playerC)
      cc <- c("red",rgb(0,1,0,0.5))
    }
    if(playerA == "None" & playerB != "None" & playerC != "None"){
      m <- paste(tog,"Yearly ADP for", playerB, "&", playerC)
      t <- c(playerB,playerC)
      cc <- c("blue",rgb(0,1,0,0.5))
    }
    if(playerA != "None" & playerB != "None" & playerC != "None"){
      m <- paste(tog,"Yearly ADP for", playerA, "&", playerB, "&", playerC)
      t <- c(playerA,playerB,playerC)
      cc <- c("red","blue",rgb(0,1,0,0.5))
    }
    pmax <- mround(max(adpyear[1,],adpyear[2,],adpyear[3,], na.rm = TRUE),5) + 5
    
    plot(adpyear[1,], ylim = rev(c(1,pmax)), type = "o", axes = FALSE, xlab = "Year", pch = 19,
         ylab = paste(tog,"ADP"),main = m,col = c("red"))
    par(new = TRUE)
    plot(adpyear[2,], ylim = rev(c(1,pmax)), type = "o", axes = FALSE, xlab = "Year", pch = 19,
         ylab = "",col = c("blue"))
    par(new = TRUE)
    plot(adpyear[3,], ylim = rev(c(1,pmax)), type = "o", axes = FALSE, xlab = "Year", pch = 19,
         ylab = "",col = rgb(0,1,0,0.5))
    axis(1, at=1:length(years), lab=years)
    axis(2, at=c(1,1:pmax*5))
    legend("bottom",cex = 0.7, horiz = TRUE, bty = "n", t, col = cc, lwd = 8)
    
    
  })
  
  output$adpyeartable <- renderDataTable({
    
    playerA <- input$adpyear_playerA
    playerB <- input$adpyear_playerB
    playerC <- input$adpyear_playerC
    players <- c(playerA,playerB,playerC)
    years <- rev(unique(adp$Year))
    tog <- input$adpyear_tog
    
    
    if(tog == "Overall"){
      adpyear <- matrix(ncol = length(years), nrow = 3)
      colnames(adpyear) <- years
      rownames(adpyear) <- players
      for(i in 1:length(players)){
        v <- c()
        if(players[i] == "None"){
          v <- c(rep(NA,length(years)))
        }
        if(players[i] != "None"){
          for(j in 1:length(years)){
            dfloop <- adp %>% filter(Year == years[j], Player2 == players[i])
            if(nrow(dfloop) == 0){
              a <- NA
            }
            if(nrow(dfloop) > 0){
              a <- round(mean(dfloop$OverallADP,na.rm = TRUE),2)
            }
            v <- c(v,a)
          }
        }
        adpyear[i,] <- v
      }
    }
    if(tog == "Positional"){
      adpyear <- matrix(ncol = length(years), nrow = 3)
      colnames(adpyear) <- years
      rownames(adpyear) <- players
      for(i in 1:length(players)){
        v <- c()
        if(players[i] == "None"){
          v <- c(rep(NA,length(years)))
        }
        if(players[i] != "None"){
          for(j in 1:length(years)){
            dfloop <- adp %>% filter(Year == years[j], Player2 == players[i])
            if(nrow(dfloop) == 0){
              a <- NA
            }
            if(nrow(dfloop) > 0){
              a <- round(mean(dfloop$OverallADP,na.rm = TRUE),2)
            }
            v <- c(v,a)
          }
        }
        adpyear[i,] <- v
      }
    }
    
    adpyear
    
  }, options = list(dom = "t"))
  
  
  output$adpmonthgraph <- renderPlot({
    
    playerA <- input$adpmonth_playerA
    playerB <- input$adpmonth_playerB
    playerC <- input$adpmonth_playerC
    players <- c(playerA,playerB,playerC)
    months <- c("Jun18","Jul18","Aug18","Sep18","Oct18","Nov18")
    tog <- input$adpmonth_tog
    
    if(tog == "Overall"){
      adpmonth <- matrix(ncol = length(months), nrow = 3)
      colnames(adpmonth) <- months
      rownames(adpmonth) <- players
      for(i in 1:length(players)){
        v <- c()
        if(players[i] == "None"){
          v <- c(rep(NA,length(months)))
        }
        if(players[i] != "None"){
          for(j in 1:length(months)){
            dfloop <- adp %>% filter(YM %in% months[j], Player2 == players[i])
            v <- c(v,dfloop$OverallADP)
          }
        }
        adpmonth[i,] <- v
      }
    }
    
    if(tog == "Positional"){
      adpmonth <- matrix(ncol = length(months), nrow = 3)
      colnames(adpmonth) <- months
      rownames(adpmonth) <- players
      for(i in 1:length(players)){
        v <- c()
        if(players[i] == "None"){
          v <- c(rep(NA,length(months)))
        }
        if(players[i] != "None"){
          for(j in 1:length(months)){
            dfloop <- adp %>% filter(YM %in% months[j], Player2 == players[i])
            v <- c(v,dfloop$PosADP)
          }
        }
        adpmonth[i,] <- v
      }
    }
    
    if(playerA != "None" & playerB == "None" & playerC == "None"){
      m <- paste(tog,"Monthly ADP for", playerA)
      t <- playerA
      cc <- "red"
    }
    if(playerA == "None" & playerB != "None" & playerC == "None"){
      m <- paste(tog,"Monthly ADP for", playerB)
      t <- playerB
      cc <- "blue"
    }
    if(playerA == "None" & playerB == "None" & playerC != "None"){
      m <- paste(tog,"Monthly ADP for", playerC)
      t <- playerC
      cc <- rgb(0,1,0,0.5)
    }
    if(playerA != "None" & playerB != "None" & playerC == "None"){
      m <- paste(tog,"Monthly ADP for", playerA, "&", playerB)
      t <- c(playerA,playerB)
      cc <- c("red","blue")
    }
    if(playerA != "None" & playerB == "None" & playerC != "None"){
      m <- paste(tog,"Monthly ADP for", playerA, "&", playerC)
      t <- c(playerA,playerC)
      cc <- c("red",rgb(0,1,0,0.5))
    }
    if(playerA == "None" & playerB != "None" & playerC != "None"){
      m <- paste(tog,"Monthly ADP for", playerB, "&", playerC)
      t <- c(playerB,playerC)
      cc <- c("blue",rgb(0,1,0,0.5))
    }
    if(playerA != "None" & playerB != "None" & playerC != "None"){
      m <- paste(tog,"Monthly ADP for", playerA, "&", playerB, "&", playerC)
      t <- c(playerA,playerB,playerC)
      cc <- c("red","blue",rgb(0,1,0,0.5))
    }
    pmax <- mround(max(adpmonth[1,],adpmonth[2,],adpmonth[3,], na.rm = TRUE),5) + 5
    
    plot(adpmonth[1,], ylim = rev(c(1,pmax)), type = "o", axes = FALSE, xlab = "Month", pch = 19,
         ylab = paste(tog,"ADP"),main = m,col = c("red"))
    par(new = TRUE)
    plot(adpmonth[2,], ylim = rev(c(1,pmax)), type = "o", axes = FALSE, xlab = "Month", pch = 19,
         ylab = "",col = c("blue"))
    par(new = TRUE)
    plot(adpmonth[3,], ylim = rev(c(1,pmax)), type = "o", axes = FALSE, xlab = "Month", pch = 19,
         ylab = "",col = rgb(0,1,0,0.5))
    axis(1, at=1:length(months), lab=months)
    axis(2, at=c(1,1:pmax*5))
    legend("bottom",cex = 0.7, horiz = TRUE, bty = "n", t, col = cc, lwd = 8)
    
  })
  
  output$adpmonthtable <- renderDataTable({
    
    playerA <- input$adpmonth_playerA
    playerB <- input$adpmonth_playerB
    playerC <- input$adpmonth_playerC
    players <- c(playerA,playerB,playerC)
    months <- c("Jun18","Jul18","Aug18","Sep18","Oct18","Nov18")
    tog <- input$adpmonth_tog
    
    if(tog == "Overall"){
      adpmonth <- matrix(ncol = length(months), nrow = 3)
      colnames(adpmonth) <- months
      rownames(adpmonth) <- players
      for(i in 1:length(players)){
        v <- c()
        if(players[i] == "None"){
          v <- c(rep(NA,length(months)))
        }
        if(players[i] != "None"){
          for(j in 1:length(months)){
            dfloop <- adp %>% filter(YM %in% months[j], Player2 == players[i])
            v <- c(v,dfloop$OverallADP)
          }
        }
        adpmonth[i,] <- v
      }
    }
    
    if(tog == "Positional"){
      adpmonth <- matrix(ncol = length(months), nrow = 3)
      colnames(adpmonth) <- months
      rownames(adpmonth) <- players
      for(i in 1:length(players)){
        v <- c()
        if(players[i] == "None"){
          v <- c(rep(NA,length(months)))
        }
        if(players[i] != "None"){
          for(j in 1:length(months)){
            dfloop <- adp %>% filter(YM %in% months[j], Player2 == players[i])
            v <- c(v,dfloop$PosADP)
          }
        }
        adpmonth[i,] <- v
      }
    }
    
    adpmonth
    
  })
  
  output$deltatable <- renderDataTable({
    
    adp <- merge(adp,adpvalues, by = "OverallADP")
    
    month1 <- adp %>% filter(Month == input$delta_monthA, Year == input$delta_yearA) %>%
      select(Year, Month, Player2,OverallADP,ADPValue)
    month2 <- adp %>% filter(Month == input$delta_monthB, Year == input$delta_yearB) %>%
      select(Year, Month, Player2,OverallADP,ADPValue)
    
    test <- merge(month1,month2, by = "Player2")
    test$Difference <- test$ADPValue.y-test$ADPValue.x
    delta <- test[order(test$Difference, decreasing = TRUE),c(1,10,4,8)]

    colnames(delta) <- c("Player","Change in ADP Value",paste(input$delta_monthA,input$delta_yearA,"ADP"),paste(input$delta_monthB,input$delta_yearB,"ADP"))
    delta$`Change in ADP Value` <- round(as.numeric(delta$`Change in ADP Value`),2)
    
    delta
    
  }, rownames = FALSE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

