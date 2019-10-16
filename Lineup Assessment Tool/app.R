
# Load packages

library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(shiny)
library(DT)
library(tidyverse)

# Read data

TwoMan <- read_excel("data/LINEUP DATA.xlsx", sheet = "2Man")
ThreeMan <- read_excel("data/LINEUP DATA.xlsx", sheet = "3Man")
FourMan <- read_excel("data/LINEUP DATA.xlsx", sheet = "4Man")
FiveMan <- read_excel("data/LINEUP DATA.xlsx", sheet = "5Man")

# Quick check

for (i in list(TwoMan, ThreeMan, FourMan, FiveMan)) {
  if (sum(i$`+/-` != -1*i$`OPP +/-`) == 0 ) {
    print("data looks good")
  }
  else {
    print("data has some errors")
  }
}

# Calculating possessions

for (i in c("TwoMan", "ThreeMan", "FourMan", "FiveMan")) {
  assign(i, get(i) %>%
    mutate(POSS = round(100*PTS/OFFRTG),
           `OPP POSS` = round(100*`OPP PTS`/DEFRTG)) %>%
    mutate_all(funs(ifelse(is.nan(.), 0, .))))
}

# Dropping advanced stats

for (i in c("TwoMan", "ThreeMan", "FourMan", "FiveMan")) {
  assign(paste(i,"cleaned", sep = "_"), get(i) %>% 
           select(-(OFFRTG:PIE), -`+/-`, -`OPP +/-`) %>%
           mutate(`EFG%` = round((FGM + 0.5*`3PM`)/FGA, 3)*100,
                  `TS%` = round(PTS/(2*(FGA+(.44*FTA))), 3)*100,
                  `OPP EFG%` = round((`OPP FGM` + 0.5*`OPP 3PM`)/`OPP FGA`, 3)*100,
                  `OPP TS%` = round(`OPP PTS`/(2*(`OPP FGA`+(.44*`OPP FTA`))), 3)*100) %>%
           mutate_all(funs(ifelse(is.nan(.), 0, .))) %>%
           select(LINEUPS:`OPP PITP`,POSS:`OPP TS%`,RESULT))
}

# Summing stats across wins & losses

for (i in c("TwoMan", "ThreeMan", "FourMan", "FiveMan")) {
  assign(paste(i,"combined", sep = "_"), 
         get(paste(i,"cleaned", sep = "_")) %>% 
           select(-RESULT) %>%
           group_by(LINEUPS, SEASON, TEAM) %>%
           summarise_at(vars(GP:`OPP POSS`), funs(sum(.))) %>%
           mutate(`FG%` = 100*round(FGM/FGA,3),
                  `3P%` = 100*round(`3PM`/`3PA`,3),
                  `FT%` = 100*round(FTM/FTA,3),
                  `OPP FG%` = 100*round(`OPP FGM`/`OPP FGA`,3),
                  `OPP 3P%` = 100*round(`OPP 3PM`/`OPP 3PA`,3),
                  `OPP FT%` = 100*round(`OPP FTM`/`OPP FTA`,3),
                  `EFG%` = round((FGM + 0.5*`3PM`)/FGA, 3)*100,
                  `TS%` = round(PTS/(2*(FGA+(.44*FTA))), 3)*100,
                  `OPP EFG%` = round((`OPP FGM` + 0.5*`OPP 3PM`)/`OPP FGA`, 3)*100,
                  `OPP TS%` = round(`OPP PTS`/(2*(`OPP FGA`+(.44*`OPP FTA`))), 3)*100) %>%
           mutate_all(funs(ifelse(is.nan(.), 0, .))) %>%
           mutate(RESULT = "Combined"))
}

# Combining dataframes

for (i in c("TwoMan", "ThreeMan", "FourMan", "FiveMan")) {
  assign(paste(i, "final", sep = "_"),
         rbind(as.data.frame(get(paste(i, "cleaned", sep = "_"))), as.data.frame(get(paste(i, "combined", sep = "_")))))
}

# Clearing up workspace

rm(list=setdiff(ls(), c("TwoMan_final", "ThreeMan_final", "FourMan_final", "FiveMan_final")))

# Rearranging columns

for (i in c("TwoMan", "ThreeMan", "FourMan", "FiveMan")) {
  assign(paste(i, "final", sep = "_"),
         get(paste(i,"final", sep = "_")) %>%
           select(LINEUPS:MIN, POSS:`OPP POSS`, PTS:FGA, `3PM`:`3PA`, FTM:FTA, OREB:PFD, `OPP PTS`, `OPP FGM`:`OPP FGA`, `OPP 3PM`:`OPP 3PA`, `OPP FTM`:`OPP FTA`, `OPP OREB`:`OPP PITP`,
                  `FG%`, `3P%`, `FT%`, `EFG%`, `TS%`, `OPP FG%`, `OPP 3P%`, `OPP FT%`, `OPP EFG%`, `OPP TS%`, RESULT))
}

rm(i)


#Setting table template

opp_stats <- names(TwoMan_final)[grepl( "OPP" , names( TwoMan_final ) )]
tm_stats <- names(TwoMan_final)[!grepl( "OPP" , names( TwoMan_final ) )]
tm_stats <- tm_stats[-c(1:5,33)]


# UI for app

ui <- fluidPage(
  titlePanel("Lineup Assessment Tool"),
  sidebarLayout(
    sidebarPanel(width = 4,
                 helpText('This application allows users to perform in-depth analysis on a single lineup, as well as compare lineups across a number of different statistical measures.'),
                 br(), br(),
                 selectInput("num", "Number of players in lineup:", width = "50%",
                             choices = as.character(2:5)),
                 br(),
                 radioButtons("data", "Per Mode:", choices = c("Totals", "Per Game", "Per 100 Poss", "Per 48 Min")),
                 br(),
                 radioButtons("result", "Game Result:", choices = c("All", "Wins", "Losses")),
                 br(),
                 "The selection below controls what is shown on the 'Lineup Breakdown' tab.",
                 uiOutput("lineup"),
                 br(),
                 "The selection below controls what is shown on the 'Lineup Comparison' tab.",
                 selectInput("stat", h3("Area of focus:"), choices = tm_stats)
    ),
    mainPanel( tags$style(type="text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output_error:before { visibility: hidden; }"),
      tabsetPanel(
        tabPanel("Lineup Breakdown",
                 br(),
                 p("On/off court values in each statistical category are provided for the selected lineup. These can be used to identify strengths and weaknesses of the lineup.",
                   style = "font-family: 'times'; font-size:14pt"),
                 br(),br(),br(),
                 fluidRow(align='center', htmlOutput("info")),
                 br(),br(),
                 fluidRow(align='center', dataTableOutput("summary"))
                 ),
        tabPanel("Lineup Comparison",
                 br(),
                 p("On/off court values for lineups that have played 100 minutes or more are shown. This can be used to quickly compare different lineup combinations within a particular statistical category.",
                   style = "font-family: 'times'; font-size:14pt"),
                 br(),br(),br(),
                 fluidRow(align = 'center', dataTableOutput("compare")))
      )
    )
  )
)

server <- function(input, output, session) {
  
  dataInput <- reactive({
    switch (input$num,
      "2" = TwoMan_final,
      "3" = ThreeMan_final,
      "4" = FourMan_final,
      "5" = FiveMan_final
    )
  })
  
  gameRes <- reactive({
    switch(input$result,
           "All" = "Combined",
           "Wins" = "W",
           "Losses" = "L")
  })
  
  output$lineup <- renderUI({
    dataset <- dataInput()
    selectInput("players", h3("Lineup:"), choices = sort(unique(dataset$LINEUPS)))
  })
  
  output$info <- renderUI({
    dataset <- dataInput()
    dataset <- dataset %>% filter(LINEUPS %in% input$players)
    
    if(sum(dataset$RESULT=='W')>0) {W <- dataset$GP[dataset$RESULT=='W']} else {W <- 0}
    if(sum(dataset$RESULT=='L')>0) {L <- dataset$GP[dataset$RESULT=='L']} else {L <- 0}
    MIN <- dataset$MIN[dataset$RESULT=='Combined']
    
    if(MIN > 100) {
      x <- paste0("<font size=5 color=blue>","<strong>",input$players,"</strong>","</font>",
                "<br>",
                "<font size=4>",W,"W - ",L,"L","</font>",
                "<br>",
                "<font size=4>",MIN," minutes","</font>")
    HTML(x)
    } else {
      x <- paste0("<font size=5 color=blue>","<strong>",input$players,"</strong>","</font>",
                  "<br>",
                  "<font size=4>",W,"W - ",L,"L","</font>",
                  "<br>",
                  "<font size=4>",MIN," minutes","</font>",
                  "<br>", "<br>",
                  "<font size=3 color=red>","This lineup has not seen much playing time, which affects results.")
      HTML(x)
    }
  })
  
  
  output$summary <- renderDataTable({
    dataset <- dataInput()
    dataset <- dataset %>% filter(LINEUPS %in% input$players & RESULT %in% gameRes())
    
    players <- unlist(str_split(dataset$LINEUPS, ", "))
    
    others_lineups <- FiveMan_final[!apply(sapply(players, grepl, FiveMan_final$LINEUPS), 1, all) , ] %>%
      filter(RESULT %in% gameRes()) %>%
      group_by(SEASON, TEAM) %>%
      summarise_at(vars(GP:`OPP TS%`), funs(sum(., na.rm = T))) %>%
      mutate(`FG%` = 100*round(FGM/FGA,3),
             `3P%` = 100*round(`3PM`/`3PA`,3),
             `FT%` = 100*round(FTM/FTA,3),
             `OPP FG%` = 100*round(`OPP FGM`/`OPP FGA`,3),
             `OPP 3P%` = 100*round(`OPP 3PM`/`OPP 3PA`,3),
             `OPP FT%` = 100*round(`OPP FTM`/`OPP FTA`,3),
             `EFG%` = round((FGM + 0.5*`3PM`)/FGA, 3)*100,
             `TS%` = round(PTS/(2*(FGA+(.44*FTA))), 3)*100,
             `OPP EFG%` = round((`OPP FGM` + 0.5*`OPP 3PM`)/`OPP FGA`, 3)*100,
             `OPP TS%` = round(`OPP PTS`/(2*(`OPP FGA`+(.44*`OPP FTA`))), 3)*100)
    
    table <- as.data.frame(matrix(NA, nrow = length(tm_stats), ncol = 7))
    names(table) <- c('Tm On-Court', 'Tm Off-Court', 'Tm Swing', 'Opp On-Court', 'Opp Off-Court', 'Opp Swing', 'Difference in Swing')
    rownames(table) <- tm_stats
    
    if (input$data == "Totals") {
      dataset <- dataset
      others_lineups <- others_lineups
    } else if (input$data == "Per Game") {
      dataset <- dataset %>% mutate_at(vars(POSS:`OPP PITP`), funs(round(./GP, 2)))
      others_lineups <- others_lineups %>% mutate_at(vars(POSS:`OPP PITP`), funs(round(./GP, 2)))
    } else if (input$data == "Per 100 Poss") {
      dataset <- dataset %>%
        mutate(POSS_2 = POSS) %>%
        mutate_at(vars(POSS:`OPP PITP`), funs(round(100*./POSS_2, 2)))
      others_lineups <- others_lineups %>%
        mutate(POSS_2 = POSS) %>%
        mutate_at(vars(POSS:`OPP PITP`), funs(round(100*./POSS_2, 2)))
    } else if (input$data == "Per 48 Min") {
      dataset <- dataset %>% mutate_at(vars(POSS:`OPP PITP`), funs(round(48*./MIN, 2)))
      others_lineups <- others_lineups %>% mutate_at(vars(POSS:`OPP PITP`), funs(round(48*./MIN, 2)))
    }
    
    for (i in 1:length(tm_stats)) {
      table[i, 1] <- unname(unlist(dataset[tm_stats[i]]))
      table[i, 2] <- unname(unlist(others_lineups[tm_stats[i]]))
      table[i, 3] <- round(table[i, 1] - table[i, 2], 1)
      
      table[i, 4] <- unname(unlist(dataset[opp_stats[i]]))
      table[i, 5] <- unname(unlist(others_lineups[opp_stats[i]]))
      table[i, 6] <- round(table[i, 4] - table[i, 5], 1)
      
      table[i, 7] <- round(table[i, 3] - table[i, 6], 1)
    }
    
    table <- table %>% 
      rownames_to_column('stat') %>%
      arrange(desc(`Difference in Swing`)) %>%
      column_to_rownames('stat')
    
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, ''),
          th(colspan = 3, 'TEAM'),
          th(colspan = 3, 'OPPONENT')
        ),
        tr(
          lapply(c(rep(c('On-Court', 'Off-Court', 'Swing'), 2), 'Difference in Swing'), th)
        )
      )
    ))
    
    datatable(table, container = sketch, rownames = TRUE)
  })

  
  output$compare <- renderDataTable({
    
    tmStat <- input$stat
    oppStat <- paste('OPP', tmStat)
    
    dataset <- dataInput()
    dataset <- dataset %>% filter(RESULT %in% gameRes() & MIN >= 100)
    
    table <- as.data.frame(matrix(NA, nrow = nrow(dataset), ncol = 10))
    names(table) <- c('Lineup', 'GP', 'MIN', 'Tm On-Court', 'Tm Off-Court', 'Tm Swing', 'Opp On-Court', 'Opp Off-Court', 'Opp Swing', 'Difference in Swing')
    table$Lineup <- dataset$LINEUPS
    table$GP <- dataset$GP
    table$MIN <- dataset$MIN
     
    others_lineups <- vector("list", length = nrow(dataset))
    
    for (i in 1:nrow(dataset)) {
      players <- unlist(str_split(dataset$LINEUPS[i], ", "))
      others_lineups[[i]] <- FiveMan_final[!apply(sapply(players, grepl, FiveMan_final$LINEUPS), 1, all) , ] %>%
        filter(RESULT %in% gameRes()) %>%
        group_by(SEASON, TEAM) %>%
        summarise_at(vars(GP:`OPP TS%`), funs(sum(., na.rm = T))) %>%
        mutate(`FG%` = 100*round(FGM/FGA,3),
               `3P%` = 100*round(`3PM`/`3PA`,3),
               `FT%` = 100*round(FTM/FTA,3),
               `OPP FG%` = 100*round(`OPP FGM`/`OPP FGA`,3),
               `OPP 3P%` = 100*round(`OPP 3PM`/`OPP 3PA`,3),
               `OPP FT%` = 100*round(`OPP FTM`/`OPP FTA`,3),
               `EFG%` = round((FGM + 0.5*`3PM`)/FGA, 3)*100,
               `TS%` = round(PTS/(2*(FGA+(.44*FTA))), 3)*100,
               `OPP EFG%` = round((`OPP FGM` + 0.5*`OPP 3PM`)/`OPP FGA`, 3)*100,
               `OPP TS%` = round(`OPP PTS`/(2*(`OPP FGA`+(.44*`OPP FTA`))), 3)*100)
    }
    
    others_lineups <- do.call('rbind', others_lineups)
    
    if (input$data == "Totals") {
      dataset <- dataset
      others_lineups <- others_lineups
    } else if (input$data == "Per Game") {
      dataset <- dataset %>% mutate_at(vars(POSS:`OPP PITP`), funs(round(./GP, 2)))
      others_lineups <- others_lineups %>% mutate_at(vars(POSS:`OPP PITP`), funs(round(./GP, 2)))
    } else if (input$data == "Per 100 Poss") {
      dataset <- dataset %>%
        mutate(POSS_2 = POSS) %>%
        mutate_at(vars(POSS:`OPP PITP`), funs(round(100*./POSS_2, 2)))
      others_lineups <- others_lineups %>%
        mutate(POSS_2 = POSS) %>%
        mutate_at(vars(POSS:`OPP PITP`), funs(round(100*./POSS_2, 2)))
    } else if (input$data == "Per 48 Min") {
      dataset <- dataset %>% mutate_at(vars(POSS:`OPP PITP`), funs(round(48*./MIN, 2)))
      others_lineups <- others_lineups %>% mutate_at(vars(POSS:`OPP PITP`), funs(round(48*./MIN, 2)))
    }
    
    for (i in 1:nrow(dataset)) {
      table[i, 4] <- unname(unlist(dataset[i, tmStat]))
      table[i, 5] <- unname(unlist(others_lineups[i, tmStat]))
      table[i, 6] <- round(table[i, 4] - table[i, 5], 1)
      
      table[i, 7] <- unname(unlist(dataset[i, oppStat]))
      table[i, 8] <- unname(unlist(others_lineups[i, oppStat]))
      table[i, 9] <- round(table[i, 7] - table[i, 8], 1)
      
      table[i, 10] <- round(table[i, 6] - table[i, 9], 1)
    }
    
    if (input$stat %in% c('TOV', 'PF')) {
      table <- table %>%
        arrange(`Difference in Swing`)
    } else {
      table <- table %>%
        arrange(desc(`Difference in Swing`))
    }
    
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'LINEUP'),
          th(rowspan = 2, 'GP'),
          th(rowspan = 2, 'MIN'),
          th(colspan = 3, 'TEAM'),
          th(colspan = 3, 'OPPONENT')
        ),
        tr(
          lapply(c(rep(c('On-Court', 'Off-Court', 'Swing'), 2), 'Difference in Swing'), th)
        )
      )
    ))
    
    datatable(table, container = sketch, rownames = FALSE)
    
  })
  
}

shinyApp(ui, server)





















# dataset <- dataInput() %>% filter(LINEUPS %in% input$players)
# 
# if (input$data == "Totals") {
#   dataset <- dataset
# } else if (input$data == "Per Game") {
#   dataset <- dataset %>% mutate_at(vars(POSS:`OPP PITP`), funs(round(./GP, 2)))
# } else if (input$data == "Per 100 Poss") {
#   dataset <- dataset %>% 
#     mutate(POSS_2 = POSS) %>%
#     mutate_at(vars(POSS:`OPP PITP`), funs(round(100*./POSS_2, 2)))
# } else if (input$data == "Per 48 Min") {
#   dataset <- dataset %>% mutate_at(vars(POSS:`OPP PITP`), funs(round(48*./MIN, 2)))
# }
# 
# 
# for (i in 1:nrow(results)) {
#   results[i,1] <- dataset[dataset$RESULT=="Combined", which(names(dataset)==tm_stats[i])]
#   results[i,4] <- dataset[dataset$RESULT=="Combined", which(names(dataset)==opp_stats[i])]
#   results[i,7] <- results[i,1] - results[i,4]
#   
#   if (sum(dataset$RESULT=='W')>0) {
#     results[i,2] <- dataset[dataset$RESULT=="W", which(names(dataset)==tm_stats[i])]
#     results[i,5] <- dataset[dataset$RESULT=="W", which(names(dataset)==opp_stats[i])]
#     results[i,8] <- results[i,2] - results[i,5]
#   }
#   
#   if (sum(dataset$RESULT=='L')>0) {
#     results[i,3] <- dataset[dataset$RESULT=="L", which(names(dataset)==tm_stats[i])]
#     results[i,6] <- dataset[dataset$RESULT=="L", which(names(dataset)==opp_stats[i])]
#     results[i,9] <- results[i,3] - results[i,6]
#   }
#   
#   results[i,10] <- results[i,8] - results[i,9]
# }
# 
# 
# for (j in 1:ncol(results)) set(results, which(is.infinite(results[[j]])), j, NA)
# results["TOV", 10] <- results["TOV", 10] * -1
# results["PF", 10] <- results["PF", 10] * -1
# results <- results[order(-results$`Win-to-Loss Change`) , ]
# 
# formattable(results, align='c',
#             list(
#               `Win-to-Loss Change` = formatter("span", 
#                                     style = ~ style(color = ifelse(`Win-to-Loss Change` > 0, "green", "red")))))












