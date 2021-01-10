library(readxl)
library(DT)
library(plotly)
library(shiny)
library(shinythemes)
library(rstudioapi)


current_path = getActiveDocumentContext()$path 
setwd(dirname(current_path))

CT <- read_excel("data/CT_2016Q4_Exam.xlsx")
MR <- read_excel("data/MR_2016Q4_Exam.xlsx")
US <- read_excel("data/US_2016Q4_Exam.xlsx")


days = c("Monday","Tuesday", "Wednesday", "Thursday","Friday","Saturday","Sunday")
times = c("12:00 AM - 1:00 AM", "1:00 AM - 2:00 AM", "2:00 AM - 3:00 AM", "3:00 AM - 4:00 AM", "4:00 AM - 5:00 AM",
          "5:00 AM - 6:00 AM", "6:00 AM - 7:00 AM", "7:00 AM - 8:00 AM", "8:00 AM - 9:00 AM", "9:00 AM - 10:00 AM",
          "10:00 AM - 11:00 AM", "11:00 AM - 12:00 PM", "12:00 PM - 1:00 PM", "1:00 PM - 2:00 PM", "2:00 PM - 3:00 PM",
          "3:00 PM - 4:00 PM", "4:00 PM - 5:00 PM", "5:00 PM - 6:00 PM", "6:00 PM - 7:00 PM", "7:00 PM - 8:00 PM",
          "8:00 PM - 9:00 PM", "9:00 PM - 10:00 PM", "10:00 PM - 11:00 PM", "11:00 PM - 12:00 AM")
shifts = c("Shift1","Shift2","Shift3")

all.levels.by.hour <- data.frame(days = factor(
  rep(days,24),
  levels = days))

all.levels.by.shift <- data.frame(days = factor(
  rep(days,3),
  levels = days))

all.levels.by.hour <- data.frame(all.levels.by.hour[order(all.levels.by.hour$days), ])
all.levels.by.shift <- data.frame(all.levels.by.shift[order(all.levels.by.shift$days), ])
all.levels.by.hour$`Hour Period` <- factor(rep(times,7),levels=times)
all.levels.by.shift$`Shift Period` <- factor(rep(shifts,7),levels=shifts)
names(all.levels.by.shift)[1] <- names(all.levels.by.hour)[1] <- "Day of the Week"

ui <- fluidPage(theme = shinytheme("cosmo"),
  
  titlePanel("Patient Volume Visualization"),
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Understanding trends in historical patient volume data to improve staff management", br()),
      # actionButton("run", "Generate Results",
      #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      br(),
      
      radioButtons(inputId = "ptype", label = "How to view the data:", choices = c("By Shift", "By Hour")),
      
      radioButtons(inputId = "mode", label = "Imaging modality:", choices = c("CT","MR","US"), inline = TRUE),
      
      br(),
      
      selectInput(inputId = "month", label = "Month",
                  choices = c("","October","November","December"),
                  width = '50%'),
      
      br(),
      
      selectInput(inputId = "week", label = "Week",
                  choices = c("",1,2,3,4,5),
                  width = '50%'),
      
      br(),
      
      selectInput(inputId = "day", label = "Day (Optional)",
                  choices = c("","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
                  width = '50%')
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", icon = icon("table"),
                 DT::dataTableOutput("tbl")
                 ),
        tabPanel("Volume for the Week", icon = icon("bar-chart-o"),
                 plotlyOutput("plot1")
                 ),
        tabPanel("Volume for the Day", icon = icon("bar-chart-o"),
                 plotlyOutput("plot2")
                 )

        )

      )
    )
  )


server <- function(input, output, session) {
  
  output$tbl <- DT::renderDataTable({
    
    if (input$ptype == "By Shift") {  
      df <- get(input$mode) %>%
        filter(Month == input$month, Week == input$week) %>%
        group_by(`Day of the Week`, `Shift Period`) %>%
        summarise(Patients=n())
      
      df <- merge(all.levels.by.shift, df, by=names(all.levels.by.shift), sort = FALSE, all.x = TRUE)
      df <- df[order(df$`Day of the Week`, df$`Shift Period`), ]
      df[is.na(df)] <- 0
      
      df$popularity = df$Patients/max(df$Patients)
      
      dt <- aggregate(Patients ~ `Shift Period`, data = df, sum)
      dt$`Relative %` <- round((dt$Patients/sum(dt$Patients))*100, 1)
      
      DT::datatable(dt)
      
    } else if (input$ptype == "By Hour") {  
      df <- get(input$mode) %>%
        filter(Month == input$month, Week == input$week) %>%
        group_by(`Day of the Week`, `Hour Period`) %>%
        summarise(Patients=n())
      
      df <- merge(all.levels.by.hour, df, by=names(all.levels.by.hour), sort = FALSE, all.x = TRUE)
      df <- df[order(df$`Day of the Week`, df$`Hour Period`), ]
      df[is.na(df)] <- 0
      
      df$popularity = df$Patients/max(df$Patients)
      
      dt <- aggregate(Patients ~ `Hour Period`, data = df, sum)
      dt$`Relative %` <- round((dt$Patients/sum(dt$Patients))*100, 1)
      
      DT::datatable(dt)
    }
    
  })
  
  output$plot1 <- renderPlotly({
    
    if (input$ptype == "By Shift") {  
      df <- get(input$mode) %>%
        filter(Month == input$month, Week == input$week) %>%
        group_by(`Day of the Week`, `Shift Period`) %>%
        summarise(Patients=n())
      
      df <- merge(all.levels.by.shift, df, by=names(all.levels.by.shift), sort = FALSE, all.x = TRUE)
      df <- df[order(df$`Day of the Week`, df$`Shift Period`), ]
      df[is.na(df)] <- 0
      
      df$popularity = df$Patients/max(df$Patients)
      
      p <- plot_ly(
        x = df$`Day of the Week`, y = df$`Shift Period`, colors = colorRamp(c("white", "blue")),
        z = df$popularity, type = "heatmap", showscale = FALSE,
        hoverinfo = 'text',
        text = ~paste(df$`Day of the Week`, '</br>', '</br>', df$`Shift Period`,
                      '</br>', df$Patients,'Patient(s)')
      ) %>%
        layout(xaxis = list(showgrid = F),
               yaxis = list(showgrid = F))
      
      p
      
    } else if (input$ptype == "By Hour") {  
      df <- get(input$mode) %>%
        filter(Month == input$month, Week == input$week) %>%
        group_by(`Day of the Week`, `Hour Period`) %>%
        summarise(Patients=n())
      
      df <- merge(all.levels.by.hour, df, by=names(all.levels.by.hour), sort = FALSE, all.x = TRUE)
      df <- df[order(df$`Day of the Week`, df$`Hour Period`), ]
      df[is.na(df)] <- 0
      
      df$popularity = df$Patients/max(df$Patients)
      
      p <- plot_ly(
        x = df$`Day of the Week`, y = df$`Hour Period`, colors = colorRamp(c("white", "blue")),
        z = df$popularity, type = "heatmap", showscale = FALSE,
        hoverinfo = 'text',
        text = ~paste(df$`Day of the Week`, '</br>', '</br>', df$`Hour Period`,
                      '</br>', df$Patients,'Patient(s)')
      ) %>%
        layout(xaxis = list(showgrid = F),
               yaxis = list(showgrid = F))
      
      p
    }
    
  })
  
  output$plot2 <- renderPlotly({
    
    if (input$ptype == "By Shift") {  
      df <- get(input$mode) %>%
        filter(Month == input$month, Week == input$week) %>%
        group_by(`Day of the Week`, `Shift Period`) %>%
        summarise(Patients=n())
      
      df <- merge(all.levels.by.shift, df, by=names(all.levels.by.shift), sort = FALSE, all.x = TRUE)
      df <- df[order(df$`Day of the Week`, df$`Shift Period`), ]
      df[is.na(df)] <- 0
      
      df$popularity = df$Patients/max(df$Patients)
      
      q <- ggplot(data=subset(df,`Day of the Week`==input$day), aes(x=`Shift Period`, y=Patients, group=1)) +
        geom_line(color="blue") +
        geom_point(fill="red", colour="black",pch=21, size=1.5) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("Number of Patients") + xlab("")
      q <- ggplotly(q, tooltip = c("Patients"))
      
      q
      
    } else if (input$ptype == "By Hour") {  
      df <- get(input$mode) %>%
        filter(Month == input$month, Week == input$week) %>%
        group_by(`Day of the Week`, `Hour Period`) %>%
        summarise(Patients=n())
      
      df <- merge(all.levels.by.hour, df, by=names(all.levels.by.hour), sort = FALSE, all.x = TRUE)
      df <- df[order(df$`Day of the Week`, df$`Hour Period`), ]
      df[is.na(df)] <- 0
      
      df$popularity = df$Patients/max(df$Patients)
      
      q <- ggplot(data=subset(df,`Day of the Week`==input$day), aes(x=`Hour Period`, y=Patients, group=1)) +
        geom_line(color="blue") +
        geom_point(fill="red", colour="black",pch=21, size=1.5) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("Number of Patients") + xlab("")
      q <- ggplotly(q, tooltip = c("Patients"))
      
      q
    }
    
  })

  

}


shinyApp(ui, server)













