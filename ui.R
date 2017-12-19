#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      h4("Выберите даты"),
      h5("Период 1"),
      fluidRow(
        
        column(6,
               
              
              dateInput("date1", "с:", value = Sys.Date()-15)
              
        ),
        
        
        
        column(6,
              
              dateInput("date2", "по:", value = Sys.Date()-8)
        )
      ),
      h5("Период 2"),
      fluidRow(
        
        column(6,
               
               dateInput("date3", "с:", value = Sys.Date()-7)
               
        ),
        
        
        
        column(6,
               
               dateInput("date4", "по:")
        )
      ),
      
      textInput("ga_view_id", "Идентификатор предствления GA", "120758474"),
      
      textInput("ya_login", "Логин проекта яндекс", "biolatic-project"),
      
      textInput("goals", "Номера целей GA", "6, 12, 13, 15, 16, 17, 3, 20"),
      
      actionButton("do", "загрузить отчет")
    ),
    mainPanel(
      h4("Сравнительный отчет по периодам"),
      fluidRow(
        
        column(6,
               
               h5(textOutput("period1"))
               
        ),
        
        
        
        column(6,
               
               h5(textOutput("period2"))
        )
      ),
     
      h4(textOutput("warning"), style="color:red"),
      tabsetPanel(
        id = 'dataset',
        tabPanel("report.ya.1", DT::dataTableOutput("yatable1")),
        tabPanel("report.ya.2", DT::dataTableOutput("yatable2")),
        tabPanel("report.ya.dif", DT::dataTableOutput("yatablediff")),
        tabPanel("report.google.1", DT::dataTableOutput("gtable1")),
        tabPanel("report.google.2", DT::dataTableOutput("gtable2")),
        tabPanel("report.google.dif", DT::dataTableOutput("gtablediff"))
        
      )
      
      
    )
  )
  
))
