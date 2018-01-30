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
library(shinyjs)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  useShinyjs(),
  sidebarLayout(
    div( id ="Sidebar",
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
      
      fluidRow(
        
        column(6,
               
               
               textInput("ya_login", "Введите логин проекта яндекс", "")
               
        ),
        
        
        
        column(6,
               
               selectInput('ya_previous', 'или выберите из использованных ранее', c("..."))
        )
      ),
      
      
      selectInput('ga_account', 'Аккаунт GA', c("sz.mastim", "adv.binario")),
      
      textInput("ga_view_id", "Идентификатор предствления GA", ""),
      
      textInput("goals", "Номера целей GA (не должны дублировать транзакции)", ""),
      
      fluidRow(
        
        column(6,
               
               actionButton("do", "загрузить отчет")
               
        ),
        
        
        
        column(6,
               
               actionButton("test", "тестовый пример")
        )
      )
      
    )),
    mainPanel(
      fluidRow(
        
        column(6,
               
               actionButton("showSidebar", "показать меню"),
               actionButton("hideSidebar", "спрятать меню")
               
        ),
        
        
        
        column(6,
               
               disabled(actionButton("exportCSV", "Сохранить в csv"))
        )
      ),
     
     
      h4(textOutput("warning"), style="color:red"),
      tabsetPanel(
        id = 'dataset',
        tabPanel("Директ.1", DT::dataTableOutput("yatable1")),
        tabPanel("Директ.2", DT::dataTableOutput("yatable2")),
        tabPanel("Директ.dif", DT::dataTableOutput("yatablediff")),
        tabPanel("AdWords.1", DT::dataTableOutput("gtable1")),
        tabPanel("AdWords.2", DT::dataTableOutput("gtable2")),
        tabPanel("AdWords.dif", DT::dataTableOutput("gtablediff"))
        
      )
      
      
    )
  )
  
))
