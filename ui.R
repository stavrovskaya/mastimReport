#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      h4("Выберите даты"),
      h5("Период 1"),
      # Application title
      dateInput("date1", "с:", value = Sys.Date()-15),
      
      # Default value is the date in client's time zone
      dateInput("date2", "по:", value = Sys.Date()-8),
      
      h5("Период 2"),
      
      # value is always yyyy-mm-dd, even if the display format is different
      dateInput("date3", "с:", value = Sys.Date()-7),
      
      # Pass in a Date object
      dateInput("date4", "по:"),
      
      textInput("ga_view_id", "Идентификатор предствления GA", "120758474"),
      
      textInput("ya_login", "Логин проекта яндекс", "biolatic-project"),
      #
      textInput("goals", "Номера целей GA", "6, 12, 13, 15, 16, 17, 3, 20"),
      
      actionButton("do", "загрузить отчет")
    ),
    mainPanel(
      h4("Сравнительный отчет по периодам"),
      h5(textOutput("period1")),
      h4("и"),
      h5(textOutput("period2")),
      h4(textOutput("warning"), style="color:red")
    )
  )
  
))
