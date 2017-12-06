#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)





# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  
  output$warning <- renderText({ 
    ""
  })
  output$period1 <- renderText({ 
    paste(input$date1, "-", input$date2)
  })
  output$period2 <- renderText({ 
    paste(input$date3, "-", input$date4)
  })
  observeEvent(input$do, {
    
    if (input$date1>input$date2 || input$date3>input$date4 ){
      output$warning<-renderText({"Ошибка: Начальная дата периода должна быть меньше конечной"})
    }
    else if(input$ga_view_id==""){
      output$warning<-renderText({"Ошибка: Введите идентификатор представления GA"})
    }
    else if(input$ya_login == ""){
      output$warning<-renderText({"Ошибка: Введите логин проекта яндекс"})
    }
    else{
      source("formReport.R")
      init(input$ga_view_id, input$ya_login, input$goals)
      form_reports(input$date1, input$date2, input$date3, input$date4)
      stopApp()
    }

  })
  observeEvent(input$date1, {
    output$warning <- renderText({ 
      ""
    })
  })
  observeEvent(input$date2, {
    output$warning <- renderText({ 
      ""
    })
  })
  observeEvent(input$date3, {
    output$warning <- renderText({ 
      ""
    })
  })
  observeEvent(input$date4, {
    output$warning <- renderText({ 
      ""
    })
  })
  observeEvent(input$ga_view_id, {
    output$warning <- renderText({ 
      ""
    })
  })
  observeEvent(input$ya_login, {
    output$warning <- renderText({ 
      ""
    })
  })
  session$onSessionEnded(stopApp)
  
})
