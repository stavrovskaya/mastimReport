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
      progress <- shiny::Progress$new()
      progress$set(message = "Computing data", value = 0)
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())
      
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      
      init(input$ga_view_id, input$ya_login, input$goals)
      report <-form_reports(input$date1, input$date2, input$date3, input$date4, updateProgress)
      
      output$yatable1 = DT::renderDataTable({
        report[[1]]
      })
      output$yatable2 = DT::renderDataTable({
        report[[2]]
      })
      output$yatablediff = DT::renderDataTable({
        brks<-c(-50, -20, 0, 20, 50)
        clrs<-c("#ff0000", "#ff6666", "#ffcccc", "#ebfaeb","#99e699", "#33cc33")
        datatable(report[[3]]) %>% formatStyle(names(report[[3]])[c(-1,-2, -3)], backgroundColor = styleInterval(brks, clrs))
      })
      output$gtable1 = DT::renderDataTable({
        report[[4]]
      })
      output$gtable2 = DT::renderDataTable({
        report[[5]]
      })
      output$gtablediff = DT::renderDataTable({
        brks<-c(-50, -20, 0, 20, 50)
        clrs<-c("#ff0000", "#ff6666", "#ffcccc", "#ebfaeb","#99e699", "#33cc33")
        datatable(report[[6]]) %>% formatStyle(names(report[[6]])[c(-1,-2, -3)], backgroundColor = styleInterval(brks, clrs))
      })
      
      
      #stopApp()
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
