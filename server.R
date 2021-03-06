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
library(stringr)



report<-NULL
prev_params<-NULL
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  observe({
    prev_params<<-read.csv("prev_params.csv", stringsAsFactors = F)
    updateSelectInput(session, "ya_previous", choices = c("...",prev_params$ya_login))
  })
  
  observeEvent(input$showSidebar, {
    shinyjs::show(id = "Sidebar")
  })
  observeEvent(input$hideSidebar, {
    shinyjs::hide(id = "Sidebar")
  })
  
  output$warning <- renderText({ 
    ""
  })
  output$period1 <- renderText({ 
    paste(input$date1, "-", input$date2)
  })
  output$period2 <- renderText({ 
    paste(input$date3, "-", input$date4)
  })
  
  observeEvent(input$test, {
    updateSelectInput(session, "ya_previous", selected="...")
    
    updateTextInput(session, "ya_login", value="obruchalki-direct")
    updateSelectInput(session, "ya_account", selected="stbinario")
    updateSelectInput(session, "ga_account", selected="sz.mastim")
    updateTextInput(session, "ga_view_id", value="172465481")
    updateTextInput(session, "goals", value="1, 10")
    
  })  

  observeEvent(input$ya_previous, {
    if (input$ya_previous=="..."){
      updateTextInput(session, "ya_login", value="")
      updateSelectInput(session, "ya_account", selected="stbinario")
      updateSelectInput(session, "ga_account", selected="sz.mastim")
      updateTextInput(session, "ga_view_id", value="")
      updateTextInput(session, "goals", value="")
      
    }else{
      a<-prev_params[prev_params$ya_login == input$ya_previous, ]
      updateTextInput(session, "ya_login", value=a$ya_login)
      updateSelectInput(session, "ya_account", selected=a$ya_account)
      updateSelectInput(session, "ga_account", selected=a$ga_account)
      updateTextInput(session, "ga_view_id", value=a$ga_view_id)
      updateTextInput(session, "goals", value=a$goals)
    }
  })  
  
  observeEvent(input$do, {
    
    if (input$date1>input$date2 || input$date3>input$date4 ){
      
      output$warning<-renderText({"Ошибка: Начальная дата периода должна быть меньше конечной"})
      
    }
    
    else if(input$ga_view_id==""){
      
      output$warning<-renderText({"Ошибка: Введите идентификатор представления GA"})
      
    }
    else if(grepl("[^0-9]", input$ga_view_id) ){
      
      output$warning<-renderText({"Ошибка: идентификатор представления GA должен состоять только из цифр"})
      
    }   
    else if(input$ya_login == ""){
      
      output$warning<-renderText({"Ошибка: Введите логин проекта яндекс"})
      
    }
    else{
      goals_ga_numbers<<-unlist(strsplit(input$goals, ","))
      goals_ga_numbers<<-str_trim(goals_ga_numbers)
      print(input$goals)
      print(goals_ga_numbers)
      
      if (length(goals_ga_numbers)==0){
        goals_ga_numbers<-c()
      }
      
      if (any(grepl("[^0-9]", goals_ga_numbers))){
        output$warning<-renderText({"Ошибка: Идентификаторы целей должны состоять только из цифр, перечислите идентицикаторы целей в формате 1,2,3"})
      }else if (any(as.numeric(goals_ga_numbers)>20)||any(as.numeric(goals_ga_numbers)<=0)){
        output$warning<-renderText({"Ошибка: Идентификаторы целей могут быть только от 1 до 20"})
      }else{
        #save params
        params<-data.frame(ya_login=input$ya_login, ya_account=input$ya_account,  ga_account=input$ga_account, 
                           ga_view_id=input$ga_view_id, goals=input$goals, stringsAsFactors = F)
        print(params)
        if (input$ya_login %in% prev_params$ya_login){
          print("input$ya_login %in% prev_params$ya_login")
          prev_params[prev_params$ya_login==input$ya_login,]<-params
            
        }else{
          print("input$ya_login NOT %in% prev_params$ya_login")
          prev_params<-rbind(prev_params, params)
        }
        print(prev_params)
        write.csv(prev_params, "prev_params.csv", row.names = F)
        
        source("formPlacesReport.R")
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
        
        init(input$ga_view_id, input$ya_login, goals_ga_numbers, google_account=input$ga_account, yandex_account=input$ya_account)
        if (input$report_type=="keywords"){
          report <<-form_reports(input$date1, input$date2, input$date3, input$date4, updateProgress)
          string_colums<-c(1,2,3)
        }
        else{
          report <<-form_reports_places(input$date1, input$date2, input$date3, input$date4, updateProgress)
          string_colums<-c(1,2)
        }
        
        
        output$yatable1 = DT::renderDataTable({
  
          DT::datatable(report[[1]], options = list(
            lengthMenu = list(c(5, 10, 20, -1), c('5', '10', '20','All')),
            pageLength = 10), selection = 'none'
          )
        })
        output$yatable2 = DT::renderDataTable({
          DT::datatable(report[[2]], options = list(
            lengthMenu = list(c(5, 10, 20, -1), c('5', '10', '20','All')),
            pageLength = 10), selection = 'none'
          )
        })
        output$yatablediff = DT::renderDataTable({
          brks<-c(-50, -20, 0, 20, 50)
          clrs<-c("#ff0000", "#ff6666", "#ffcccc", "#ebfaeb","#99e699", "#33cc33")
          DT::datatable(report[[3]], options = list(
            lengthMenu = list(c(5, 10, 20, -1), c('5', '10', '20','All')),
            pageLength = 10), selection = 'none') %>% formatStyle(names(report[[3]])[-string_colums], backgroundColor = styleInterval(brks, clrs))
        })
        output$gtable1 = DT::renderDataTable({
          DT::datatable(report[[4]], options = list(
            lengthMenu = list(c(5, 10, 20, -1), c('5', '10', '20','All')),
            pageLength = 10), selection = 'none'
          )
        })
        output$gtable2 = DT::renderDataTable({
          DT::datatable(report[[5]], options = list(
            lengthMenu = list(c(5, 10, 20, -1), c('5', '10', '20','All')),
            pageLength = 10), selection = 'none'
          )
        })
        output$gtablediff = DT::renderDataTable({
          brks<-c(-50, -20, 0, 20, 50)
          clrs<-c("#ff0000", "#ff6666", "#ffcccc", "#ebfaeb","#99e699", "#33cc33")
          DT::datatable(report[[6]], options = list(
            lengthMenu = list(c(5, 10, 20, -1), c('5', '10', '20','All')),
            pageLength = 10), selection = 'none') %>% formatStyle(names(report[[6]])[-string_colums], backgroundColor = styleInterval(brks, clrs))
        })
        enable("exportCSV")
      }
      
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
  observeEvent(input$ga_account, {
    output$warning <- renderText({ 
      ""
    })
  })
  observeEvent(input$goals, {
    output$warning <- renderText({ 
      ""
    })
  })
  
  observeEvent(input$exportCSV, {
    outpath<-sub("/Shiny/Context", "/Result/Context", getwd())
    if (!is.null(report)){
      if (!file.exists(outpath)){
        dir.create(outpath)
      }
      disable("do")
      disable("exportCSV")
      

      write.csv2(report[[1]], paste(outpath, paste("direct", input$date1, input$date2, input$ya_login, ".csv", sep="_"), sep="/"), row.names = F, quote = F)
      write.csv2(report[[2]], paste(outpath, paste("direct", input$date3, input$date4, input$ya_login, ".csv", sep="_"), sep="/"), row.names = F, quote = F)
      write.csv2(report[[3]], paste(outpath, paste("direct", input$date1, input$date2, "-", input$date3, input$date4, input$ya_login, ".csv", sep="_"), sep="/"), row.names = F, quote = F)
      
      write.csv2(report[[4]], paste(outpath, paste("adwords", input$date1, input$date2, input$ya_login, ".csv", sep="_"), sep="/"), row.names = F, quote = F)
      write.csv2(report[[5]], paste(outpath, paste("adwords", input$date3, input$date4, input$ya_login, ".csv", sep="_"), sep="/"), row.names = F, quote = F)
      write.csv2(report[[6]], paste(outpath, paste("adwords", input$date1, input$date2, "-", input$date3, input$date4, input$ya_login, ".csv", sep="_"), sep="/"), row.names = F, quote = F)
      
      enable("do")
      enable("exportCSV")
      }
    
  })
  
  session$onSessionEnded(stopApp)
  
})
