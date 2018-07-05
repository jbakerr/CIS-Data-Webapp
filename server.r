source("tier1.r", local = T)
source("site_coordination.r", local = T)
source('services.r', local= T)
source('caselist.r', local = T)
source('progress_monitoring.r', local = T)
source('studentlist.r', local = T)
source('service_prep.r', local = T)
library(plyr) # ****IMPORTANT**** Load plyr before dplyr- they have some of the same named functions, and if you load in a different order it will cause problems
library(dplyr)
library(tidyr)
library(XLConnect)
library(lubridate)
library(shiny)


server <- function(input, output) {
  
  getData_caselist <- reactive({
    caselist <- input$caselist
    
    if(is.null(caselist))
      return(NULL)
    
    caselist <- readWorksheetFromFile(caselist$datapath, sheet = 1, header = T, startRow = 2)
    caselist <- caselist_script(caselist)
    return(caselist)

  })
  
  
  getData_progress <- reactive({
    
    progress <- input$progress
    
    if(is.null(progress))
      return(NULL)
    
    
    progress <-readWorksheetFromFile(progress$datapath, sheet = 1, header = T, startRow = 1)
    progress <- progress_import_script(progress)
    
    return(progress)
    
    
  })
  
  getData_services <- reactive({
    
    services <- input$services
    
    if(is.null(services))
      return(NULL)
    
    services <- readWorksheetFromFile(services$datapath, sheet = 1, header = T, startRow = 2)
    
    services <- service_script(services)
    return(services)
    
    
  })
  
  
  
  
  getData_tier1 <- reactive({
    
    tier1 <- input$tier1

    if (is.null(input$tier1))
      return(NULL)

    
    tier1_df <- readWorksheetFromFile(tier1$datapath, sheet=1, header = T, startRow = 2)
    return(tier1_df)
    
    tier1_df <- tier1_script(tier1_df)
    
  
    
  })
  
  
  getData_site_coordination <- reactive({
    
    site_coordination <- input$site_coordination
  
    if (is.null(input$site_coordination))
      return(NULL)
    

    
    site_coordination_df <- readWorksheetFromFile(site_coordination$datapath, sheet=1, header = T, startRow = 1)
    site_coordination_df <- site_coordination_script(site_coordination_df)
    
    
    
    return(site_coordination_df)
    
  })
  
  studentlist_creation <- function(caselist, progress, data){
    
    studentlist <-merge(caselist, progress, by = "Student.ID", all = T)

    stserv <- prep_service_file(data)
    
    stlist <- merge(studentlist, stserv, by = "Student.ID", all = T)
    
    studentlist <- studentlist_script(stlist)
    
    
    return(studentlist)
    
  }
  
  
  output$contents <- renderTable(
    
    getData_tier1()
    
  )
  
  output$download_tier1 <- downloadHandler(
    filename = function() {
      paste("tier1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(getData_tier1(), file, row.names = FALSE)
    }
  )
  
  output$download_site_coordination <- downloadHandler(
    filename = function() {
      paste("site_coordination", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(getData_site_coordination(), file, row.names = FALSE)
    }
  )
  
  output$download_services <- downloadHandler(
    filename = function() {
      paste("services", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(getData_services(), file, row.names = FALSE)
    }
  )
  
  output$download_studentlist <- downloadHandler(
    filename = function() {
      paste("studentlist", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(studentlist_creation(getData_caselist(), getData_progress(), getData_services()), file, row.names = FALSE)
    }
  )
  
  
}