########################### Set up environment #################################
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(shiny)
library(openxlsx)

# Pull additional scripts to be used during process ----------------------------
suppressWarnings(source("tier1.r", local = T))
suppressWarnings(source("site_coordination.r", local = T))
suppressWarnings(source('services.r', local= T))
suppressWarnings(source('caselist.r', local = T))
suppressWarnings(source('progress_monitoring.r', local = T))
suppressWarnings(source('studentlist.r', local = T))
suppressWarnings(source('service_prep.r', local = T))
suppressWarnings(source('functions.r', local = T))

# Start server function --------------------------------------------------------

server <- function(input, output) {
  
# Input functions --------------------------------------------------------------
  
  getData_caselist <- reactive({
    

    caselist <- input$caselist
    
    if(is.null(caselist))
      return(NULL)
   
    caselist <- read.xlsx(
      caselist$datapath, sheet = 1, colNames = T, startRow = 2, detectDates = T
      )
    
    caselist <- caselist_script(caselist)
    return(caselist)

  })
  
  
  getData_progress <- reactive({
    
    progress <- input$progress
    
    if(is.null(progress))
      return(NULL)
    
    progress <- read.xlsx(
      progress$datapath, sheet = 1, colNames = T, startRow = 1, detectDates = T
      )
    
    progress <- progress_import_script(progress)
    
    return(progress)
    
    
  })
  
  getData_services <- reactive({
    
    services <- input$services
    
    if(is.null(services))
      return(NULL)
    
    services <- read.xlsx(
      services$datapath, sheet = 1, colNames = T, startRow = 2
      )

    
    services <- service_script(services)
    return(services)
    
    
  })
  

  
  getData_tier1 <- reactive({
    
    tier1 <- input$tier1

    if (is.null(input$tier1))
      return(NULL)

    tier1_df <- read.xlsx(tier1$datapath, sheet = 1, colNames = T, startRow = 2)
    
    
    tier1_df <- tier1_script(tier1_df)
    
    return(tier1_df)
    
  })
  
  
  getData_site_coordination <- reactive({
    
    site_coordination <- input$site_coordination
  
    if (is.null(input$site_coordination))
      return(NULL)
    

    site_coordination_df <- read.xlsx(
      site_coordination$datapath, sheet = 1, colNames = T, 
      startRow = 1, detectDates = T
      )
    site_coordination_df <- site_coordination_script(site_coordination_df)
    
    
    
    return(site_coordination_df)
    
  })
  
  getData_studentlist <- reactive({
    
    studentlist <- input$studentlist
    
    if (is.null(input$studentlist))
      return(NULL)
    studentlist <- read.csv(studentlist$datapath, header = T)
    
    return(studentlist)
    
  })
  
# UI Display Functions ---------------------------------------------------------
  
  output$choose_school <- renderUI({
    validate(
      need(
        check_studentlist(input) == TRUE, studentlist_error_code)
        
    )
    
    caselist <- getData_caselist()
    services <- getData_services()
    studentlist <- getData_studentlist()
    
    schools <- school_options(
      caselist, studentlist, services
      )
    
      selectInput("school", "Select Schools", as.list(schools))
 
    
  }
  )
# Download Output Functions ----------------------------------------------------
  
  output$validate_inputs <- renderText({
    validate(
      need(check_studentlist(input) == TRUE, studentlist_error_code)
    )
    
    
  })
  
  
  
# Download Output Functions ----------------------------------------------------
  
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
      req(input$services, input$caselist, input$progress)
      write.csv(studentlist_creation(
        getData_caselist(), getData_progress(), 
        getData_services()), file, row.names = FALSE
        )
    }
  )
  
# Report Output Functions ------------------------------------------------------  
  
  output$student_table <- renderTable({
    
    validate(
      need(
        check_studentlist(input) == TRUE, studentlist_error_code)
    )
    
    if(is.null(input$school)){
      return(NULL)
    }
    

    
    studentlist <- studentlist_check(
      getData_caselist(), getData_progress(), getData_services(),
      getData_studentlist()
      )
    
    subsetted_df <- select(
      filter(studentlist, School == input$school),c(Student, Hours)
      )
    
    subsetted_df <- subsetted_df[order(subsetted_df$Hours, decreasing = T),]
    
  
    head_tail(subsetted_df)
    
    
  })
  
  output$service_table <- renderTable({
    
    validate(
      need(input$services, services_error_code)
    )

    service_list <- getData_services()
    subseted_services <- select(
      filter(service_list, Home.School == input$school),
      c(Student.Support.Category, Student.Support.Name, hoursspent)
      )
    service_table <- aggregate(
      subseted_services$hoursspent, 
      by=list(Category=subseted_services$Student.Support.Name), FUN=sum
      )
    
    service_table
    
  })
  
  output$setup_table <- renderTable({

     validate(
      need(
        check_studentlist(input) == TRUE, studentlist_error_code)
      )

    studentlist <- studentlist_check(
      getData_caselist(), getData_progress(), getData_services(), 
      getData_studentlist()
      )
    
    select(filter(
      studentlist, School == input$school & error == TRUE),c(Student)
      )
    
    
  })
  
  output$missing_grades_table <- renderTable({

    validate(
      need(
        check_studentlist(input) == TRUE, studentlist_error_code)
      )
    
    studentlist <- studentlist_check(
      getData_caselist(), getData_progress(), getData_services(), 
      getData_studentlist()
      )
    select(filter(
      studentlist, School == input$school & no_metrics == TRUE),
      c(Student, no_metrics, no_metrics_Q1, 
        no_metrics_Q2, no_metrics_Q3, no_metrics_Q4)
      )
    
    
  })
  

  
  
}


