source("tier1.r", local = T)
source("site_coordination.r", local = T)
source('services.r', local= T)
source('caselist.r', local = T)
source('progress_monitoring.r', local = T)
source('studentlist.r', local = T)
source('service_prep.r', local = T)
source('functions.r', local = T)
library(plyr) # ****IMPORTANT**** Load plyr before dplyr- they have some of the same named functions, and if you load in a different order it will cause problems
library(dplyr)
library(tidyr)
library(lubridate)
library(shiny)
library(openxlsx)


server <- function(input, output) {
  
  getData_caselist <- reactive({
    caselist <- input$caselist
    
    if(is.null(caselist))
      return(NULL)
   
    caselist <- read.xlsx(caselist$datapath, sheet = 1, colNames = T, startRow = 2, detectDates = T)
    caselist <- caselist_script(caselist)
    return(caselist)

  })
  
  
  getData_progress <- reactive({
    
    progress <- input$progress
    
    if(is.null(progress))
      return(NULL)
    
    progress <- read.xlsx(progress$datapath, sheet = 1, colNames = T, startRow = 1, detectDates = T)
    
    progress <- progress_import_script(progress)
    
    return(progress)
    
    
  })
  
  getData_services <- reactive({
    
    services <- input$services
    
    if(is.null(services))
      return(NULL)
    
    services <- read.xlsx(services$datapath, sheet = 1, colNames = T, startRow = 2)

    
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
    

    site_coordination_df <- read.xlsx(site_coordination$datapath, sheet = 1, colNames = T, startRow = 1, detectDates = T)
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
  
  
  studentlist_creation <- function(caselist, progress, data){
    
    req(input$services, input$caselist, input$progress)
    
    studentlist <-merge(caselist, progress, by = "Student.ID", all = T)

    stserv <- prep_service_file(data)
    
    stlist <- merge(studentlist, stserv, by = "Student.ID", all = T)
    
    studentlist <- studentlist_script(stlist)
    
    
    return(studentlist)
    
  }
  
  studentlist_check <- function(caselist, progress, data){
    if(!is.null(getData_studentlist())){
      studentlist <- getData_studentlist()
      return(studentlist)
    }
    else if(!is.null(studentlist_creation(caselist, progress, data))){
      studentlist <- studentlist_creation(caselist, progress, data)
      return(studentlist)
    }
    else{
      return(FALSE)}
  }
    
    

  
  # output$contents <- renderTable(
  #   
  #   getData_tier1()
    
  # )
  
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
  
  output$student_table <- renderTable({
    studentlist <- studentlist_check(getData_caselist(), getData_progress(), getData_services())
    subsetted_df <- select(filter(studentlist, School == input$school),c(Student, Hours))
    subsetted_df <- subsetted_df[order(subsetted_df$Hours, decreasing = T),]
    head_tail(subsetted_df)
    
    
    
    
  })
  
  output$service_table <- renderTable({
    service_list <- getData_services()
    subseted_services <- select(filter(service_list, Home.School == input$school),c(Student.Support.Category, Student.Support.Name, hoursspent))
    aggregate(subseted_services$hoursspent, by=list(Category=subseted_services$Student.Support.Name), FUN=sum)
    
    
    
    
    
  })
  
  output$setup_table <- renderTable({
    studentlist <- studentlist_check(getData_caselist(), getData_progress(), getData_services())
    
    select(filter(studentlist, School == input$school & error == TRUE),c(Student))
    
    
  })
  
  output$missing_grades_table <- renderTable({
    studentlist <- studentlist_check(getData_caselist(), getData_progress(), getData_services())
    select(filter(studentlist, School == input$school & no_metrics == TRUE),c(Student, no_metrics, no_metrics_Q1, no_metrics_Q2, no_metrics_Q3, no_metrics_Q4))
    
    
  })
}