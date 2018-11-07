################################# Server  ######################################
# Set up Environment -----------------------------------------------------------
suppressWarnings(library(plyr))
suppressWarnings(library(dplyr))
suppressWarnings(library(tidyr))
suppressWarnings(library(lubridate))
suppressWarnings(library(shiny))
suppressWarnings(library(readxl))


# Pull additional scripts to be used during process ----------------------------
suppressWarnings(source("scripts/tier1.r", local = T))
suppressWarnings(source("scripts/site_coordination.r", local = T))
suppressWarnings(source('scripts/services.r', local= T))
suppressWarnings(source('scripts/caselist.r', local = T))
suppressWarnings(source('scripts/progress_monitoring.r', local = T))
suppressWarnings(source('scripts/studentlist.r', local = T))
suppressWarnings(source('scripts/service_prep.r', local = T))
suppressWarnings(source('scripts/functions.r', local = T))


# Start server function --------------------------------------------------------

server <- function(input, output) {
  
# Input functions --------------------------------------------------------------
  
  getData_caselist <- reactive({
    

    caselist <- input$caselist
    
    if(is.null(caselist))
      return(NULL)
   
    nms <- names(read_excel(caselist$datapath, n_max = 0))
    
  
    ct <- ifelse(grepl("Date", nms), "date", "guess")
    
    caselist <- suppressWarnings(
      read_excel(caselist$datapath, sheet = 1,skip = 1, col_types = ct)
      )
      
    caselist <- caselist_script(caselist)
    return(caselist)

  })
  
  getData_progress <- reactive({
    
    progress <- input$progress
    
    if(is.null(progress))
      return(NULL)
    
    nms <- names(read_excel(progress$datapath, n_max = 0))
    
    
    ct <- ifelse(grepl("Date", nms), "date", "guess")
    
    progress <- suppressWarnings(
      read_excel(progress$datapath, sheet = 1,skip = 0, col_types = ct)
      )
    
    
    progress <- progress_import_script(progress)
    
    return(progress)
    
  })
  
  getData_services <- reactive({
    
    services <- input$services
    
    if(is.null(services))
      return(NULL)
    nms <- names(read_excel(services$datapath, n_max = 0))
    
    
    ct <- ifelse(grepl("Date", nms), "date", "guess")
    
    services <- suppressWarnings(
      read_excel(services$datapath, sheet = 1,skip = 1, col_types = ct)
      )
    
    services <- service_script(services)
    return(services)
    
  })
  
  
  getData_tier1 <- reactive({
    
    tier1 <- input$tier1

    if (is.null(input$tier1))
      return(NULL)
    
    nms <- names(read_excel(tier1$datapath, n_max = 0))
    
    ct <- ifelse(grepl("Date", nms), "date", "guess")
    
    tier1_df <- suppressWarnings(
      read_excel(tier1$datapath, sheet = 1,skip = 1, col_types = ct)
      )

    tier1_df <- tier1_script(tier1_df)
    
    return(tier1_df)
    
  })
  
  
  getData_site_coordination <- reactive({
    
    site_coordination <- input$site_coordination
  
    if (is.null(input$site_coordination))
      return(NULL)
    
    nms <- names(read_excel(site_coordination$datapath, n_max = 0))
    
    ct <- ifelse(grepl("Date", nms), "date", "guess")
    
    site_coordination_df <- suppressWarnings(
      read_excel(site_coordination$datapath, sheet = 1,skip = 0, col_types = ct)
      )
    
    site_coordination_df <- site_coordination_script(site_coordination_df)
    
    
    return(site_coordination_df)
    
  })
  
  getData_studentlist <- reactive({
    
    if(check_studentlist(input) == FALSE){
      return(NULL)
    }
    else{
  
      studentlist <- studentlist_check(
        getData_caselist(), getData_progress(), getData_services(),
        input$studentlist
      )
      
    }
     return(studentlist)
    
    
  })
  
  
# Run Student List Creation if Files Present -----------------------------------
  
  observeEvent(check_studentlist(input), {
  studentlist <<- getData_studentlist()
  })


# UI Display Functions ---------------------------------------------------------
  
  output$choose_school <- renderUI({
    validate(
      need(
        check_studentlist(input) == TRUE, studentlist_error_code)
        
    )
    
    caselist <- getData_caselist()
    services <- getData_services()
    
    
    schools <- school_options(
      caselist, studentlist, services
      )
    
      selectInput("school", "Select Schools", as.list(schools))
 
  }
  )
  
# Verify Input/Output Functions ----------------------------------------------------
  
  output$validate_inputs <- renderText({
    validate(
      need(check_studentlist(input) == TRUE, studentlist_error_code)
    )
  })
  
  output$validate_uploads <- renderText({
    
    validate(
    need(file_upload_check(input) != TRUE, incorrect_file_message)
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
      
      write.csv(studentlist, file, row.names = FALSE
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
    
    
    subsetted_df <- select(
      filter(studentlist, School == input$school),c(Student, Hours)
      )
    
    subsetted_df <- subsetted_df[order(subsetted_df$Hours, decreasing = T),]
    colnames(subsetted_df) <- c("Students", "Hours")
    
  
    head_tail(subsetted_df)
    
  })
  
  output$service_table <- renderTable({
    
    validate(
      need(input$services, services_error_code)
    )

    service_list <- getData_services()
    service_list$Support.Date <- as.Date(service_list$Support.Date)
    subseted_services <- select(
      filter(service_list, Home.School == input$school & between(Support.Date, today() - weeks(2), today())),
      c(Student.Support.Category, Student.Support.Name, hoursspent, Support.Date)
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

    
    select(filter(
      studentlist, School == input$school & error == TRUE),c(Student)
      )
  })
  
  output$missing_grades_table <- renderTable({

    validate(
      need(
        check_studentlist(input) == TRUE, studentlist_error_code)
      )
    
    missing_grades_display <- select(filter(
      studentlist, School == input$school & no_metrics == TRUE),
      c(Student, no_metrics_Q1, 
        no_metrics_Q2, no_metrics_Q3, no_metrics_Q4)
      )
    
    colnames(missing_grades_display) <- c(
      "Student", "Missing Q1", "Missing Q2", "Missing Q3", "Missing Q4"
      )
    missing_grades_display
  })
  
  output$eos_table <- renderTable({
    
    validate(
      need(input$services, services_error_code)
    )
    service_list <- getData_services()
    service_list$Support.Date <- as.Date(service_list$Support.Date)
    subseted_services <- select(
      filter(service_list, Home.School == input$school & between(Support.Date, today() - weeks(4), today())),
      c(Student.ID, Student.Support.Category, Student.Support.Name, hoursspent, Support.Date)
    )
    
    eos_rows <- c('Students Served', 'Hours Delivered', 'Parent Services')
    
    subseted_services$week <- floor_date(as.Date(subseted_services$Support.Date, "%m/%d/%Y"), unit="week")+1
    
    eos_columns <- unique(subseted_services$week)
    
    
    
    eos_table <- as.data.frame(matrix(,ncol = length(eos_columns), nrow = 3 ))
    rownames(eos_table) <- eos_rows
    colnames(eos_table) <- eos_columns
    
    
    for(i in seq(1,length(eos_columns))){
      
      eos_table[1,i] <- length(unique(subseted_services[subseted_services$week == eos_columns[i], "Student.ID"]))
      
      eos_table[2,i] <- sum(subseted_services[subseted_services$week == eos_columns[i], ]$hoursspent)
      
      eos_table[3,i] <- nrow(subseted_services[subseted_services$week == eos_columns[i] & subseted_services$Student.Support.Name == 
                                                 'Home Visit/Parent/Care Giver Contact' , ])
    }
    
    
    
    eos_table
    
    
    
  }, rownames = TRUE)
  
  
}


