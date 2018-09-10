################################# UI  ##########################################

# Set up Environment -----------------------------------------------------------
library(markdown)
library(plyr) 
library(dplyr)
library(lubridate)
library(shiny)
library(rsconnect)
library(readxl)

# Pull additional scripts to be used during process ----------------------------
source('instructions.r', local = T)


# Start ui function ------------------------------------------------------------

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
                    .shiny-output-error-validation {
                    color: red;
                    }
                    "))
    ),
  
  fluidPage(
    img(src = "logo.png", height = 102, width = 102),
    titlePanel("Communities In Schools of Durham Data Process Manager"),
    navbarPage("Data Manager",
# Start Page -------------------------------------------------------------------
               
    tabPanel("Start", 
# Start Page Side Bar UI--------------------------------------------------------
      sidebarLayout(
        sidebarPanel(
          fileInput('services', 'Upload Student Support Detail File',
                    accept=c('xlsx', 
                             'text/comma-separated-values,text/plain', 
                             '.xlsx')),
          fileInput('caselist', 'Upload Caselist Detail File',
                    accept=c('xlsx', 
                             'text/comma-separated-values,text/plain', 
                             '.xlsx')),
          fileInput('progress', 'Upload Student Metric Data File',
                    accept=c('xlsx', 
                             'text/comma-separated-values,text/plain', 
                             '.xlsx')),
          fileInput('site_coordination', 'Upload Site Coordination Entries File',
                    accept=c('xlsx', 
                             'text/comma-separated-values,text/plain', 
                             '.xlsx')),
          
          fileInput('tier1', 'Upload Tier 1 Support File',
                    accept=c('xlsx', 
                             'text/comma-separated-values,text/plain', 
                             '.xlsx')),
          fileInput('studentlist', 'Upload Generated Student List File',
                    accept=c('xlsx', 
                             'text/comma-separated-values,text/plain', 
                             '.xlsx')),
          downloadButton('download_tier1', 'Download Tier 1 Data'),
          br(),
          downloadButton(
            'download_site_coordination', 'Download Site Coordination'
            ),
          br(),
          downloadButton('download_services', 'Download Service File'),
          br(),
          downloadButton('download_studentlist', 'Download Studentlist File')
          
        ),
# Start Page Main Panel UI------------------------------------------------------
        mainPanel(
          
         includeMarkdown("md/instructions.md"),
         textOutput("validate_inputs"),
         br(),
         textOutput('validate_uploads')
        )
      )
      
  
      ),
# Reports Page -----------------------------------------------------------------
    tabPanel("Reports", 
# Reports Side Bar UI-----------------------------------------------------------
             sidebarLayout(
               sidebarPanel(
                 uiOutput("choose_school")
               ),
# Reports Main Panel UI---------------------------------------------------------
               mainPanel(
# Reports Main Panel Tabs UI----------------------------------------------------
                 tabsetPanel(type = 'tabs',
                             tabPanel(
                               'Students', 
                               includeMarkdown("md/student_explanation.md"), 
                               tableOutput('student_table')),
                             tabPanel(
                               'Services',
                               includeMarkdown("md/service_explanation.md"),
                               tableOutput('service_table')),
                             tabPanel(
                               'Set Up', 
                               includeMarkdown("md/setup_explanation.md"),
                               tableOutput('setup_table')),
                             tabPanel(
                               'EOS Check',
                               includeMarkdown('md/eos_explanation.md'),
                               tableOutput('eos_table')
                             )
                             # tabPanel(
                             #   'Missing Grades', 
                             #   includeMarkdown("md/metric_explanation.md"), 
                             #   tableOutput('missing_grades_table'))
                             
               )
             )
             
    )
    )
  )
)
)