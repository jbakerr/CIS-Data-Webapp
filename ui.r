source('instructions.r', local = T)
library(markdown)


ui <- fluidPage(
  fluidPage(
    img(src = "logo.png", height = 102, width = 102),
    titlePanel("Communities In Schools of Durham Data Process Manager"),
    navbarPage("Data Manager",
    tabPanel("Start", 
      sidebarLayout(
        sidebarPanel(
          fileInput('tier1', 'Upload Tier 1 File',
                    accept=c('xlsx', 
                             'text/comma-separated-values,text/plain', 
                             '.xlsx')),
          fileInput('site_coordination', 'Upload Site Coordination File',
                    accept=c('xlsx', 
                             'text/comma-separated-values,text/plain', 
                             '.xlsx')),
          fileInput('services', 'Upload Services File',
                    accept=c('xlsx', 
                             'text/comma-separated-values,text/plain', 
                             '.xlsx')),
          fileInput('caselist', 'Upload Caselist File',
                    accept=c('xlsx', 
                             'text/comma-separated-values,text/plain', 
                             '.xlsx')),
          fileInput('progress', 'Upload Progress Monitoring File',
                    accept=c('xlsx', 
                             'text/comma-separated-values,text/plain', 
                             '.xlsx')),
          fileInput('studentlist', 'Upload Generated Student List File',
                    accept=c('xlsx', 
                             'text/comma-separated-values,text/plain', 
                             '.xlsx')),
          downloadButton('download_tier1', 'Download Tier 1 Data'),
          br(),
          downloadButton('download_site_coordination', 'Download Site Coordination'),
          br(),
          downloadButton('download_services', 'Download Service File'),
          br(),
          downloadButton('download_studentlist', 'Download Studentlist File')
          
        ),
        mainPanel(
          
         includeMarkdown("md/instructions.md")
        )
      )
      
      
      
        
  
      ),
    tabPanel("Reports", 
             sidebarLayout(
               sidebarPanel(
                 selectInput('school', 'School',
                             choices = c("EK Powe Elementary School", "Eno Valley Elementary", "Glenn Elementary School", "Merrick-Moore", "Shepard"))
                 
                 
               ),
               mainPanel(
                 
                 tabsetPanel(type = 'tabs',
                             tabPanel('Students', includeMarkdown("md/student_explanation.md"), tableOutput('student_table')),
                             tabPanel('Services',includeMarkdown("md/service_explanation.md"), tableOutput('service_table')),
                             tabPanel('Set Up', includeMarkdown("md/setup_explanation.md"), tableOutput('setup_table')),
                             tabPanel('Missing Grades', includeMarkdown("md/metric_explanation.md"), tableOutput('missing_grades_table'))
                             
               )
             )
    
             
    )
    )
  )
)
)