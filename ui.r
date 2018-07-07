
ui <- fluidPage(
  fluidPage(
    img(src = "logo.png", height = 102, width = 102),
    titlePanel("Communities In Schools of Durham Data Process Manager"),
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
        downloadButton('download_tier1', 'Download Tier 1 Data'),
        br(),
        downloadButton('download_site_coordination', 'Download Site Coordination'),
        br(),
        downloadButton('download_services', 'Download Service File'),
        br(),
        downloadButton('download_studentlist', 'Download Studentlist File')
        
      ),
      mainPanel(
        h3("Instructions"),
        h4("Weekly Data Review"),
        p("Upload all of the files listed in the side bar and select all of the download buttons following all of the files successfully uploading"),
        br(),
        h4("To Generate Student List"),
        p("Upload the service file, caselist file, and progess monitoring file. Following successfull upload, the student list file can be downloaded")
        
  
      )
    )
  )
)