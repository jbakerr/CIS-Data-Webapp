
ui <- fluidPage(
  fluidPage(
    titlePanel("Uploading Files"),
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
        downloadButton('download_site_coordination', 'Download Site Coordination'),
        downloadButton('download_services', 'Download Service File'),
        downloadButton('download_studentlist', 'Download Studentlist File')
        
      ),
      mainPanel(
        tableOutput('contents')
      )
    )
  )
)