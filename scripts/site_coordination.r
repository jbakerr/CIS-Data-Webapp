#########################  Site Coordination  ##################################

# Script will take site coordination file in xlsx format and return a csv file 

site_coordination_script <- function(site_coordination){  
  
  colnames(site_coordination) <- make.names(colnames(site_coordination))
  
  site_coordination <- site_coordination[!is.na(site_coordination$School), ]
  
  return(site_coordination)
}

