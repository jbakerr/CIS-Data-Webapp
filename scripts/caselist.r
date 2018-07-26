##############################  Caselist  ######################################

# This script imports the caselist file, adjust some column headers, adds
# ages to students, and drops blank rows.  

suppressWarnings(source('scripts/functions.r', local = T))


caselist_script <- function(caselist){

  
  colnames(caselist) <- make.names(colnames(caselist))
  


  caselist <- data.frame(apply(caselist, 2, function(x) gsub("^$|^ $", NA, x)))
  
  caselist  <- caselist[,colSums(is.na(caselist))<nrow(caselist)]
  
  
# Changing name of columns, make sure that the column numbers are still accurate
  colnames(caselist)[1:2] <- c("School","Student.ID")
  
  caselist$Student.ID <- as.character(caselist$Student.ID)
  

  
  
  caselist$Birth.Date <- as.Date(caselist$Birth.Date)
  
  caselist$age <- age_years(caselist$Birth.Date, Sys.Date())


  
  return(caselist)

}
