site_coordination_script <- function(site_coordination){  
  
  colnames(site_coordination) <- make.names(colnames(site_coordination))
  
  #site_coordination <- data.frame(apply(site_coordination, 2, function(x) gsub("^$|^ $", NA, x)))
  #site_coordination  <- site_coordination[,colSums(is.na(site_coordination))<nrow(site_coordination)]
  
  site_coordination <- site_coordination[!is.na(site_coordination$School), ]
  
  #colnames(site_coordination)[15:18] <- c("students_served", "parents_served", "other_served", "volunteers")
  
  return(site_coordination)
}

