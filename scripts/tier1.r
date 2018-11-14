################################  Tier 1  ######################################

# Script will take inputed tier 1 file as xlsx and return a csv file 

tier1_script <- function(tier1){

  colnames(tier1) <- make.names(colnames(tier1))
  
  # tier1 <- data.frame(apply(tier1, 2, function(x) gsub("^$|^ $", NA, x)))
  # tier1  <- tier1[,colSums(is.na(tier1))<nrow(tier1)]
  
  tier1 <- tier1[!is.na(tier1$School), ]
  
  colnames(tier1)[14:15] <- c(
    "students_served", "other_served"
    )
  
  return(tier1)
}