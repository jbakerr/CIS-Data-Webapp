################################# Services  ####################################

# This script takes the uploaded serices file renames some columns, drops unused
# columns, creates a variable for group size and hours spent

service_script <- function(data){
  
  
  colnames(data) <- make.names(colnames(data))
  
  data <- data.frame(apply(data, 2, function(x) gsub("^$|^ $", NA, x)))
  

  columns_to_drop <- colSums(is.na(data))<nrow(data)
  columns_to_drop <- names(which(columns_to_drop == FALSE))
  columns_to_drop <-  columns_to_drop[!grepl("Activity",unlist(columns_to_drop))]
  
  data[,columns_to_drop] <- NULL


  
  colnames(data)[1:2] <- c("Home.School","Student.ID")
  
# get rid of accidental blank rows
  data <- data[!is.na(data$Student.ID), ]

  
  drops <- c(
    "Provider.Type.2","ProviderUserType3", 
    "Donation.Value", "Total.Value.of.Time"
    )
  
  data <- data[, ! (names(data) %in% drops)]
  
  
  d <- data %>% group_by(Home.School, Entry.Date, Support.Date, Provider.Type.1, 
                         Activity, Student.Support.Category, Hours, Tier, Individual.or.Group
                         ) %>% summarize(groupsize = n())
  
  

  data <- merge(data, d, by = c("Home.School", "Entry.Date", "Support.Date", 
                                "Student.Support.Category", "Hours", "Tier",
                                "Activity"
                                )
                )
  
  data_test <- data
  as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
  
  data$Hours <- as.numeric.factor(data$Hours)

  
  data$hoursspent <- data$Hours/data$groupsize
  
  return(data)

}