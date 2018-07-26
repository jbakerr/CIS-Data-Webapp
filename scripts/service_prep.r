##########################  Service Prep  ######################################

# This script preps the service data file for use in generating the studentlist
# file. It collects all the hours each student has recieved, the number of 
# after school services they've had, the service hours in the previous month
# and the average service hours per month

prep_service_file <- function(data){
  
  # Adding service aggregates to student list
  stserv <- data %>%
    group_by(Student.ID) %>% 
    summarize(Hours = sum(Hours),
              num_serv = length(Student.ID), 
              service_date = tail(Support.Date, n =1 )
              )
  
  afterschool_hours <- data %>% 
    group_by(Student.ID) %>% 
    filter(Activity == "21st Century Afterschool") %>% 
    summarise(afterschool_hours = sum(Hours))
  
  previous_months_service <- data %>% 
    group_by(Student.ID) %>% 
    filter(month(Support.Date) == month(Sys.Date())-1) %>% 
    summarise(previous_months_service = sum(Hours))
  
  month_serv <- data %>% 
    group_by(Student.ID) %>% 
    summarise(month_serv = length(unique(month(Support.Date))))  
  
  
  stserv <- merge(stserv, afterschool_hours, all = T)
  stserv <- merge(stserv, previous_months_service, all = T)
  stserv <- merge(stserv, month_serv, all = T)
  
  
  stserv$non_afterschool_hours <- stserv$Hours - stserv$afterschool_hours
  
  
  stserv$avg_month_serv <- stserv$Hours / stserv$month_serv

  
  
  return(stserv)
  
  
}