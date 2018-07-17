

check_studentlist <- function(){
  
  if (is.null(input$caselist))
    return(FALSE)
  else if (is.null(input$services))
    return(FALSE)
  else if (is.null(input$progress))
    return(FALSE)
  else if (is.null(input$studentlist))
    return(FALSE)
  else{
    return(TRUE)
  }
}


head_tail <- function(dataframe, head_number=5, tail_number=head_number){
  # print the head and tail together
  list(High = head(dataframe,head_number), Low = tail(dataframe,tail_number))
}


studentlist_creation <- function(caselist, progress, data){
  
  # req(input$services, input$caselist, input$progress)
  
  studentlist <-merge(caselist, progress, by = "Student.ID", all = T)
  
  stserv <- prep_service_file(data)
  
  stlist <- merge(studentlist, stserv, by = "Student.ID", all = T)
  
  studentlist <- studentlist_script(stlist)
  
  
  return(studentlist)
  
}


studentlist_check <- function(caselist, progress, data, studentlist){
  if(!is.null(studentlist)){
    return(studentlist)
  }
  else if(!is.null(studentlist_creation(caselist, progress, data))){
    studentlist <- studentlist_creation(caselist, progress, data)
    return(studentlist)
  }
  else{
    return(FALSE)}
}


school_options <- function(caselist, studentlist, services){
  if(!is.null(services)){
    schools <- unique(services$Home.School)
    return(schools)
  }
  else if(!is.null(caselist)){
    schools <- unique(caselist$School)
    print(schools)
    return(schools)
  }
  else if(!is.null(studentlist)){
    schools <- unique(studentlist$School)
    return(schools)
  }
  else{
    return()
  }
  
}