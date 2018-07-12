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