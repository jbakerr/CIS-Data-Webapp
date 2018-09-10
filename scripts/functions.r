############################### Functions  #####################################



# Checks to ensure studentlist file exist in a generated form, or all the
# required files are uploaded to generate a student list file. If files are not 
# present will return FALSE. If they are present will return TRUE.

check_studentlist <- function(input){
  
  if(file_upload_check(input) == TRUE){
    return(FALSE)
  }
  
  else if(!is.null(input$studentlist)){
    return(TRUE)
  }
  else{
    if (is.null(input$caselist)){
      return(FALSE)
    }
    else if (is.null(input$services)){
      return(FALSE)
    }
    else if (is.null(input$progress)){
      return(FALSE)
    }
    else{
      return(TRUE)
    }
  }
}

# Creates a table for showing the students with the most and lease service hours
head_tail <- function(dataframe, head_number=5, tail_number=head_number){

  head_display <- head(dataframe,head_number)
  tail_display <- tail(dataframe,tail_number)
  
  colnames(head_display) <- c("Student", "High_Hours")
  colnames(tail_display) <- c("Student", "Low_Hours")
  
  
  list(head_display, tail_display)
}

# Pulls the caselist, services, and progress monitoring files together to create
# the student list file.

studentlist_creation <- function(caselist, progress, services){
  

  studentlist <-merge(caselist, progress, by = "Student.ID", all = T)
  
  stserv <- prep_service_file(services)
  
  stlist <- merge(studentlist, stserv, by = "Student.ID", all = T)
  
  studentlist <- studentlist_script(stlist)
  
  
  return(studentlist)
  
}

# Look into see if duplicating earlier function 
studentlist_check <- function(caselist, progress, data, studentlist){
  if(!is.null(studentlist)){
    studentlist <- read.csv(studentlist$datapath, header = T)
    return(studentlist)
  }
  else if(!is.null(studentlist_creation(caselist, progress, data))){
    studentlist <- studentlist_creation(caselist, progress, data)
    return(studentlist)
  }
  else{
    return(FALSE)}
}

# Generates a list of possible schools to select based on the inputed data sets
# Selected schools will then alter the tables in the report section

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
    return(NULL)
  }
  
}


studentlist_error_code <- "Please upload either a generated studentlist file or the required files to proceed."

services_error_code <- "Please upload the caselist, student support details, and student metrics files."



age_years <- function(earlier, later)
{
  lt <- data.frame(earlier, later)
  age <- as.numeric(format(lt[,2],format="%Y")) - as.numeric(format(lt[,1],format="%Y"))
  
  dayOnLaterYear <- ifelse(format(lt[,1],format="%m-%d")!="02-29",
                           as.Date(
                             paste(format(lt[,2],format="%Y"),"-",
                                   format(lt[,1],format="%m-%d"),sep="")),
                           ifelse(as.numeric(
                             format(later,format="%Y")) %% 400 == 0 | 
                               as.numeric(format(later,format="%Y")) %% 100 != 0 &
                               as.numeric(format(later,format="%Y")) %% 4 == 0,
                             as.Date(paste(format(lt[,2],format="%Y"),"-",
                                           format(lt[,1],format="%m-%d"),sep="")),
                             as.Date(paste(format(lt[,2],format="%Y"),
                                           "-","02-28",sep=""))))
  
  age[which(dayOnLaterYear > lt$later)] <- age[which(dayOnLaterYear > lt$later)] - 1
  
  age
}


attendance_check <- function(q_i, q_i_minus){
  if(is.na(q_i) | is.na(q_i_minus)){
    return(0)
  }
  else if(q_i == q_i_minus){
    return(0)
  }
  
  else if(q_i >= (q_i_minus/2 + 50)){
    return(1)
  }
  else if(q_i <= (q_i_minus/2 + 50) & (q_i >= q_i_minus - 2)){
    return(0)
  }
  else{
    return(-1)
  }
}

attend_improve_check <- function(q2, q3, q4){
  if(sum(q2, q3, q4) >= 1){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}


# Check For Correct Files Uploaded
# Returns TRUE if there is an error. 

file_upload_check <- function(input){
  
  if (!is.null(input$progress)){
    return(file_upload_check_internals(input$progress, 'Baseline'))
  }
  
  if (!is.null(input$services)){
    return(file_upload_check_internals(input$services, 'Individual or Group'))
  }
  
  
  if (!is.null(input$caselist)){
    return(file_upload_check_internals(input$caselist, "Initiatives"))
  }
  
  if (!is.null(input$tier1)){
    return(file_upload_check_internals(input$tier1, 'Student Level Attendance'))
  }

  if (!is.null(input$site_coordination)){
    return(file_upload_check_internals(input$site_coordination, 'Primary Provider'))
  }
  
  
  else{
    return(FALSE)
  }
}


file_upload_check_internals <- function(input, validation){
  
    file_to_check <- input
    nms <- names(read_excel(file_to_check$datapath, n_max = 0))
    if(!(validation %in% nms)){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
}

incorrect_file_message <- 'ERROR! File uploaded is incorrect, please upload the correct file.'
