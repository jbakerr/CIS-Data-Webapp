############################### Functions  #####################################



# Checks to ensure studentlist file exist in a generated form, or all the
# required files are uploaded to generate a student list file. 

check_studentlist <- function(input){
  
  if(!is.null(input$studentlist)){
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

services_error_code <- "Please upload the services file."



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