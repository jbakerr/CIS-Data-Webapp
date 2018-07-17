caselist_script <- function(caselist){
  #Adding Caselist 
  
  #caselist<-read.csv('caselist.csv', skip = 3, header = T, sep = ",")

  # caselist$Birth.Date <- convertToDate(caselist$Birth.Date)
  
  caselist <- data.frame(apply(caselist, 2, function(x) gsub("^$|^ $", NA, x)))
  
  caselist  <- caselist[,colSums(is.na(caselist))<nrow(caselist)]
  
  
  #Changing name of columns, make sure that the column numbers are still accurate. 
  colnames(caselist)[1:2] <- c("School","Student.ID")
  
  #Changing name of the progress dataset 
  
  caselist$Student.ID <- as.character(caselist$Student.ID)
  
  age_years <- function(earlier, later)
  {
    lt <- data.frame(earlier, later)
    age <- as.numeric(format(lt[,2],format="%Y")) - as.numeric(format(lt[,1],format="%Y"))
    
    dayOnLaterYear <- ifelse(format(lt[,1],format="%m-%d")!="02-29",
                             as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                             ifelse(as.numeric(format(later,format="%Y")) %% 400 == 0 | as.numeric(format(later,format="%Y")) %% 100 != 0 & as.numeric(format(later,format="%Y")) %% 4 == 0,
                                    as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                                    as.Date(paste(format(lt[,2],format="%Y"),"-","02-28",sep=""))))
    
    age[which(dayOnLaterYear > lt$later)] <- age[which(dayOnLaterYear > lt$later)] - 1
    
    age
  }
  
  
  caselist$Birth.Date <- as.Date(caselist$Birth.Date)
  
  caselist$age <- age_years(caselist$Birth.Date, Sys.Date())

  
  
  #Adding Birth days
  
  return(caselist)

}
