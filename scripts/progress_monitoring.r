#######################  Progress Monitoring  ##################################

# Script will take the inputed progress monitoring file, edit columns titles and 
# metric titles and remove unused columns. Script will also check for any errors
# in how a student is setup - i.e missing baseline or metric targets. 


progress_import_script <- function(progress){

  colnames(progress) <- make.names(colnames(progress))
  
# Setting Vectors that will be used throughout program
  metrics <- c("Math", "Math2", "Science","ELA", "Suspensions", "Attendance_Rate")
  
  elem <- c(
    "Glenn Elementary School", "Eno Valley Elementary",
    "EK Powe Elementary School", "Merrick-Moore"
    )
  
  high <- c(
    "Shepard", "Durham Performance Learning Center", 
    "Hillside High School", "Southern High School", "Northern"
    )
  
# Rename Grading Quarters - make sure that the column numbers are acurate 
  colnames(progress)[11:14] <- c("Q1", "Q2", "Q3", "Q4")
  
  
# Changing Metric Title - make sure that the first section in quotes is 
# exactly as found in the excel file. 
  progress$Metric[progress$Metric =='Core Course Grades: Eng/Lang Arts/Reading/Writing'] <- "ELA"
  progress$Metric[progress$Metric =='Core Course Grades: Math 1'] <- "Math"
  progress$Metric[progress$Metric =='Core Course Grades: Math 2'] <- "Math2"
  progress$Metric[progress$Metric =='Core Course Grades: Science'] <- "Science"
  progress <- progress[ ! (progress$Metric =='Standardized test score: English / Language Arts'),]
  progress <- progress[ ! (progress$Metric =='Standardized test score: Science'),]
  progress$Metric[progress$Metric =='Attendance Rate'] <- "Attendance_Rate"
  
  
# Rearranged column orders
  # progress <- progress[,c(1:14, 9:12,21,13, 14:20)]
  # c(1:8, 9:12,21,13, 14:20)

# Changing Student Name to Student
  colnames(progress)[4] <- c("Student")

# Dropping Unneeded Columns
  progress$Latest.Progress <- NULL
  progress$Target.Set.Date <- NULL
  progress$Affiliate <- NULL
  progress$Enrollment.End.Date <- NULL
  progress$School.Year <- NULL
  progress$Baseline.Date <- NULL
  progress$Goal <- NULL
  progress$Enrollment.Begin.Date <- NULL
  progress$Enrollment.Status <- NULL
  progress$EnrollmentID <- NULL

# removing unwanted metrics - if the column title doesn't apear in the list of 
# metrics from the environment.r script, then they will be deleted.
  progress <- subset(progress, progress$Metric %in% metrics)

# Creating Names for each quarter / subject combination

  progress <- gather(progress, Period, Value, Baseline:Q4, factor_key = T)
  quartersubject <- paste(progress$Period, "_", progress$Metric, sep = "")
  progress$quartersubject <- quartersubject


# removing duplicates
# This is a soft option that just deletes one of the duplicates arbitrarily

  progress <- progress[!duplicated(progress[,c(
    "School", "Student", "Metric", "Value", "Student.ID",
    "Period"
    )]), ]

#Creating a wide data frame
  progress <- spread(progress[, ! colnames(progress) %in% 
                                c("Metric", "Period")], quartersubject, Value)

# Checking for incomplete progress monitoring settups. If a studuent is 
# missing baseline or goal data they will be flagged as TRUE
  progress$attend_error <- ifelse(is.na(progress$`Baseline_Attendance_Rate`) |
                                    is.na(progress$`Target_Attendance_Rate`), 
                                  TRUE, FALSE
                                  )
  
  progress$math_error <- ifelse(is.na(progress$Baseline_Math) | 
                                  is.na(progress$Target_Math), 
                                TRUE, FALSE
                                )
  
  progress$science_error <- ifelse(is.na(progress$Baseline_Science) | 
                                     is.na(progress$Target_Science), 
                                   TRUE, FALSE
                                   )
  
  progress$ELA_error <- ifelse(is.na(progress$Baseline_ELA) | 
                                 is.na(progress$Target_ELA), 
                               TRUE, FALSE
                               )
  
  progress$suspension_error <- ifelse(is.na(progress$Baseline_Suspensions) | 
                                        is.na(progress$Target_Suspensions),
                                      TRUE, FALSE
                                      )
  
  progress$math_error <- ifelse(progress$math_error == TRUE & (is.na(progress$Baseline_Math2) & 
                                  is.na(progress$Target_Math2)),
                                TRUE, FALSE
                                )
  
  progress$error <- ifelse(progress$attend_error == T |
                             progress$math_error == T | 
                             progress$science_error == T |
                             progress$ELA_error == T |
                             progress$suspension_error == T,
                           TRUE, FALSE
                           )

# Removing Baseline and Target data now that it isn't relevant
  # progress$`Baseline_Attendance_Rate` <- NULL
  # progress$Baseline_ELA <- NULL
  # progress$Baseline_Math <- NULL
  # progress$Baseline_Math2 <- NULL
  # progress$Baseline_Science <- NULL
  # progress$Baseline_Suspensions <- NULL
  # 
  # progress$Target_ELA <- NULL
  # progress$`Target_Attendance_Rate` <- NULL
  # progress$Target_Math <- NULL
  # progress$Target_Math2 <- NULL
  # progress$Target_Suspensions <- NULL
  # progress$Target_Science <- NULL

  progress[,c(1:2, 4)] <- NULL
  progress$Student.ID <- as.character(progress$Student.ID)

  return(progress)
  
}
  
  
  
  
