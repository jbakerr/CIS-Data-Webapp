###########################  Studentlist Prep  #################################

# This script creates the studentlist file which is the master file for all the
# students on the caseload. The file includes the student demographics, progress
# monitoring data, hours, criteria, and flags based on ABC indicators. 

studentlist_script <- function(stlist){
  
  
# Determining what quarters to include in metric check
  
  stlist$First.CIS.Enrollment.Date <- as.Date(stlist$First.CIS.Enrollment.Date)
  
  
  start_year <-  as.integer(format(Sys.Date(), "%Y"))
  
  if (as.Date(Sys.Date()) > as.Date(paste(start_year, "-08-15", sep = "")) &
      as.Date(Sys.Date()) < as.Date(paste(start_year, "-12-31", sep = ""))){
    start_year <- start_year
  } else{
    
    start_year <- start_year - 1
  }
  
  
  Q1_date <- as.Date(paste(start_year, "-10-31", sep = ""))
  Q2_date <- as.Date(paste(start_year + 1, "-01-15", sep = ""))
  Q3_date <- as.Date(paste(start_year + 1, "-03-31", sep = ""))
  Q4_date <- as.Date(paste(start_year + 1, "-06-10", sep = ""))
  
  quarters_to_include <- 1
  
  if(Sys.Date() >= Q1_date + 10){
    quarters_to_include <- 1
  }  
  if (Sys.Date() >= Q2_date + 10){
    quarters_to_include <- 2
  } 
  if (Sys.Date() >= Q3_date + 10){
    quarters_to_include <- 3
  } 
  if (Sys.Date() >= Q4_date + 10){
    quarters_to_include <- 4
  }
  
  

  
  metrics <- c("Math", "Science","ELA", "Suspensions", "Attendance_Rate")
  
  elem <- c(
    "Glenn Elementary School", "Eno Valley Elementary", 
    "EK Powe Elementary School", "Merrick-Moore"
    )
  
  high <- c(
    "Shepard", "Durham Performance Learning Center",
    "Hillside High School", "Southern High School", "Northern"
    )
  
# removing any fully duplicated student entries
  stlist <- stlist[!duplicated(stlist[,c(
    "Student.ID", "Student","Birth.Date")]), ] 
  
  
# Create average grade metric
  
  grades <- c(
    "Q1_Science", "Q1_Math", "Q1_ELA","Q2_Science", "Q2_Math", "Q2_ELA", 
    "Q1_Attendance_Rate", "Q2_Attendance_Rate", "Q3_Science", "Q3_Math", 
    "Q3_ELA", "Q3_Attendance_Rate", "Q4_Science", "Q4_Math", "Q4_ELA",
    "Q4_Attendance_Rate"
    )
  
  stlist[,colnames(stlist) %in% grades] <- sapply(
    stlist[,colnames(stlist) %in% grades], as.numeric
    )
  

  stlist$avg.grade.Q1 <- 0
  
  stlist$avg.grade.Q1 <- rowMeans(stlist[, c(
    "Q1_Science", "Q1_Math", "Q1_ELA")], na.rm =T)
  
  stlist$avg.grade.Q2 <- 0
  stlist$avg.grade.Q2 <- rowMeans(stlist[, c(
    "Q2_Science", "Q2_Math", "Q2_ELA")], na.rm =T)
  
  stlist$avg.grade.Q3 <- 0
  stlist$avg.grade.Q3 <- rowMeans(stlist[, c(
    "Q3_Science", "Q3_Math", "Q3_ELA")], na.rm =T)
  
  stlist$avg.grade.Q4 <- 0
  stlist$avg.grade.Q4 <- rowMeans(stlist[, c(
    "Q4_Science", "Q4_Math", "Q4_ELA")], na.rm =T)
  
  
  stlist$avg.grade <- rowMeans(stlist[, c(
    "avg.grade.Q1","avg.grade.Q2", "avg.grade.Q3", "avg.grade.Q4")], na.rm = T)
  
  
  stlist$avg.attend <- rowMeans(stlist[, c(
    "Q1_Attendance_Rate", "Q2_Attendance_Rate", 
    "Q3_Attendance_Rate", "Q4_Attendance_Rate")], na.rm = T)
  
  
# Calculate Suspenion Data
  
  stlist$suspended <- F
  
  stlist$suspended <- ifelse(
    is.na(stlist$Q1_Suspensions) & 
      is.na(stlist$Q2_Suspensions) & 
      is.na(stlist$Q3_Suspensions) & 
      is.na(stlist$Q4_Suspensions),
    stlist$suspended <- F, stlist$suspended <- T
    )
  
  stlist$suspended <- ifelse(
    stlist$suspended == T & 
      (stlist$Q1_Suspensions > 0 | 
         stlist$Q2_Suspensions > 0 | 
         stlist$Q3_Suspensions > 0 | 
         stlist$Q4_Suspensions > 0), 
    stlist$suspended <- T, stlist$suspended <- F
    )
  
  
# This section creates a new variable, criteria, which calculates the number of
# eligibility criteria a student meets.
  
# Criteria Calculations --------------------------------------------------------
  
# Starting all criteria at 0  
  
  stlist$`Q_1_criteria` <- 0
  stlist$`Q_2_criteria` <- 0
  stlist$`Q_3_criteria` <- 0
  stlist$`Q_4_criteria` <- 0
  stlist$criteria <- 0
  
  
# Setting category criteria to False
  
  stlist$attend_criteria <- F
  stlist$beh_criteria <- F
  stlist$course_criteria <- F
  

# Calculating Course Work Criteria ---------------------------------------------

# loop through each quarter and each subject to determine if student failed a
# core class. If a class was failed, then the criteria met increases by 1

# Setting up the subject titles
  q1_subjects <- c("Q1_Math","Q1_Science","Q1_ELA")
  q2_subjects <- c("Q2_Math","Q2_Science","Q2_ELA")
  q3_subjects <- c("Q3_Math","Q3_Science","Q3_ELA")
  q4_subjects <- c("Q4_Math","Q4_Science","Q4_ELA")

# Creating a list of the subject titles
  subjects <- list(q1_subjects = q1_subjects, q2_subjects = q2_subjects,
                   q3_subjects = q3_subjects, q4_subjects = q4_subjects)

# Elementary Grading Scale For Loop
  for(quarters in 1:quarters_to_include){
    for(metrics in 1:3){
      quart_adjust <- paste("Q_", as.character(quarters),"_criteria", sep = "")
      stlist[,quart_adjust] <- ifelse(
        is.element(stlist$School, elem) &
          stlist[,quart_adjust] != 1 &
          (stlist[,subjects[[quarters]][[metrics]]] <= 2 & !is.na(stlist[,subjects[[quarters]][[metrics]]])),
        stlist[,quart_adjust] + 1, stlist[,quart_adjust]
      )
    }
  }

# High School Grading Scale For Loop

  for(quarters in 1:quarters_to_include){
    for(metrics in 1:3){
      quart_adjust <- paste("Q_", as.character(quarters),"_criteria", sep = "")
      stlist[,quart_adjust] <- ifelse(
        is.element(stlist$School, elem) &
          stlist[,quart_adjust] != 1 &
          (stlist[,subjects[[quarters]][[metrics]]] <= 70 & !is.na(stlist[,subjects[[quarters]][[metrics]]])),
        stlist[,quart_adjust] + 1, stlist[,quart_adjust]
      )
    }
  }

stlist$max_criteria <- pmax(
  stlist$`Q_1_criteria`, stlist$`Q_2_criteria`,
  stlist$`Q_3_criteria`, stlist$`Q_4_criteria`
  )

# Determining if student met the course criteria
  stlist$course_criteria <- ifelse(
    stlist$max_criteria == 1,
    stlist$course_criteria <- T, stlist$course_criteria <- F
    )


# Calculating Behavior Criteria ------------------------------------------------

# Setting the bahavior categories (not really used)
  behavior_categories <- c(
    "Q1_Suspensions", "Q2_Suspensions", "Q3_Suspensions", "Q4_Suspensions"
    )

# Looping through quarters and suspension date to determine if criteria met.
  for(quarter in 1:quarters_to_include){
    quart_adjust <- paste("Q_", as.character(quarter),"_criteria", sep = "")
    suspen_review <- paste("Q", as.character(quarter),"_Suspensions", sep = "")
    stlist[,quart_adjust] <- ifelse(
    stlist[,suspen_review] == 0 | is.na(stlist[,suspen_review]) ,
    stlist[,quart_adjust], stlist[,quart_adjust] + 1
    )
  }

  stlist$max_criteria <- pmax(
    stlist$`Q_1_criteria`, stlist$`Q_2_criteria`,
    stlist$`Q_3_criteria`, stlist$`Q_4_criteria`
    )

# Checking to see if student met behaviour criteria
  stlist$beh_criteria <- ifelse(
    stlist$max_criteria == 2 |
      (stlist$max_criteria == 1 & stlist$course_criteria == F),
    stlist$beh_criteria <- T, stlist$beh_criteria <- F
    )


# Calculating Attendance Criteria ----------------------------------------------



# Looping through quarters and attendance date to determine if criteria met.
  for(quarter in 1:quarters_to_include){
    quart_adjust <- paste("Q_", as.character(quarter),"_criteria", sep = "")
    attend_review <- paste("Q", as.character(quarter),"_Attendance_Rate", sep = "")
    stlist[,quart_adjust] <- ifelse(
      stlist[,attend_review] > 90 | is.na(stlist[,attend_review]) ,
      stlist[,quart_adjust], stlist[,quart_adjust] + 1
    )
  }

  stlist$max_criteria <- pmax(
    stlist$`Q_1_criteria`, stlist$`Q_2_criteria`,
    stlist$`Q_3_criteria`, stlist$`Q_4_criteria`
    )

  stlist$attend_criteria <- ifelse(
    stlist$max_criteria == 3 |
      (stlist$max_criteria == 1 &
         stlist$course_criteria == F & stlist$beh_criteria == F) |
                                     (stlist$max_criteria == 2 &
                                        (stlist$beh_criteria == F |
                                           stlist$course_criteria == F)),
    stlist$attend_criteria <- T, stlist$attend_criteria <- F
    )


# Metric Check Calculation -----------------------------------------------------


# Creates no-metric column
  stlist$no_metrics <- FALSE
  stlist$no_metrics_Q1 <- FALSE
  stlist$no_metrics_Q2 <- FALSE
  stlist$no_metrics_Q3 <- FALSE
  stlist$no_metrics_Q4 <- FALSE


  metrics_colums <- c(
    "Q1_Science", "Q1_Math", "Q1_ELA","Q1_Suspensions", "Q1_Attendance_Rate",
    "Q2_Science", "Q2_Math", "Q2_ELA","Q2_Suspensions", "Q2_Attendance_Rate",
    "Q3_Science", "Q3_Math", "Q3_ELA","Q3_Suspensions", "Q3_Attendance_Rate",
    "Q4_Science", "Q4_Math", "Q4_ELA","Q4_Suspensions", "Q4_Attendance_Rate"
    )

  stlist$no_metrics_Q1 <- ifelse(Sys.Date() > Q1_date &
                                   stlist$First.CIS.Enrollment.Date < Q1_date,
                                 stlist$no_metrics_Q1 <- (rowSums(
                                   is.na(stlist[, metrics_colums[1:5]])) > 1 ),
                                 stlist$no_metrics_Q1 <- FALSE)


  stlist$no_metrics_Q2 <- ifelse(Sys.Date() > Q2_date &
                                   stlist$First.CIS.Enrollment.Date < Q2_date,
                                 stlist$no_metrics_Q2 <- (rowSums(
                                   is.na(stlist[, metrics_colums[6:10]])) > 1 ),
                                 stlist$no_metrics_Q2 <- FALSE)


  stlist$no_metrics_Q3 <- ifelse(Sys.Date() > Q3_date &
                                   stlist$First.CIS.Enrollment.Date < Q3_date,
                                 stlist$no_metrics_Q3 <- (
                                   rowSums(is.na(stlist[, metrics_colums[11:15]])) > 1 ),
                                 stlist$no_metrics_Q3 <- FALSE)

  stlist$no_metrics_Q4 <- ifelse(Sys.Date() > Q4_date &
                                   stlist$First.CIS.Enrollment.Date < Q4_date,
                                 stlist$no_metrics_Q4 <- (rowSums(
                                   is.na(stlist[, metrics_colums[16:20]])) > 1),
                                 stlist$no_metrics_Q4 <- FALSE)


  stlist$no_metrics <- apply(stlist[,87:90], 1, any)


# Caclulate Progress Monitoring Improvements -----------------------------------

# Academic Improvements --------------------------------------------------------

# 
#   metric_categories <- c('Math', 'Science', 'ELA')
# 
# # Middle and High School Calculations
# 
#   for(i in metric_categories){
# 
#     assign(paste("improve_", i, sep = ''),
#            subset(stlist, stlist$School %in% high & (
#       (stlist[,paste("Q4_", i, sep = '')] - stlist[,paste("Q3_", i, sep = '')]) +
#         (stlist[,paste("Q3_", i, sep = '')] - stlist[,paste("Q2_", i, sep = '')]) +
#         (stlist[,paste("Q2_", i, sep = '')] - stlist[,paste("Q1_", i, sep = '')])  >= 10)
#       )
#     )
#   }
# 
# 
# # Elementary Calculations
# 
#   for(i in metric_categories){
# 
#     assign(paste("improve_elem_", i, sep = ''),
#            subset(stlist, stlist$School %in% elem & (
#              (stlist[,paste("Q4_", i, sep = '')] - stlist[,paste("Q3_", i, sep = '')]) +
#                (stlist[,paste("Q3_", i, sep = '')] - stlist[,paste("Q2_", i, sep = '')]) +
#                (stlist[,paste("Q2_", i, sep = '')] - stlist[,paste("Q1_", i, sep = '')])  >= 1.0)
#            )
#     )
#   }
# 
# # If a student appears in the following data set they have improved
# # the appropriate metric
# 
#   improve_Math$improve_Math <- logical()
#   improve_Science$improve_Science <- logical()
#   improve_ELA$improve_ELA <- logical()
#   improve_elem_Math$improve_Math <- logical()
#   improve_elem_Science$improve_Science <- logical()
#   improve_elem_ELA$improve_ELA <- logical()
# 
# 
# # Merging datasets
# 
#   improve_Science <- merge(improve_Science, improve_elem_Science, all = T)
#   improve_Math <- merge(improve_Math, improve_elem_Math, all = T)
#   improve_ELA <- merge(improve_ELA, improve_elem_ELA, all = T)
#   improve_grades <- merge(improve_ELA, improve_Science, all = T)
#   improve_grades <- merge(improve_grades, improve_Math, all = T)
#   improve_grades$improve_grades <- logical()
# 
# # Setting the master file's default value to False, will get overwritten if
# # student appears in the improve_grades data frame.
# 
#   stlist$improve_grades <- F
#   stlist$improve_math <- F
#   stlist$improve_science <- F
#   stlist$improve_ela <- F
#   stlist$improve_all_grades <- F
# 
# # Checking if students from master list appear in the improve grade subsets.
# 
#   stlist$improve_grades <- ifelse(
#     stlist$Student.ID %in% improve_grades$Student.ID,
#     stlist$improve_grades <- T, stlist$improve_grades <- F
#     )
#   stlist$improve_math <- ifelse(
#     stlist$Student.ID %in% improve_grades[improve_grades$improve_Math == T,
#                                           "Student.ID"],
#     stlist$improve_math <- T, stlist$improve_math <- F
#     )
#   stlist$improve_science <- ifelse(
#     stlist$Student.ID %in% improve_grades[improve_grades$improve_Science == T,
#                                           "Student.ID"],
#     stlist$improve_science <- T, stlist$improve_science <- F
#     )
#   stlist$improve_ela <- ifelse(
#     stlist$Student.ID %in% improve_grades[improve_grades$improve_ELA == T,
#                                           "Student.ID"],
#     stlist$improve_ela <- T, stlist$improve_ela <- F
#     )
#   stlist$improve_all_grades <- ifelse(
#     stlist$improve_ela == T & stlist$improve_science == T &
#       stlist$improve_math == T,
#     stlist$improve_all_grades <- T, stlist$improve_all_grades <- F
#     )
# 
# 
# 
# # Attendance Improvements ------------------------------------------------------
# 
# 
# 
#   stlist$improve_q2_attend <- 0
#   stlist$improve_q3_attend <- 0
#   stlist$improve_q4_attend <- 0
# 
#   stlist$improve_q2_attend <- apply(
#     stlist[, c('Q1_Attendance_Rate', 'Q2_Attendance_Rate')], 1,
#     function(x) attendance_check(x[2], x[1])
#     )
# 
#   stlist$improve_q3_attend <- apply(
#     stlist[, c('Q2_Attendance_Rate', 'Q3_Attendance_Rate')], 1,
#     function(x) attendance_check(x[2], x[1])
#     )
# 
#   stlist$improve_q4_attend <- apply(
#     stlist[, c('Q3_Attendance_Rate', 'Q4_Attendance_Rate')], 1,
#     function(x) attendance_check(x[2], x[1])
#     )
# 
#   stlist$improve_attend <- FALSE
# 
#   stlist$improve_attend <- apply(
#     stlist[, c("improve_q2_attend", "improve_q3_attend", "improve_q4_attend")], 1,
#     function(x) attend_improve_check(x[1],x[2],x[3])
#     )


# Ranking Students By Quintile Based on Q1 Performance -------------------------

# Bottom flagging students by percentiles (1/3)

# 
#   stlist$grade_quintile <- 2
# 
#     for(row in 1:nrow(stlist)){
# 
#       if(!is.na(stlist[row,]$avg.grade.Q1) &
#          stlist[row,]$avg.grade.Q1 <
#          quantile(stlist[stlist$School == stlist[row,]$School,]$avg.grade.Q1,
#                   prob = 0.33, na.rm = T)){
# 
#       stlist[row,]$grade_quintile <- 1
#       }
# 
#     else if(!is.na(stlist[row,]$avg.grade.Q1) &
#             stlist[row,]$avg.grade.Q1 >
#             quantile(stlist[stlist$School == stlist[row,]$School,]$avg.grade.Q1,
#                      prob = 0.67, na.rm = T)){
#       stlist[row,]$grade_quintile <- 3
#     }
# 
#   }


  return(stlist)

}
