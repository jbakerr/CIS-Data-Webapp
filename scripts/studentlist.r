###########################  Studentlist Prep  #################################

# This script creates the studentlist file which is the master file for all the
# students on the caseload. The file includes the student demographics, progress
# monitoring data, hours, criteria, and flags based on ABC indicators. 

studentlist_script <- function(stlist){
  
  metrics <- c("Math","Science","ELA", "Suspensions", "Attendance_Rate")
  
  elem <- c(
    "Glenn Elementary School", "Eno Valley Elementary", 
    "EK Powe Elementary School", "Merrick-Moore"
    )
  
  high <- c(
    "Shepard", "Durham Performance Learning Center",
    "Hillside High School", "Southern High School", "Northern"
    )
  
  #removing any fully duplicated student entries
  stlist <- stlist[!duplicated(stlist[,c(
    "Student.ID", "Student","Birth.Date")]), ] 
  
  
  #Create average grade metric
  
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
  
  
  
  #Calculate Suspenion Data
  
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
  for(quarters in 1:length(subjects)){
    for(metrics in subjects[quarters]){
      quart_adjust <- paste("Q_", as.character(quarters),"_criteria", sep = "")
      stlist[,quart_adjust] <- ifelse(
        is.element(stlist$School, elem) &
          stlist[,quart_adjust] != 1 &
          (stlist[,metrics] <= 2 & !is.na(stlist[,metrics])),
        stlist[,quart_adjust] + 1, stlist[,quart_adjust]
      )
    }
  }
  
# High School Grading Scale For Loop  
  for(quarters in 1:length(subjects)){
    for(metrics in subjects[quarters]){
      quart_adjust <- paste("Q_", as.character(quarters),"_criteria", sep = "")
      stlist[,quart_adjust] <- ifelse(
        is.element(stlist$School, high) &
          stlist[,quart_adjust] != 1 &
          (stlist[,metrics] <= 70 & !is.na(stlist[,metrics])),
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
  for(quarter in 1:length(behavior_categories)){
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
  
  # Setting the attendance categories (not really used)  
  attendance_categories <- c(
    "Q1_Attendance_Rate", "Q2_Attendance_Rate", 
    "Q3_Attendance_Rate", "Q4_Attendance_Rate"
  )

  # Looping through quarters and attendance date to determine if criteria met.  
  for(quarter in 1:length(attendance_categories)){
    quart_adjust <- paste("Q_", as.character(quarter),"_criteria", sep = "")
    attend_review <- paste("Q", as.character(quarter),"_Attendance_Rate", sep = "")
    stlist[,quart_adjust] <- ifelse(
      stlist[,attend_review] == 0 | is.na(stlist[,attend_review]) , 
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
  

  
  
  #Creates no-metric column
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
  
  
  
  stlist$no_metrics <- apply(stlist[,68:71], 1, any)
  
  
  
# Caclulate Progress Monitoring Improvements -----------------------------------

  # improve.math <- subset(stlist, stlist$School %in% high & (stlist$Q2_Math - stlist$Q1_Math) >= 10)
  # improve.math$improve_math <- T
  # #improve.math <- subset(improve.math, !is.na(improve.math$Name))
  # 
  # improve.elm.math <- subset(stlist, stlist$School %in% elem & ((stlist$Q2_Math - stlist$Q1_Math) + (stlist$Q3_Math - stlist$Q2_Math)  >= 1.0 ))
  # improve.elm.math$improve_math <- T
  # #improve.elm.math <- subset(improve.elm.math, !is.na(improve.elm.math$Name))
  # 
  # 
  # improve.la <- subset(stlist, stlist$School %in% high & (stlist$Q2_ELA - stlist$Q1_ELA) >= 10)
  # improve.la$improve_la <- T
  # #improve.la <- subset(improve.la, !is.na(improve.la$Name))
  # 
  # improve.elm.la <- subset(stlist, stlist$School %in% elem & (stlist$Q2_ELA - stlist$Q1_ELA) + (stlist$Q3_ELA - stlist$Q2_ELA)  >= 1.0 )
  # improve.elm.la$improve_la <- T
  # #improve.elm.la <- subset(improve.elm.la, !is.na(improve.elm.la$Name))
  # 
  # improve.science <- subset(stlist, stlist$School %in% high & (stlist$Q2_Science - stlist$Q1_Science) >= 10)
  # improve.science$improve_science <- T
  # #improve.science <- subset(improve.science, !is.na(improve.science$Name))
  # 
  # improve.elm.science <- subset(stlist, stlist$School %in% elem & (stlist$Q2_Science - stlist$Q1_Science) + (stlist$Q3_Science - stlist$Q2_Science)  >= 1.0 )
  # improve.elm.science$improve_science <- T
  # #improve.elm.science <- subset(improve.elm.science, !is.na(improve.elm.science$Name))
  # 
  # 
  # improve.elem.attend <- subset(stlist, stlist$School %in% elem  & ((stlist$`Q2_Attendance_Rate` - stlist$`Q1_Attendance_Rate`) + (stlist$`Q3_Attendance_Rate` - stlist$`Q2_Attendance_Rate`) >= 6))
  # #improve.elem.attend <- subset(improve.elem.attend, !is.na(improve.elem.attend$Name))
  # #elem.attend.eligible <- subset(stlist, stlist$Site %in% elem  &  (stlist$totabs1 > 3 | stlist$totabs2 > 3 | stlist$totabs3 > 3 | stlist$totabs4 > 3))
  # 
  # improve.high.attend <- subset(stlist, stlist$School %in% high  & ((stlist$`Q2_Attendance_Rate` - stlist$`Q1_Attendance_Rate`) + (stlist$`Q3_Attendance_Rate` - stlist$`Q2_Attendance_Rate`) >= 10))
  # #improve.high.attend <- subset(improve.high.attend, !is.na(improve.high.attend$Name))
  # #high.attend.eligible <- subset(stlist, stlist$Site %in% high  &  (stlist$totabs1 > 5 | stlist$totabs2 > 5 | stlist$totabs3 > 5 | stlist$totabs4 > 5))
  # 
  # 
  # improve.grades <- merge(improve.la, improve.science, all = TRUE)
  # improve.grades <- merge(improve.grades, improve.math, all= T)
  # improve.grades <- merge(improve.grades, improve.elm.science, all = T)
  # improve.grades <- merge(improve.grades, improve.elm.la, all = T)
  # improve.grades <- merge(improve.grades, improve.elm.math, all = T)
  # improve.grades$improve.grades <- TRUE
  # 
  # improve.attend <- merge(improve.high.attend, improve.elem.attend, all = T)
  # improve.attend$attend <- TRUE
  # 
  # stlist$improve_grades <- F
  # stlist$improve_math <- F
  # stlist$improve_science <- F
  # stlist$improve_ela <- F
  # stlist$improve_all_grades <- F
  # 
  # stlist$improve_attend <- ifelse(stlist$Student.ID %in% improve.attend$Student.ID, stlist$improve.attend <- T, stlist$improve.attend <- F)
  # stlist$improve_grades <- ifelse(stlist$Student.ID %in% improve.grades$Student.ID, stlist$improve_grades <- T, stlist$improve_grades <- F)
  # stlist$improve_math <- ifelse(stlist$Student.ID %in% improve.grades[improve.grades$improve_math == T, "Student.ID"], stlist$improve_math <- T, stlist$improve_math <- F)
  # stlist$improve_science <- ifelse(stlist$Student.ID %in% improve.grades[improve.grades$improve_science == T, "Student.ID"], stlist$improve_science <- T, stlist$improve_science <- F)
  # stlist$improve_ela <- ifelse(stlist$Student.ID %in% improve.grades[improve.grades$improve_la == T, "Student.ID"], stlist$improve_ela <- T, stlist$improve_ela <- F)
  # stlist$improve_all_grades <- ifelse(stlist$improve_ela == T & stlist$improve_science == T & stlist$improve_math == T, stlist$improve_all_grades <- T, stlist$improve_all_grades <- F)


# Ranking Students By Quintile Based on Q1 Performance -------------------------
  #Bottom flagging students by percentiles (1/3)



  stlist$grade_quintile <- 2



    for(row in 1:nrow(stlist)){
      if(!is.na(stlist[row,]$avg.grade.Q1) & stlist[row,]$avg.grade.Q1 < quantile(stlist[stlist$School == stlist[row,]$School,]$avg.grade.Q1, prob = 0.33, na.rm = T)){
      stlist[row,]$grade_quintile <- 1
    }
    else if(!is.na(stlist[row,]$avg.grade.Q1) & stlist[row,]$avg.grade.Q1 > quantile(stlist[stlist$School == stlist[row,]$School,]$avg.grade.Q1, prob = 0.67, na.rm = T)){
      stlist[row,]$grade_quintile <- 3
    }

  }




  
  return(stlist)

}