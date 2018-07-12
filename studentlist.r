studentlist_script <- function(stlist){
  
  metrics <- c("Math","Science","ELA", "Suspensions", "Attendance Rate")
  
  elem <- c("Glenn Elementary School", "Eno Valley Elementary", "EK Powe Elementary School", "Merrick-Moore")
  high <- c("Shepard", "Durham Performance Learning Center", "Hillside High School", "Southern High School", "Northern")
  
  #removing any fully duplicated student entries
  stlist <- stlist[!duplicated(stlist[,c("Student.ID", "Student","Birth.Date")]), ] # This is a soft option that just deletes one of the duplicates arbitrarily
  
  
  #Create average grade metric
  
  grades <- c("Q1_Science", "Q1_Math", "Q1_ELA","Q2_Science", "Q2_Math", "Q2_ELA", "Q1_Attendance Rate", "Q2_Attendance Rate", "Q3_Science", "Q3_Math", "Q3_ELA", "Q3_Attendance Rate", "Q4_Science", "Q4_Math", "Q4_ELA", "Q4_Attendance Rate")
  
  stlist[,colnames(stlist) %in% grades] <- sapply(stlist[,colnames(stlist) %in% grades], as.numeric )
  
  
  
  
  stlist$avg.grade.Q1 <- 0
  stlist$avg.grade.Q1 <- rowMeans(stlist[, c("Q1_Science", "Q1_Math", "Q1_ELA")], na.rm =T)
  
  stlist$avg.grade.Q2 <- 0
  stlist$avg.grade.Q2 <- rowMeans(stlist[, c("Q2_Science", "Q2_Math", "Q2_ELA")], na.rm =T)
  
  stlist$avg.grade.Q3 <- 0
  stlist$avg.grade.Q3 <- rowMeans(stlist[, c("Q3_Science", "Q3_Math", "Q3_ELA")], na.rm =T)
  
  stlist$avg.grade.Q4 <- 0
  stlist$avg.grade.Q4 <- rowMeans(stlist[, c("Q4_Science", "Q4_Math", "Q4_ELA")], na.rm =T)
  
  
  stlist$avg.grade <- rowMeans(stlist[, c("avg.grade.Q1","avg.grade.Q2", "avg.grade.Q3", "avg.grade.Q4")], na.rm = T)
  
  
  stlist$avg.attend <- rowMeans(stlist[, c("Q1_Attendance Rate", "Q2_Attendance Rate", "Q3_Attendance Rate", "Q4_Attendance Rate")], na.rm = T)
  # 
  # #Aggregating Outcome info
  # stlist$avgrade1 <- rowMeans(stlist[, c("Q1_Science", "Q1_Math", "Q1_ELA", "Q_", "Q_1 Lang. Arts")], na.rm =T)
  # stlist$avgrade2 <- rowMeans(stlist[, c("Q_2 Science", "Q_2 Math", "Q_2 Writing", "Q_2 Reading", "Q_2 Lang. Arts")], na.rm =T)
  # stlist$avgrade3 <- rowMeans(stlist[, c("Q_3 Science", "Q_3 Math", "Q_3 Writing", "Q_3 Reading", "Q_3 Lang. Arts")], na.rm =T)# will give an error before 3rd quarter outcomes are entered
  # stlist$avgrade4 <- rowMeans(stlist[, c("Q_4 Science", "Q_4 Math", "Q_4 Writing", "Q_4 Reading", "Q_4 Lang. Arts")], na.rm =T)# will give an error before 4th quarter outcomes are entered
  # stlist$avgrade <- rowMeans(stlist[, colnames(stlist) %in% c("avgrade1", "avgrade2", "avgrade3", "avgrade4")], na.rm = T)
  # stlist$nogrades <- is.na(stlist$avgrade)
  # stlist$nogrades1 <- is.na(stlist$avgrade1)
  # stlist$nogrades2 <- is.na(stlist$avgrade2)
  # stlist$nogrades3 <- is.na(stlist$avgrade3)
  # stlist$nogrades4 <- is.na(stlist$avgrade4)
  
  
  
  #Calculate Suspenion Data
  
  stlist$suspended <- F
  
  stlist$suspended <- ifelse(is.na(stlist$Q1_Suspensions) & is.na(stlist$Q2_Suspensions) & is.na(stlist$Q3_Suspensions) & is.na(stlist$Q4_Suspensions), stlist$suspended <- F, stlist$suspended <- T)
  stlist$suspended <- ifelse(stlist$suspended == T & (stlist$Q1_Suspensions > 0 | stlist$Q2_Suspensions > 0 | stlist$Q3_Suspensions > 0 | stlist$Q4_Suspensions > 0), stlist$suspended <- T, stlist$suspended <- F)
  
  
  #This section creates a new variable, criteria, which calculates the number of eligibility criteria a student meets. #####
  
  
  
  
  
  
  
  stlist$`Q_1 criteria` <- 0
  stlist$`Q_2 criteria` <- 0
  stlist$`Q_3 criteria` <- 0
  stlist$`Q_4 criteria` <- 0
  stlist$criteria <- 0
  
  stlist$attend_criteria <- F
  stlist$beh_criteria <- F
  stlist$course_criteria <- F
  
  criteria_cats <- c(117:120)
  
  abs_cats <- 9
  
  susp_cats <- c(13)
  
  q1_subjects <- c("Q1_Math","Q1_Science","Q1_ELA")
  q2_subjects <- c("Q2_Math","Q2_Science","Q2_ELA")
  q3_subjects <- c("Q3_Math","Q3_Science","Q3_ELA")
  q4_subjects <- c("Q4_Math","Q4_Science","Q4_ELA")


  for(i in q1_subjects){

    stlist$`Q_1 criteria` <- ifelse(is.element(stlist$School, elem) & stlist$`Q_1 criteria` != 1 & (stlist[,i] <= 2 & !is.na(stlist[,i])), stlist$`Q_1 criteria`+ 1, stlist$`Q_1 criteria`)
    stlist$`Q_1 criteria` <- ifelse(is.element(stlist$School, high) & stlist$`Q_1 criteria` != 1 & (stlist[,i] < 70 & !is.na(stlist[,i])), stlist$`Q_1 criteria` + 1, stlist$`Q_1 criteria`)

  }


  for(i in q2_subjects){

    stlist$`Q_2 criteria` <- ifelse(is.element(stlist$School, elem) & stlist$`Q_2 criteria` != 1 & (stlist[,i] <= 2 & !is.na(stlist[,i])), stlist$`Q_2 criteria`+ 1, stlist$`Q_2 criteria`)
    stlist$`Q_2 criteria` <- ifelse(is.element(stlist$School, high) & stlist$`Q_2 criteria` != 1 & (stlist[,i] < 70 & !is.na(stlist[,i])), stlist$`Q_2 criteria` + 1, stlist$`Q_2 criteria`)

  }

  for(i in q3_subjects){

    stlist$`Q_3 criteria` <- ifelse(is.element(stlist$School, elem) & stlist$`Q_3 criteria` != 1 & (stlist[,i] <= 2 & !is.na(stlist[,i])), stlist$`Q_3 criteria`+ 1, stlist$`Q_3 criteria`)
    stlist$`Q_3 criteria` <- ifelse(is.element(stlist$School, high) & stlist$`Q_3 criteria` != 1 & (stlist[,i] < 70 & !is.na(stlist[,i])), stlist$`Q_3 criteria` + 1, stlist$`Q_3 criteria`)

  }

  for(i in q4_subjects){

    stlist$`Q_4 criteria` <- ifelse(is.element(stlist$School, elem) & stlist$`Q_4 criteria` != 1 & (stlist[,i] <= 2 & !is.na(stlist[,i])), stlist$`Q_4 criteria`+ 1, stlist$`Q_4 criteria`)
    stlist$`Q_4 criteria` <- ifelse(is.element(stlist$School, high) & stlist$`Q_4 criteria` != 1 & (stlist[,i] < 70 & !is.na(stlist[,i])), stlist$`Q_4 criteria` + 1, stlist$`Q_4 criteria`)

  }

  stlist$max_criteria <- pmax(stlist$`Q_1 criteria`, stlist$`Q_2 criteria`, stlist$`Q_3 criteria`, stlist$`Q_4 criteria`)


  stlist$course_criteria <- ifelse(stlist$max_criteria == 1, stlist$course_criteria <- T, stlist$course_criteria <- F)

  stlist$`Q_1 criteria` <- ifelse(stlist$`Q1_Suspensions` == 0 | is.na(stlist$`Q1_Suspensions`) , stlist$`Q_1 criteria`, stlist$`Q_1 criteria` + 1)
  stlist$`Q_2 criteria` <- ifelse(stlist$`Q2_Suspensions` == 0 | is.na(stlist$`Q2_Suspensions`) , stlist$`Q_2 criteria`, stlist$`Q_2 criteria` + 1)
  stlist$`Q_3 criteria` <- ifelse(stlist$`Q3_Suspensions` == 0 | is.na(stlist$`Q3_Suspensions`) , stlist$`Q_3 criteria`, stlist$`Q_3 criteria` + 1)
  stlist$`Q_4 criteria` <- ifelse(stlist$`Q4_Suspensions` == 0 | is.na(stlist$`Q4_Suspensions`) , stlist$`Q_4 criteria`, stlist$`Q_4 criteria` + 1)

  stlist$max_criteria <- pmax(stlist$`Q_1 criteria`, stlist$`Q_2 criteria`, stlist$`Q_3 criteria`, stlist$`Q_4 criteria`)


  stlist$beh_criteria <- ifelse(stlist$max_criteria == 2 | (stlist$max_criteria == 1 & stlist$course_criteria == F), stlist$beh_criteria <- T, stlist$beh_criteria <- F)


  stlist$`Q_1 criteria` <- ifelse(stlist$`Q1_Attendance Rate` > 90 | is.na(stlist$`Q1_Attendance Rate`), stlist$`Q_1 criteria`, stlist$`Q_1 criteria` + 1)
  stlist$`Q_2 criteria` <- ifelse(stlist$`Q2_Attendance Rate` > 90 | is.na(stlist$`Q2_Attendance Rate`), stlist$`Q_2 criteria`, stlist$`Q_2 criteria` + 1)
  stlist$`Q_3 criteria` <- ifelse(stlist$`Q3_Attendance Rate` > 90 | is.na(stlist$`Q3_Attendance Rate`), stlist$`Q_3 criteria`, stlist$`Q_3 criteria` + 1)
  stlist$`Q_4 criteria` <- ifelse(stlist$`Q4_Attendance Rate` > 90 | is.na(stlist$`Q4_Attendance Rate`), stlist$`Q_4 criteria`, stlist$`Q_4 criteria` + 1)


  stlist$max_criteria <- pmax(stlist$`Q_1 criteria`, stlist$`Q_2 criteria`, stlist$`Q_3 criteria`, stlist$`Q_4 criteria`)

  stlist$attend_criteria <- ifelse(stlist$max_criteria == 3 | (stlist$max_criteria == 1 & stlist$course_criteria == F & stlist$beh_criteria == F) |
                                     (stlist$max_criteria == 2 & (stlist$beh_criteria == F | stlist$course_criteria == F)), stlist$attend_criteria <- T, stlist$attend_criteria <- F)


  #Creates no-metric column
  stlist$no_metrics <- FALSE
  stlist$no_metrics_Q1 <- FALSE
  stlist$no_metrics_Q2 <- FALSE
  stlist$no_metrics_Q3 <- FALSE
  stlist$no_metrics_Q4 <- FALSE


  metrics_colums <- c("Q1_Science", "Q1_Math", "Q1_ELA","Q1_Suspensions", "Q1_Attendance Rate","Q2_Science", "Q2_Math", "Q2_ELA","Q2_Suspensions", "Q2_Attendance Rate","Q3_Science", "Q3_Math", "Q3_ELA","Q3_Suspensions", "Q3_Attendance Rate", "Q4_Science", "Q4_Math", "Q4_ELA","Q4_Suspensions", "Q4_Attendance Rate")
  
  stlist$no_metrics_Q1 <- (rowSums(is.na(stlist[, metrics_colums[1:5]])) > 1 )
  stlist$no_metrics_Q2 <- (rowSums(is.na(stlist[, metrics_colums[6:10]])) > 1 )
  stlist$no_metrics_Q3 <- (rowSums(is.na(stlist[, metrics_colums[11:15]])) > 1 )
  stlist$no_metrics_Q4 <- (rowSums(is.na(stlist[, metrics_colums[16:20]])) > 1)
  stlist$no_metrics <- apply(stlist[,66:69], 1, any)


# 
#   improve.math <- subset(stlist, stlist$School %in% high & (stlist$Q2_Math - stlist$Q1_Math) >= 10)
#   improve.math$improve_math <- T
#   #improve.math <- subset(improve.math, !is.na(improve.math$Name))
# 
#   improve.elm.math <- subset(stlist, stlist$School %in% elem & ((stlist$Q2_Math - stlist$Q1_Math) + (stlist$Q3_Math - stlist$Q2_Math)  >= 1.0 ))
#   improve.elm.math$improve_math <- T
#   #improve.elm.math <- subset(improve.elm.math, !is.na(improve.elm.math$Name))
# 
# 
#   improve.la <- subset(stlist, stlist$School %in% high & (stlist$Q2_ELA - stlist$Q1_ELA) >= 10)
#   improve.la$improve_la <- T
#   #improve.la <- subset(improve.la, !is.na(improve.la$Name))
# 
#   improve.elm.la <- subset(stlist, stlist$School %in% elem & (stlist$Q2_ELA - stlist$Q1_ELA) + (stlist$Q3_ELA - stlist$Q2_ELA)  >= 1.0 )
#   improve.elm.la$improve_la <- T
#   #improve.elm.la <- subset(improve.elm.la, !is.na(improve.elm.la$Name))
# 
#   improve.science <- subset(stlist, stlist$School %in% high & (stlist$Q2_Science - stlist$Q1_Science) >= 10)
#   improve.science$improve_science <- T
#   #improve.science <- subset(improve.science, !is.na(improve.science$Name))
# 
#   improve.elm.science <- subset(stlist, stlist$School %in% elem & (stlist$Q2_Science - stlist$Q1_Science) + (stlist$Q3_Science - stlist$Q2_Science)  >= 1.0 )
#   improve.elm.science$improve_science <- T
#   #improve.elm.science <- subset(improve.elm.science, !is.na(improve.elm.science$Name))
# 
# 
#   improve.elem.attend <- subset(stlist, stlist$School %in% elem  & ((stlist$`Q2_Attendance Rate` - stlist$`Q1_Attendance Rate`) + (stlist$`Q3_Attendance Rate` - stlist$`Q2_Attendance Rate`) >= 6))
#   #improve.elem.attend <- subset(improve.elem.attend, !is.na(improve.elem.attend$Name))
#   #elem.attend.eligible <- subset(stlist, stlist$Site %in% elem  &  (stlist$totabs1 > 3 | stlist$totabs2 > 3 | stlist$totabs3 > 3 | stlist$totabs4 > 3))
# 
#   improve.high.attend <- subset(stlist, stlist$School %in% high  & ((stlist$`Q2_Attendance Rate` - stlist$`Q1_Attendance Rate`) + (stlist$`Q3_Attendance Rate` - stlist$`Q2_Attendance Rate`) >= 10))
#   #improve.high.attend <- subset(improve.high.attend, !is.na(improve.high.attend$Name))
#   #high.attend.eligible <- subset(stlist, stlist$Site %in% high  &  (stlist$totabs1 > 5 | stlist$totabs2 > 5 | stlist$totabs3 > 5 | stlist$totabs4 > 5))

# 
#   improve.grades <- merge(improve.la, improve.science, all = TRUE)
#   improve.grades <- merge(improve.grades, improve.math, all= T)
#   improve.grades <- merge(improve.grades, improve.elm.science, all = T)
#   improve.grades <- merge(improve.grades, improve.elm.la, all = T)
#   improve.grades <- merge(improve.grades, improve.elm.math, all = T)
#   improve.grades$improve.grades <- TRUE
# 
#   improve.attend <- merge(improve.high.attend, improve.elem.attend, all = T)
#   improve.attend$attend <- TRUE
# 
#   stlist$improve_grades <- F
#   stlist$improve_math <- F
#   stlist$improve_science <- F
#   stlist$improve_ela <- F
#   stlist$improve_all_grades <- F
# 
#   stlist$improve_attend <- ifelse(stlist$Student.ID %in% improve.attend$Student.ID, stlist$improve.attend <- T, stlist$improve.attend <- F)
#   stlist$improve_grades <- ifelse(stlist$Student.ID %in% improve.grades$Student.ID, stlist$improve_grades <- T, stlist$improve_grades <- F)
#   stlist$improve_math <- ifelse(stlist$Student.ID %in% improve.grades[improve.grades$improve_math == T, "Student.ID"], stlist$improve_math <- T, stlist$improve_math <- F)
#   stlist$improve_science <- ifelse(stlist$Student.ID %in% improve.grades[improve.grades$improve_science == T, "Student.ID"], stlist$improve_science <- T, stlist$improve_science <- F)
#   stlist$improve_ela <- ifelse(stlist$Student.ID %in% improve.grades[improve.grades$improve_la == T, "Student.ID"], stlist$improve_ela <- T, stlist$improve_ela <- F)
#   stlist$improve_all_grades <- ifelse(stlist$improve_ela == T & stlist$improve_science == T & stlist$improve_math == T, stlist$improve_all_grades <- T, stlist$improve_all_grades <- F)
# 


  #Bottom flagging students by percentiles (1/3)



  # stlist$grade_quintile <- 2
  # 
  # 
  # 
  # # for (school in levels(stlist$School)){
  # for(row in 1:nrow(stlist)){
  #   if(!is.na(stlist[row,]$avg.grade.Q1) & stlist[row,]$avg.grade.Q1 < quantile(stlist[stlist$School == stlist[row,]$School,]$avg.grade.Q1, prob = 0.33, na.rm = T)){
  #     stlist[row,]$grade_quintile <- 1
  #   }
  #   else if(!is.na(stlist[row,]$avg.grade.Q1) & stlist[row,]$avg.grade.Q1 > quantile(stlist[stlist$School == stlist[row,]$School,]$avg.grade.Q1, prob = 0.67, na.rm = T)){
  #     stlist[row,]$grade_quintile <- 3
  #   }
  # 
  # }



  #   ifelse(stlist$School == school,
  #     ifelse(stlist$avg.grade.Q1 < quantile(stlist[stlist$School == school,]$avg.grade.Q1, prob = 0.01, na.rm = T), stlist$grade_quintile <- 1, stlist$grade_quintile),
  #     stlist$grade_quintile)
  # }


  
  return(stlist)

}