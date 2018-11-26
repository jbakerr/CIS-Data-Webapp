subseted_services <- select(
  filter(services, Home.School == 'Glenn Elementary School' & between(Support.Date, today() - weeks(4), today())),
  c(Student.ID, Student.Support.Category, Student.Support.Name, hoursspent, Support.Date)
)

eos_rows <- c('Students Served', 'Hours Delivered', 'Parent Services')

subseted_services$week <- floor_date(as.Date(subseted_services$Support.Date, "%m/%d/%Y"), unit="week")+1

eos_columns <- unique(subseted_services$week)

eos_table <- as.data.frame(matrix(,ncol = length(eos_columns), nrow = 3 ))
rownames(eos_table) <- eos_rows
colnames(eos_table) <- eos_columns


for(i in seq(1,length(eos_columns))){

  eos_table[1,i] <- length(unique(subseted_services[subseted_services$week == eos_columns[i], "Student.ID"]))
  
  eos_table[2,i] <- sum(subseted_services[subseted_services$week == eos_columns[i], ]$hoursspent)
  
  eos_table[3,i] <- nrow(subseted_services[subseted_services$week == eos_columns[i] & subseted_services$Student.Support.Name == 
                                             'Home Visit/Parent/Care Giver Contact' , ])
}



################################################################################
services$Support.Date <- as.Date(services$Support.Date)


subseted_services <- select(
  filter(services, Home.School == 'Glenn Elementary School' & between(Support.Date, today() - weeks(2), today())),
  c(Student.Support.Category, Student.Support.Name, hoursspent, Support.Date)
)

subseted_services[between(subseted_services$Support.Date, today() - weeks(2), today()),]

subseted_services$Support.Date <- as.Date(subseted_services$Support.Date)


subseted_services[subseted_services$Support.Date >= (today() - weeks(2)),]



service_list <- getData_services()
service_list$Support.Date <- as.Date(service_list$Support.Date)
subseted_services <- select(
  filter(services, Home.School == 'Glenn Elementary School' & between(Support.Date, today() - weeks(2), today())),
  c(Student.Support.Category, Student.Support.Name, hoursspent, Support.Date)
)
service_table <- aggregate(
  subseted_services$hoursspent, 
  by=list(Category=subseted_services$Student.Support.Name), FUN=sum
)

service_table



### 


subseted_services <- select(
  filter(services, Home.School == 'Glenn Elementary School' & between(Support.Date, today() - weeks(4), today())),
  c(Student.ID, Student.Support.Category, Student.Support.Name, hoursspent, Support.Date)
)

eos_rows <- c('Students Served', 'Hours Delivered', 'Parent Services')

subseted_services$week <- floor_date(as.Date(subseted_services$Support.Date, "%m/%d/%Y"), unit="week")+1

eos_columns <- unique(subseted_services$week)



eos_table <- as.data.frame(matrix(,ncol = length(eos_columns), nrow = 3 ))
rownames(eos_table) <- eos_rows
colnames(eos_table) <- eos_columns


for(i in range(1:length(eos_columns))){
  
  eos_table[1,i] <- length(unique(subseted_services[subseted_services$week == eos_columns[i], "Student.ID"]))
  
  eos_table[2,i] <- sum(subseted_services[subseted_services$week == eos_columns[i], ]$hoursspent)
  
  eos_table[3,i] <- nrow(subseted_services[subseted_services$week == eos_columns[i] & subseted_services$Student.Support.Name == 
                                             'Home Visit/Parent/Care Giver Contact' , ])
}



eos_table
