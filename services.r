service_script <- function(data){
  
  # This is the name I have been using for the detailed service dataset, however we may want to start using a different naming convention
  # This is the "Tier II and III Support Detail" report. 
  # **** IMPORTANT ***** delete the last row that sums all hours before saving the excel file. That row will cause problems
  #data<-('services.csv', skip = 4, header = T)
  # xlsxFile <- system.file("/Volumes/GoogleDrive/My Drive/Data Files/services.xlsx", package = "openxlsx")
  data$Entry.Date <- convertToDate(data$Entry.Date)
  data$Support.Date <- convertToDate(data$Support.Date)
  
  data <- data.frame(apply(data, 2, function(x) gsub("^$|^ $", NA, x)))
  
  data  <- data[,colSums(is.na(data))<nrow(data)]
  
  # # origin = as.Date("1900-01-01", "%Y-%m-%d", origin = '2016-01-01')
  # data$Entry.Date <- convertToDateTime(data$Entry.Date, origin = '2017-08-09', tx = "EST")
  # data$Support.Date <- as.Date(data$Support.Date, origin = "1900-01-01")
  
  #data<-readWorksheetFromFile('services.xlsx', sheet=1, header = T, startRow = 10)
  colnames(data)[1:2] <- c("Home.School","Student.ID")
  data <- data[!is.na(data$Student.ID), ]# get rid of accidental blank rows
  #data <- data[-nrow(data),] #removes summation row
  
  
  #removing Nina's test data
  #data <- subset(data, !data$Entered.By == "nina")
  drops <- c("Provider.Type.2","ProviderUserType3", "Donation.Value", "Total.Value.of.Time")
  data <- data[, ! (names(data) %in% drops)]
  
  
  d <- data %>% group_by(Home.School, Entry.Date, Support.Date, Provider.Type.1, Activity, Student.Support.Category, 
                         Hours, Tier) %>% summarize(groupsize = n())
  
  
  #d$groupsize[(!is.na(d$Individual.or.Group)) & d$Individual.or.Group == "Individual"] <- 1
  
  data <- merge(data, d, by = c("Home.School", "Entry.Date", "Support.Date", "Student.Support.Category",
                                "Hours", "Tier", "Activity"))
  
  data_test <- data
  as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
  
  data$Hours <- as.numeric.factor(data$Hours)
  # data$groupsize <- as.numeric.factor(data$groupsize)
  
  
  data$hoursspent <- data$Hours/data$groupsize
  
  return(data)

}