options(java.parameters = "-Xmx1024m")

library(plyr) # ****IMPORTANT**** Load plyr before dplyr- they have some of the same named functions, and if you load in a different order it will cause problems
library(dplyr)
library(tidyr)
# library(XLConnect)
library(lubridate)
library(shiny)
library(openxlsx)
library(rsconnect)

#Setting Vectors that will be used throughout program
metrics <- c("Math","Science","ELA", "Suspensions", "Attendance Rate")

elem <- c("Glenn Elementary School", "Eno Valley Elementary", "EK Powe Elementary School", "Merrick-Moore")
high <- c("Shepard", "Durham Performance Learning Center", "Hillside High School", "Southern High School", "Northern")


runApp()

