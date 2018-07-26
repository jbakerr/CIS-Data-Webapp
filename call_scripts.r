########################### Call Script ########################################

# Set Up Environment -----------------------------------------------------------

library(markdown)
library(plyr) 
library(dplyr)
library(lubridate)
library(shiny)
library(rsconnect)
library(readxl)

# Source Scripts ---------------------------------------------------------------
setwd('~/Code/cis_webapp')

suppressWarnings(source("scripts/tier1.r", local = T))
suppressWarnings(source("scripts/site_coordination.r", local = T))
suppressWarnings(source('scripts/services.r', local= T))
suppressWarnings(source('scripts/caselist.r', local = T))
suppressWarnings(source('scripts/progress_monitoring.r', local = T))
suppressWarnings(source('scripts/studentlist.r', local = T))
suppressWarnings(source('scripts/service_prep.r', local = T))
suppressWarnings(source('scripts/functions.r', local = T))


# Reset Working Directory -----------------------------------------------------

setwd("/Volumes/GoogleDrive/Team Drives/Data/CISDM Files")


# Caselist ---------------------------------------------------------------------

nms <- names(read_excel('caselist.xlsx', n_max = 0))


ct <- ifelse(grepl("Date", nms), "date", "guess")

caselist <- suppressWarnings(
  read_excel('caselist.xlsx', sheet = 1,skip = 1, col_types = ct)
  )

caselist <- caselist_script(caselist)

# Progress Monitoring ----------------------------------------------------------

nms <- names(read_excel('progress.xlsx', n_max = 0))


ct <- ifelse(grepl("Date", nms), "date", "guess")

progress <- suppressWarnings(
  read_excel('progress.xlsx', sheet = 1,skip = 0, col_types = ct)
  )


progress <- progress_import_script(progress)

# Services ---------------------------------------------------------------------

nms <- names(read_excel('services.xlsx', n_max = 0))


ct <- ifelse(grepl("Date", nms), "date", "guess")

services <- suppressWarnings(
  read_excel('services.xlsx', sheet = 1,skip = 1, col_types = ct)
  )


services <- service_script(services)


# Site Coordination ------------------------------------------------------------
nms <- names(read_excel('site_coordination.xlsx', n_max = 0))


ct <- ifelse(grepl("Date", nms), "date", "guess")

site_coordination <- suppressWarnings(
  read_excel('site_coordination.xlsx', sheet = 1,skip = 0, col_types = ct)
  )



site_coordination <- site_coordination_script(site_coordination_df)


# Tier 1 -----------------------------------------------------------------------
nms <- names(read_excel('tier1.xlsx', n_max = 0))

ct <- ifelse(grepl("Date", nms), "date", "guess")

tier1 <- suppressWarnings(
  read_excel('tier1.xlsx', sheet = 1,skip = 1, col_types = ct)
  )

tier1 <- tier1_script(tier1_df)


# Student List Creation --------------------------------------------------------


studentlist <- studentlist_creation(caselist, progress, services)

# Saving Files -----------------------------------------------------------------

setwd('/Volumes/GoogleDrive/Team Drives/Data/Generated Files')

write.csv(studentlist, 'studentlist.csv')

write.csv(site_coordination, 'site_coordination.csv')

write.csv(tier1, 'tier1.csv')

write.csv(services, 'services.csv')
