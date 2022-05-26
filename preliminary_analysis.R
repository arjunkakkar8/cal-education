library(tidyverse)
library(data.table)

absenteeism <- fread('https://www3.cde.ca.gov/demo-downloads/attendance/chrabs2021.txt') %>%
  mutate(cdscode = paste0(`County Code`, `District Code`, `School Code`))
census <- read.delim('https://www3.cde.ca.gov/researchfiles/cadashboard/censusenrollratesdownload2021.txt')
graduation <- read.delim('https://www3.cde.ca.gov/researchfiles/cadashboard/graddownload2021.txt')
career <- fread('https://www3.cde.ca.gov/researchfiles/cadashboard/ccidownload2021.txt')

enrollment <- fread('https://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear=2020-21&cCat=Enrollment&cPage=filesenr.asp')
