# miles to km is miles * 1.609344
library(lubridate)
library(data.table)
library(dplyr)
x <- '01/01 00'
x <- format(strptime(x, '%M/%d %H'), '%M/%d %I %p')
library(data.table)  
files <- list.files(path = "./data",pattern = ".csv")
for(i in 1:length(files)){
  files[i] = paste('./data',files[i],sep='/')
}
temp <- lapply(files, fread, sep=",")
data <- rbindlist( temp )
rm(temp)
gc()

data$`Trip Start Timestamp` <- ymd(data$`Trip Start Timestamp`)
