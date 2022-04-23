# miles to km is miles * 1.609344
library(lubridate)
library(data.table)
library(dplyr)

library(leaflet)
library(scales)
library(data.table)  
library(dplyr)
library(rgdal)
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
df <- group_by(data,`Trip Start Timestamp`) %>% summarise(rides = length(`Trip Seconds`))
colnames(df) = c("date","rides")
df$date = substr(df$date,5,6)
df <- group_by(df,date) %>% summarise(rides = sum(rides))
colnames(df) = c("date","rides")
df$date <-c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec')
df$date <- factor(df$date,levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec'))

df <- data.frame(data$`Trip Seconds`)
colnames(df) = c("seconds")
df <- df %>% mutate(bin = cut(seconds, breaks=c(59,180,360,600,900,1800,3600,5400,7200,10800,18000)))
df<- group_by(df,bin) %>% summarise(rides = length(seconds))
colnames(df) = c("seconds","rides")
df$seconds <- c('(59,180]','(180,360]','(360,600]','(600,900]','(900,1800]','(1800,3600]','(3600,5400]','(5400,7200]','(7200,10800]','(10800,18000]')
df$seconds <- factor(df$seconds,levels =  c('(59,180]','(180,360]','(360,600]','(600,900]','(900,1800]','(1800,3600]','(3600,5400]','(5400,7200]','(7200,10800]','(10800,18000]'))

x <- shapeData$community 
`test var` <- t
x <- 'test'
test<- 5
get(x)

data[data$`Dropoff Community Area` == 4]

x <- paste(' ','23','42')

mypal <- colorQuantile(palette = "RdYlBu", domain = c(0,50), n = 5, reverse = TRUE)
LtoM <-colorRampPalette(c('red', 'red' ))
Mid <- "snow3"
MtoH <-colorRampPalette(c('green', 'darkgreen'))

scale_fill_gradient2(low=LtoM(100), mid='snow3', high=MtoH(100)
                     , space='Lab')


df <- data.frame(c('test'),c('test2'))
df[nrow(df) + 1,] <- c('2','5')

comAreaList <- c('ROGERS_PARK','WEST_RIDGE','UPTOWN','LINCOLN_SQUARE','NORTH_CENTER','LAKE_VIEW','LINCOLN_PARK','NEAR_NORTH_SIDE','EDISON_PARK','NORWOOD_PARK','JEFFERSON_PARK','FOREST_GLEN','NORTH_PARK','ALBANY_PARK','PORTAGE_PARK','IRVING_PARK','DUNNING','MONTCLARE','BELMONT_CRAGIN','HERMOSA','AVONDALE','LOGAN_SQUARE','HUMBOLDT_PARK','WEST_TOWN','AUSTIN','WEST_GARFIELD_PARK','EAST_GARFIELD_PARK','NEAR_WEST_SIDE','NORTH_LAWNDALE','SOUTH_LAWNDALE','LOWER_WEST_SIDE','LOOP','NEAR_SOUTH_SIDE','ARMOUR_SQUARE','DOUGLAS','OAKLAND','FULLER_PARK','GRAND_BOULEVARD','KENWOOD','WASHINGTON_PARK','HYDE_PARK','WOODLAWN','SOUTH_SHORE','CHATHAM','AVALON_PARK','SOUTH_CHICAGO','BURNSIDE','CALUMET_HEIGHTS','ROSELAND','PULLMAN','SOUTH_DEERING','EAST_SIDE','WEST_PULLMAN','RIVERDALE','HEGEWISCH','GARFIELD_RIDGE','ARCHER_HEIGHTS','BRIGHTON_PARK','MCKINLEY_PARK','BRIDGEPORT','NEW_CITY','WEST_ELSDON','GAGE_PARK','CLEARING','WEST_LAWN','CHICAGO_LAWN','WEST_ENGLEWOOD','ENGLEWOOD','GREATER_GRAND_CROSSING','ASHBURN','AUBURN_GRESHAM','BEVERLY','WASHINGTON_HEIGHTS','MOUNT_GREENWOOD','MORGAN_PARK','OHARE','EDGEWATER')
library(dplyr)
df <- data[data$`Pickup Community Area` == 1]
df <- group_by(df,`Dropoff Community Area`) %>% summarise(perc = length(`Pickup Community Area`))
colnames(df) = c('area','perc')
colsum <- sum(df$perc)
df$perc <- df$perc/colsum
df['area'] = lapply(df['area'],function(x) comAreaList[x])
if(length(df['area']) < 77){
  area <- c()
  perc <- c()
  for (e in comAreaList){
    if(any(df['area']==e)){
      
    }
    else{
      area <- append(area,e)
      perc <- append(perc,0)
    }
  }
  
  dt <- data.table(area,perc)
  df <- bind_rows(dt,df)
}
colnames(df)<- c('community','perc')
dt <- shapeData

dt@data <- inner_join(df,dt@data,by='community')

df <-df[df$area == "MOUNT_GREENWOOD"]
comAreaList == dif
