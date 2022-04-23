library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(data.table)  
library(dplyr)
library(rgdal)

files <- list.files(path = "./data",pattern = ".csv")
for(i in 1:length(files)){
  files[i] = paste('./data',files[i],sep='/')
}

temp <- lapply(files, fread, sep=",")
data <- rbindlist( temp )
rm(temp)
shapeData <- readOGR('./data/geo_export.shp')
shapeData <- spTransform(shapeData, CRS("+proj=longlat +ellps=GRS80"))
shapeData$community <- gsub(" ", "_", shapeData$community)

gc()
options(scipen=10000)

DOUGLAS<-35
OAKLAND<-36
FULLER_PARK<-37
GRAND_BOULEVARD<-38
KENWOOD<-39
LINCOLN_SQUARE<-4
WASHINGTON_PARK<-40
HYDE_PARK<-41
WOODLAWN<-42
ROGERS_PARK<-1
JEFFERSON_PARK<-11
FOREST_GLEN<-12
NORTH_PARK<-13
ALBANY_PARK<-14
PORTAGE_PARK<-15
IRVING_PARK<-16
DUNNING<-17
MONTCLARE<-18
BELMONT_CRAGIN<-19
WEST_RIDGE<-2
HERMOSA<-20
AVONDALE<-21
LOGAN_SQUARE<-22
HUMBOLDT_PARK<-23
WEST_TOWN<-24
AUSTIN<-25
WEST_GARFIELD_PARK<-26
EAST_GARFIELD_PARK<-27
NEAR_WEST_SIDE<-28
NORTH_LAWNDALE<-29
UPTOWN<-3
SOUTH_LAWNDALE<-30
LOWER_WEST_SIDE<-31
NEAR_SOUTH_SIDE<-33
ARMOUR_SQUARE<-34
NORWOOD_PARK<-10
NEAR_NORTH_SIDE<-8
LOOP<-32
SOUTH_SHORE<-43
CHATHAM<-44
AVALON_PARK<-45
SOUTH_CHICAGO<-46
BURNSIDE<-47
MCKINLEY_PARK<-59
LAKE_VIEW<-6
CALUMET_HEIGHTS<-48
ROSELAND<-49
NORTH_CENTER<-5
PULLMAN<-50
SOUTH_DEERING<-51
EAST_SIDE<-52
WEST_PULLMAN<-53
RIVERDALE<-54
HEGEWISCH<-55
GARFIELD_RIDGE<-56
ARCHER_HEIGHTS<-57
BRIGHTON_PARK<-58
BRIDGEPORT<-60
NEW_CITY<-61
WEST_ELSDON<-62
GAGE_PARK<-63
CLEARING<-64
WEST_LAWN<-65
CHICAGO_LAWN<-66
WEST_ENGLEWOOD<-67
ENGLEWOOD<-68
GREATER_GRAND_CROSSING<-69
LINCOLN_PARK<-7
ASHBURN<-70
AUBURN_GRESHAM<-71
BEVERLY<-72
WASHINGTON_HEIGHTS<-73
MOUNT_GREENWOOD<-74
MORGAN_PARK<-75
OHARE<-76
EDGEWATER<-77
EDISON_PARK<-9
City_of_Chicago <- 78

comAreaList <- c('ROGERS_PARK','WEST_RIDGE','UPTOWN','LINCOLN_SQUARE','NORTH_CENTER','LAKE_VIEW','LINCOLN_PARK','NEAR_NORTH_SIDE','EDISON_PARK','NORWOOD_PARK','JEFFERSON_PARK','FOREST_GLEN','NORTH_PARK','ALBANY_PARK','PORTAGE_PARK','IRVING_PARK','DUNNING','MONTCLARE','BELMONT_CRAGIN','HERMOSA','AVONDALE','LOGAN_SQUARE','HUMBOLDT_PARK','WEST_TOWN','AUSTIN','WEST_GARFIELD_PARK','EAST_GARFIELD_PARK','NEAR_WEST_SIDE','NORTH_LAWNDALE','SOUTH_LAWNDALE','LOWER_WEST_SIDE','LOOP','NEAR_SOUTH_SIDE','ARMOUR_SQUARE','DOUGLAS','OAKLAND','FULLER_PARK','GRAND_BOULEVARD','KENWOOD','WASHINGTON_PARK','HYDE_PARK','WOODLAWN','SOUTH_SHORE','CHATHAM','AVALON_PARK','SOUTH_CHICAGO','BURNSIDE','CALUMET_HEIGHTS','ROSELAND','PULLMAN','SOUTH_DEERING','EAST_SIDE','WEST_PULLMAN','RIVERDALE','HEGEWISCH','GARFIELD_RIDGE','ARCHER_HEIGHTS','BRIGHTON_PARK','MCKINLEY_PARK','BRIDGEPORT','NEW_CITY','WEST_ELSDON','GAGE_PARK','CLEARING','WEST_LAWN','CHICAGO_LAWN','WEST_ENGLEWOOD','ENGLEWOOD','GREATER_GRAND_CROSSING','ASHBURN','AUBURN_GRESHAM','BEVERLY','WASHINGTON_HEIGHTS','MOUNT_GREENWOOD','MORGAN_PARK','OHARE','EDGEWATER')

com <- c('24 Seven Taxi','312 Medallion Mgmt.','5 Star Taxi','Adwar H. Nikola','Ahzmi','American United','American United Taxi','Arrington Ent.','Babylon Express','Benny Jona','Blue Diamond','Blue Ribbon Taxi','Checker Taxi','Chicago Carriage Cab','Chicago Ind.','Chicago Medallion Mgmt.','Chicago Star Taxicab','Chicago Taxicab','Choice Taxi','Chuks Cab','City Svc.','David K. Cab','Flash Cab','G.L.B. Cab','Globe Taxi','Gold Coast Taxi','JBL Cab.','Jay Kim','KOAM Taxi','Leonard Cab','Luhak','Medallion Leasin','Metro Jet Taxi A','N and W Cab','Nova Taxi','Omar Jada','Patriot Taxi Dba Peace Taxi','Petani Cab','RC Andrews Cab','Salifu Bawa','Sam Mestas','Santamaria Express','Sbeih','Sergey Cab','Setare','Star North Mgmt.','Sun Taxi','Tasha ride','Taxi Aff. Svc. Yellow','Taxi Aff. Svcs.','Taxicab Ins. Agcy.','Top Cab Aff.','U Taxicab','Yellow Cab')
# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Spring 2022 Project 3"),
  
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
                   
                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL)
                   ),
                   #Options for right graphs
                   menuItem("Formats",
                            selectInput("timeFormat", "Time", c('12 hour','24 hour'), selected = "24 hour"),
                            selectInput("distUnit", "Distance Unit", c('mile','km'), selected = "mile"),
                            selectInput("table",'Table',c('Graph','Table'),selected = 'Graph')
                   ),
                   menuItem("Select options",
                            
                            selectInput("parts",'Parts',c('Default','Community','Company'),selected = 'Default'),
                            conditionalPanel(
                              condition = "input.parts == 'Company'",
                            selectInput("compNames", "Company", append(com,'All Taxi Companies'), selected = "5 Star Taxi")
                            ),
                            conditionalPanel(
                              condition = "input.parts == 'Community'",
                            selectInput("comArea","Community Area", append(sort(shapeData$community),'City_of_Chicago'), selected = 'DOUGLAS')
                            ),
                            conditionalPanel(
                              condition = "input.parts != 'Default'",
                            selectInput("tofrom", "To/From", c("To","From"),selected ='To')
                            )
                   ),
                   
                   #Option to change page to about section
                   menuItem("Page options",
                            selectInput("pageOption", "Select page", c("Data","About"), selected = "Data")
                   )
  ),
  
  dashboardBody(
    #Data page, only show when page option has data selected
    conditionalPanel(
      condition = "input.pageOption == 'Data'",
      fluidRow(
        column(3,
               fluidRow(
                 verticalLayout(
                 uiOutput("perc_graph",width='100%'),
                 leafletOutput("leaflet")
                 )
               )
               
        ),
        column(3,
          fluidRow(
            verticalLayout(
              uiOutput("all_rides_hour_day",width="100%"),
              uiOutput("all_rides_year_day",width="100%")
            )
          )
        ),
        column(3,
               fluidRow(
                 verticalLayout(
                   uiOutput("all_rides_weekday",width="100%"),
                   uiOutput("all_rides_monthly",width="100%")
                 )
               )
               
        ),
        column(3,
               fluidRow(
                 verticalLayout(
                   uiOutput("all_binned_mileage",width="100%"),
                   uiOutput("all_trip_time",width="100%")
                 )
               )
               
        )
        
      )
    ),
    #About page, only show when page option about is selected
    conditionalPanel(
      condition = "input.pageOption == 'About'",
      fluidRow(
        h1("About Page"),
        p("The data is from https://data.cityofchicago.org/Transportation/Taxi-Trips-2019/h4cq-z3dy"),
        p("and boundary data is from https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6"),
        p("Vivek Patel wrote this application."),
        p("Created: Spring 2022, April"),
        p("The application was created for Project 3 of Spring 2022 CS 424 with Dr. Johnson")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  # timeFormat c('12 hour','24 hour'), distUnit c('mile','km'), compNames c('All',...)
  communityArea <- reactive({
    return(get(input$comArea))
  })
    rides_year_day <- reactive({
      if(input$parts == 'Default' | (communityArea() == 78 &  input$parts != 'Company') | (input$parts == 'Company' & input$compNames == 'All Taxi Companies' )){
        df <- group_by(data,`Trip Start Timestamp`) %>% summarise(rides = length(`Trip Seconds`))
        colnames(df) = c("date","rides")
        df$date = ymd(df$date)
        return(df)
      }
      else if(input$parts == 'Community'){
        if(input$tofrom == 'To'){
          df <- data[data$`Dropoff Community Area` == communityArea()]
          df <- group_by(df,`Trip Start Timestamp`) %>% summarise(rides = length(`Dropoff Community Area`))
          colnames(df) = c("date","rides")
          df$date = ymd(df$date)
          return(df)
        }
        else if(input$tofrom == 'From'){
          df <- data[data$`Pickup Community Area` == communityArea()]
          df <- group_by(df,`Trip Start Timestamp`) %>% summarise(rides = length(`Pickup Community Area`))
          colnames(df) = c("date","rides")
          df$date = ymd(df$date)
          return(df)
        }
      }
      else if(input$parts == 'Company'){
        if(input$tofrom == 'To'){
          df <- data[data$Company == input$compNames]
          df <- group_by(df,`Trip Start Timestamp`) %>% summarise(rides = length(`Dropoff Community Area`))
          colnames(df) = c("date","rides")
          df$date = ymd(df$date)
          return(df)
        }
        else if(input$tofrom == 'From'){
          df <- data[data$Company == input$compNames]
          df <- group_by(df,`Trip Start Timestamp`) %>% summarise(rides = length(`Pickup Community Area`))
          colnames(df) = c("date","rides")
          df$date = ymd(df$date)
          return(df)
        }
      }
    })
    
    output$all_rides_year_day <- renderUI({
      df <- rides_year_day()
      verticalLayout(
      conditionalPanel(
        condition = "input.table == 'Graph'",
        verticalLayout(
          renderPlot({
          ggplot(df, aes(x=date,y=rides)) + geom_bar( stat='identity', fill='steelblue') +
            labs(x="Day", y="Rides")+ scale_y_continuous(label=comma) +ggtitle(label = 'Ridership for each day')
          })
        )
      ),
      conditionalPanel(
        condition = "input.table == 'Table'",
        box(DT::renderDataTable(
          df,
          options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE
        )
        )
      )
      )
    })
    
    rides_hour_day <- reactive({
      if(input$parts == "Default" | (communityArea() == 78 & input$parts != 'Company') | (input$parts == 'Company' & input$compNames == 'All Taxi Companies' )){
        df <- group_by(data,Hour) %>% summarise(rides = length(`Trip Seconds`))
        return(df)
      }
      else if(input$parts == 'Community'){
        if(input$tofrom == 'To'){
          df <- data[data$`Dropoff Community Area` == communityArea()]
          df <- group_by(df,Hour) %>% summarise(rides = length(`Trip Seconds`))
          return(df)
        }
        else if(input$tofrom == 'From'){
          df <- data[data$`Pickup Community Area` == communityArea()]
          df <- group_by(df,Hour) %>% summarise(rides = length(`Trip Seconds`))
          return(df)
        }
        
      }
      
      else if(input$parts == 'Company'){
        if(input$tofrom == 'To'){
          df <- data[data$Company == input$compNames]
          df <- group_by(df,Hour) %>% summarise(rides = length(`Trip Seconds`))
          return(df)
        }
        else if(input$tofrom == 'From'){
          df <- data[data$Company == input$compNames]
          df <- group_by(df,Hour) %>% summarise(rides = length(`Trip Seconds`))
          return(df)
        }
      }
    })
    
    timeform <- reactive({
      df <- rides_hour_day()
      colnames(df) = c("hour","rides")
      if(input$timeFormat == '12 hour'){ #add 0's
        ampm <- c('12 AM', '1 AM', '2 AM', '3 AM', '4 AM', '5 AM', '6 AM', '7 AM', '8 AM', '9 AM', '10 AM', '11 AM','12 PM', '1 PM', '2 PM', '3 PM', '4 PM', '5 PM', '6 PM', '7 PM', '8 PM', '9 PM', '10 PM', '11 PM' )
        df$hour <- lapply(df$hour, function(x) ampm[x+1])
        df$hour <- factor(df$hour,levels =  ampm)
        return(df)
      }
      else{
        timeasstr <- c('0','1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23')
        return(df)
      }
    })

    output$all_rides_hour_day <- renderUI({
      df <- timeform()
      verticalLayout(
        conditionalPanel(
          condition = "input.table == 'Graph'",
          verticalLayout(
          renderPlot({
            ggplot(df, aes(x=hour,y=rides)) + geom_bar( stat='identity', fill='steelblue') +
              labs(x="Hour", y="Rides")+scale_y_continuous(label=comma) +ggtitle(label = 'Ridership for each hour') + scale_x_discrete(guide = guide_axis(angle = 90)) 
          })
          )
        ),
        conditionalPanel(
          condition = "input.table == 'Table'",
          box(DT::renderDataTable(
            df,
            options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(1, 'desc'))
            ), rownames = FALSE
          )
          )
        )
      
      )
      
    })
    
    rides_weekday <- reactive({
      if(input$parts == "Default" | (communityArea() == 78 & input$parts != 'Company') | (input$parts == 'Company' & input$compNames == 'All Taxi Companies' )){
        df <- group_by(data,`Trip Start Timestamp`) %>% summarise(rides = length(`Trip Seconds`))
        colnames(df) = c("date","rides")
        df$date <- ymd(df$date)
        df$date<- lubridate::wday(df$date,abbr = TRUE, label = TRUE)
        colnames(df) = c("weekday","rides")
        return(df)
      }
      else if(input$parts == 'Community'){
        if(input$tofrom == 'To'){
          df <- data[data$`Dropoff Community Area` == communityArea()]
          df <- group_by(df,`Trip Start Timestamp`) %>% summarise(rides = length(`Trip Seconds`))
          colnames(df) = c("date","rides")
          df$date <- ymd(df$date)
          df$date<- lubridate::wday(df$date,abbr = TRUE, label = TRUE)
          colnames(df) = c("weekday","rides")
          return(df)
          
        }
        else if(input$tofrom == 'From'){
          df <- data[data$`Pickup Community Area` == communityArea()]
          df <- group_by(df,`Trip Start Timestamp`) %>% summarise(rides = length(`Trip Seconds`))
          colnames(df) = c("date","rides")
          df$date <- ymd(df$date)
          df$date<- lubridate::wday(df$date,abbr = TRUE, label = TRUE)
          colnames(df) = c("weekday","rides")
          return(df)
        }
      }
      
      else if(input$parts == 'Company'){
        if(input$tofrom == 'To'){
          df <- data[data$Company == input$compNames]
          df <- group_by(df,`Trip Start Timestamp`) %>% summarise(rides = length(`Trip Seconds`))
          colnames(df) = c("date","rides")
          df$date <- ymd(df$date)
          df$date<- lubridate::wday(df$date,abbr = TRUE, label = TRUE)
          colnames(df) = c("weekday","rides")
          return(df)
        }
        else if(input$tofrom == 'From'){
          df <- data[data$Company == input$compNames]
          df <- group_by(df,`Trip Start Timestamp`) %>% summarise(rides = length(`Trip Seconds`))
          colnames(df) = c("date","rides")
          df$date <- ymd(df$date)
          df$date<- lubridate::wday(df$date,abbr = TRUE, label = TRUE)
          colnames(df) = c("weekday","rides")
          return(df)
        }
      }
    })
    

    output$all_rides_weekday <- renderUI({
      df <- rides_weekday()
      verticalLayout(
        conditionalPanel(
          condition = "input.table == 'Graph'",
          renderPlot({
            ggplot(df, aes(x=weekday,y=rides)) + geom_bar( stat='identity', fill='steelblue') +
              labs(x="Day", y="Rides")+ scale_y_continuous(label=comma) +ggtitle(label = 'Ridership for each week day')
            
            
          })
        ),
        conditionalPanel(
          condition = "input.table == 'Table'",
          box(DT::renderDataTable(
            df,
            options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(1, 'desc'))
            ), rownames = FALSE
          )
          )
        )
      )
    })

    rides_monthly <- reactive({
      if(input$parts == "Default" | (communityArea() == 78 & input$parts != 'Company') | (input$parts == 'Company' & input$compNames == 'All Taxi Companies' )){
        df <- group_by(data,`Trip Start Timestamp`) %>% summarise(rides = length(`Trip Seconds`))
        colnames(df) = c("date","rides")
        df$date = substr(df$date,5,6)
        df <- group_by(df,date) %>% summarise(rides = sum(rides))
        colnames(df) = c("date","rides")
        df$date <-c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec')
        df$date <- factor(df$date,levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec'))
        return(df)
      }
      else if(input$parts == 'Community'){
        if(input$tofrom == 'To'){
          df <- data[data$`Dropoff Community Area` == communityArea()]
          df <- group_by(df,`Trip Start Timestamp`) %>% summarise(rides = length(`Trip Seconds`))
          colnames(df) = c("date","rides")
          df$date = substr(df$date,5,6)
          df <- group_by(df,date) %>% summarise(rides = sum(rides))
          colnames(df) = c("date","rides")
          df$date <-c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec')
          df$date <- factor(df$date,levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec'))
          return(df)
        }
        else if(input$tofrom == 'From'){
          df <- data[data$`Pickup Community Area` == communityArea()]
          df <- group_by(df,`Trip Start Timestamp`) %>% summarise(rides = length(`Trip Seconds`))
          colnames(df) = c("date","rides")
          df$date = substr(df$date,5,6)
          df <- group_by(df,date) %>% summarise(rides = sum(rides))
          colnames(df) = c("date","rides")
          df$date <-c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec')
          df$date <- factor(df$date,levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec'))
          return(df)
        }
        
      }
      
      else if(input$parts == 'Company'){
          df <- data[data$Company == input$compNames]
          df <- group_by(df,`Trip Start Timestamp`) %>% summarise(rides = length(`Trip Seconds`))
          colnames(df) = c("date","rides")
          df$date <- ymd(df$date)
          df$date<- lubridate::month(df$date,abbr = TRUE, label = TRUE)
          df <- group_by(df,date) %>% summarise(rides = sum(rides))
          colnames(df) = c("date","rides")
          return(df)
      }
    })

    output$all_rides_monthly <- renderUI({
      df <- rides_monthly()
      verticalLayout(
        conditionalPanel(
          condition = "input.table == 'Graph'",
          renderPlot({
            ggplot(df, aes(x=date,y=rides)) + geom_bar( stat='identity', fill='steelblue') +
              labs(x="Month", y="Rides")+ scale_y_continuous(label=comma) +ggtitle(label = 'Ridership for each month')
            
          })
          
        ),
        conditionalPanel(
          condition = "input.table == 'Table'",
          box(DT::renderDataTable(
            df,
            options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(1, 'desc'))
            ), rownames = FALSE
          )
          )
        )
      )
    })
    
    binned_mileage_parts <- reactive({
      if(input$parts == 'Default' | (communityArea() == 78 & input$parts != 'Company') | (input$parts == 'Company' & input$compNames == 'All Taxi Companies' )){
        return(data)
      }
      else if(input$parts == 'Community' ){
        if(input$tofrom == 'To'){
          df <- data[data$`Dropoff Community Area` == communityArea()]
          return(df)
        }
        else if(input$tofrom == 'From'){
          df <- data[data$`Pickup Community Area` == communityArea()]
          return(df)
        }
      }
      
      else if(input$parts == 'Company'){
        df <- data[data$Company == input$compNames]
        return(df)
      }
      
    })
    mileagedf <- reactive({
      df<-binned_mileage_parts()
      
      if(input$distUnit == 'mile')
      {
        df <- data.table(df$`Trip Miles`)
        colnames(df) = c("miles")
        df <- df %>% mutate(bin = cut(miles, breaks=c(.49,1,1.5,2,2.5,3,4,5,10,15,20,25,50,75,100)))
        df <- group_by(df,bin) %>% summarise(rides = length(miles))
        colnames(df) = c("unit_dist","rides")
        return(df)
      }
      else{
        df <- data.table(df$`Trip Miles`)
        colnames(df) = c("miles")
        df$miles <- df$miles * 1.609344
        mikm <- c(.49,1,1.5,2,2.5,3,4,5,10,15,20,25,50,75,100)
        mikm <- mikm * 1.609344
        df <- df %>% mutate(bin = cut(miles, breaks=mikm))
        df <- group_by(df,bin) %>% summarise(rides = length(miles))
        colnames(df) = c("unit_dist","rides")
        return(df)
      }
      
    })
    
    output$all_binned_mileage <- renderUI({
      df<-mileagedf()
      verticalLayout(
        conditionalPanel(
          condition = "input.table == 'Graph'",
          renderPlot({
            ggplot(df, aes(x=unit_dist,y=rides)) + geom_bar( stat='identity', fill='steelblue') +
              labs(x=input$distUnit, y="Rides")+ scale_y_continuous(label=comma) +ggtitle(label = 'Ridership for distance') +scale_x_discrete(guide = guide_axis(angle = 90))    
          })
        ),
        conditionalPanel(
          condition = "input.table == 'Table'",
          box(DT::renderDataTable(
            df,
            options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(1, 'desc'))
            ), rownames = FALSE
          )
          )
        )
      )
    })
    
    triptime <- reactive({
      if(input$parts == 'Default' | (communityArea() == 78 & input$parts != 'Company') | (input$parts == 'Company' & input$compNames == 'All Taxi Companies' )){
        df <- data.table(data$`Trip Seconds`)
        colnames(df) = c("seconds")
        df <- df %>% mutate(bin = cut(seconds, breaks=c(59,180,360,600,900,1800,3600,5400,7200,10800,18000)))
        df<- group_by(df,bin) %>% summarise(rides = length(seconds))
        colnames(df) = c("seconds","rides")
        return(df)
      }
      else if(input$parts == 'Community'){
        if(input$tofrom == 'To'){
          df <- data[data$`Dropoff Community Area` == communityArea()]
          df <- data.table(df$`Trip Seconds`)
          colnames(df) = c("seconds")
          df <- df %>% mutate(bin = cut(seconds, breaks=c(59,180,360,600,900,1800,3600,5400,7200,10800,18000)))
          df<- group_by(df,bin) %>% summarise(rides = length(seconds))
          colnames(df) = c("seconds","rides")
          return(df)
        }
        else if(input$tofrom == 'From'){
          df <- data[data$`Pickup Community Area` == communityArea()]
          df <- data.table(df$`Trip Seconds`)
          colnames(df) = c("seconds")
          df <- df %>% mutate(bin = cut(seconds, breaks=c(59,180,360,600,900,1800,3600,5400,7200,10800,18000)))
          df<- group_by(df,bin) %>% summarise(rides = length(seconds))
          colnames(df) = c("seconds","rides")
          return(df)
        }
        
      }
      
      else if(input$parts == 'Company'){
        df <- data[data$Company == input$compNames]
        df <- data.table(df$`Trip Seconds`)
        colnames(df) = c("seconds")
        df <- df %>% mutate(bin = cut(seconds, breaks=c(59,180,360,600,900,1800,3600,5400,7200,10800,18000)))
        df<- group_by(df,bin) %>% summarise(rides = length(seconds))
        colnames(df) = c("seconds","rides")
        return(df)
      }
    })
    output$all_trip_time <- renderUI({
      df <- triptime()
      verticalLayout(
        conditionalPanel(
          condition = "input.table == 'Graph'",
          renderPlot({
            ggplot(df, aes(x=seconds,y=rides)) + geom_bar( stat='identity', fill='steelblue') +
              labs(x="Seconds", y="Rides")+ scale_y_continuous(label=comma) +ggtitle(label = 'Ridership for time of ride') +scale_x_discrete(guide = guide_axis(angle = 90))    
          })
        ),
        conditionalPanel(
          condition = "input.table == 'Table'",
          box(DT::renderDataTable(
            df,
            options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(1, 'desc'))
            ), rownames = FALSE
          )
          )
        )
      )
      
    })
      
    
    percent_gr <- reactive({
      if(input$parts == 'Default' | (communityArea() == 78 & input$parts != 'Company') | (input$parts == 'Company' & input$compNames == 'All Taxi Companies' )){
        df <- data.table(c('Chicago'),c(1))
        colnames(df) <- c('community','perc')
        return(df)
      }
      else if(input$parts == 'Community'){
        if(input$tofrom == 'To'){
          df <- data[data$`Dropoff Community Area` == communityArea()]
          df <- group_by(df,`Pickup Community Area`) %>% summarise(perc = length(`Dropoff Community Area`))
          colnames(df) = c('area','perc')
          colsum <- sum(df$perc)
          df$perc <- (df$perc/colsum)*100
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
          colnames(df) <- c('community','perc')
          return(df)
        }
        else if(input$tofrom == 'From'){
          df <- data[data$`Pickup Community Area` == communityArea()]
          df <- group_by(df,`Dropoff Community Area`) %>% summarise(perc = length(`Pickup Community Area`))
          colnames(df) = c('area','perc')
          colsum <- sum(df$perc)
          df$perc <- (df$perc/colsum)*100
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
          colnames(df) <- c('community','perc')
          return(df)
        }

      }
      else if(input$parts == 'Company'){
        if(input$tofrom == 'To'){
          df <- data[data$Company == input$compNames]
          df <- group_by(df,`Pickup Community Area`) %>% summarise(perc = length(`Dropoff Community Area`))
          colnames(df) = c('area','perc')
          colsum <- sum(df$perc)
          df$perc <- (df$perc/colsum)*100
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
          colnames(df) <- c('community','perc')
          return(df)
        }
        else if(input$tofrom =='From'){
          df <- data[data$Company == input$compNames]
          df <- group_by(df,`Dropoff Community Area`) %>% summarise(perc = length(`Pickup Community Area`))
          colnames(df) = c('area','perc')
          colsum <- sum(df$perc)
          df$perc <- (df$perc/colsum)*100
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
          colnames(df) <- c('community','perc')
          return(df)
          
        }
      }
    })
    
    output$perc_graph <- renderUI({
      df <- percent_gr()
      verticalLayout(
        conditionalPanel(
          condition = "input.table == 'Graph'",
      renderPlot({
      ggplot(df, aes(x=community,y=perc)) + geom_bar( stat='identity', fill='steelblue') +
        labs(x="Area", y="percent")+ scale_y_continuous(label=comma) +ggtitle(label = paste('%')) +scale_x_discrete(guide = guide_axis(angle = 90))    
      })),
      conditionalPanel(
        condition = "input.table == 'Table'",
        box(DT::renderDataTable(
          df,
          options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE
        )
        )
      )
      )
    })
    
    observeEvent(input$leaflet_shape_click,{
      if(input$parts == "Community"){
      click <- input$leaflet_shape_click
      map<-leafletProxy(mapId = "leaflet",session)
      # print(click$id)
      updateSelectInput(session, "comArea","Community Area", append(sort(shapeData$community),c('City_of_Chicago',click$id)), selected = click$id)
      }
    })
    

    output$leaflet <- renderLeaflet({
      df <- percent_gr()
      dt <- shapeData
      perc <- c()
      
      for(e in dt@data$community){
        perc <- append(perc, as.double(df[df$community == e][,2]))
      }
      
      x <- comAreaList
      
      x <- x[x != input$comArea]
      
      x <- lapply(x, function(x) paste0(x,'1'))
      if(input$parts == 'Default' | (communityArea() == 78 & input$parts != 'Company') | (input$parts == 'Company' & input$compNames == 'All Taxi Companies' )){
        
        leaflet()  %>% addTiles() %>% 
          setView(lng = -87.683177, lat = 41.921832, zoom = 11) %>% 
          addPolygons(data=dt,weight=2,fillOpacity = 0.7,color = "grey"                      ) 
      }
      else if(input$parts == 'Community'){
        mypal <- colorBin("Blues", perc, bins = c(0,.5,1,2,4,8,16,100))
        leaflet()  %>% addTiles() %>% 
          setView(lng = -87.683177, lat = 41.921832, zoom = 11) %>% 
          addPolygons(data=dt,weight=2,fillOpacity = 0.7,color = "black",
                      fillColor = mypal(perc), label = paste(dt$community,perc), layerId = dt@data$community) %>%
          addLegend(position = "bottomright", pal = mypal, values = perc,
                    title ='percent',
                    opacity = 0.7) %>%
          addPolylines(data=dt,color = 'red', layerId = paste0(dt@data$community,1)) %>%
          removeShape(layerId=x)
        
      }
      else if (input$parts == 'Company'){
        mypal <- colorBin("Blues", perc, bins = c(0,.5,1,2,4,8,16,100))
        leaflet()  %>% addTiles() %>% 
          setView(lng = -87.683177, lat = 41.921832, zoom = 11) %>% 
          addPolygons(data=dt,weight=2,fillOpacity = 0.7,color = "black",
                      fillColor = mypal(perc), label = paste(dt$community,perc), layerId = dt@data$community) %>%
          addLegend(position = "bottomright", pal = mypal, values = perc,
                    title ='percent',
                    opacity = 0.7)
        
      }
        
  
        
      
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
