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
gc()
options(scipen=10000)
com <- c('All','24 Seven Taxi','312 Medallion Mgmt.','5 Star Taxi','Adwar H. Nikola','Ahzmi','American United','American United Taxi','Arrington Ent.','Babylon Express','Benny Jona','Blue Diamond','Blue Ribbon Taxi','Checker Taxi','Chicago Carriage Cab','Chicago Ind.','Chicago Medallion Mgmt.','Chicago Star Taxicab','Chicago Taxicab','Choice Taxi','Chuks Cab','City Svc.','David K. Cab','Flash Cab','G.L.B. Cab','Globe Taxi','Gold Coast Taxi','JBL Cab.','Jay Kim','KOAM Taxi','Leonard Cab','Luhak','Medallion Leasin','Metro Jet Taxi A','N and W Cab','Nova Taxi','Omar Jada','Patriot Taxi Dba Peace Taxi','Petani Cab','RC Andrews Cab','Salifu Bawa','Sam Mestas','Santamaria Express','Sbeih','Sergey Cab','Setare','Star North Mgmt.','Sun Taxi','Tasha ride','Taxi Aff. Svc. Yellow','Taxi Aff. Svcs.','Taxicab Ins. Agcy.','Top Cab Aff.','U Taxicab','Yellow Cab')
# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Spring 2022 Project 3"),
  #edit to make mini menu items for both
  
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
                            selectInput("distUnit", "Distance Unit", c('mile','km'), selected = "mile")
                   ),
                   menuItem("Select options",
                            selectInput("parts",'Parts',c('All','Community','Company'),selected = 'All'),
                            selectInput("compNames", "Company", com, selected = "All"),
                            selectInput("comArea","Community Area", append(sort(shapeData$community),'None'), selected = 'None')
                   ),
                   #Option to change page to about section
                   menuItem("Page options",
                            selectInput("pageOption", "Select page", c("Data","About"), selected = "Data")
                   ),
                   
                   #Options to change left side of the page
                   menuItem("Left Options",
                            actionButton("reset_button", "Reset Map View"),
                            selectInput("longGraphOrder", "Order By", c("Alphabetical","Minimum","Maximum"), selected = "Alphabetical"),
                            selectInput("datesChange", "Date Constraints",c("One Day", "Two Day Differ"), selected = "One Day"),
                            conditionalPanel(
                              condition = "input.datesChange == 'One Day'",
                              actionButton("prev_day", "Prev Day"),
                              actionButton("next_day", "Next Day")
                            ),
                            dateInput("date1",label = "date1", value = "2021-8-23"),
                            conditionalPanel(
                              condition = "input.datesChange == 'Two Day Differ'",
                              dateInput("date2",label = "date2",value = "2021-8-22")
                            )
                   )
  ),
  
  dashboardBody(
    #Data page, only show when page option has data selected
    conditionalPanel(
      condition = "input.pageOption == 'Data'",
      fluidRow(
        column(6,
               fluidRow(
                 conditionalPanel(
                   condition = "input.datesChange == 'Two Day Differ'",
                   # plotOutput("main2",width="100%")
                 ),
                 conditionalPanel(
                   condition = "input.datesChange == 'One Day'",
                   verticalLayout(
                   plotOutput("all_rides_hour_day",width="100%"),
                   uiOutput("all_rides_year_day",width="100%"),
                   plotOutput("all_rides_weekday",width="100%"),
                   plotOutput("all_rides_monthly",width="100%"),
                   plotOutput("all_binned_mileage",width="100%"),
                   plotOutput("all_trip_time",width="100%"),
                   leafletOutput("leaflet")
                   )
                 )
                 
               ),
               fluidRow(
                 tags$style(type = "text/css", "#leaf {height: calc(65vh - 80px) !important;}"),
                 splitLayout(
                   cellWidths = c('80%','20%'),
                   # leafletOutput("leaf"),
                   verticalLayout(
                     conditionalPanel(
                       condition = "input.datesChange == 'Two Day Differ'",
                       # dataTableOutput("tab2")
                     ),
                     conditionalPanel(
                       condition = "input.datesChange == 'One Day'",
                       # dataTableOutput("tab1")
                     )
                   )
                 )
               ),
               
        )
        
      )
    ),
    #About page, only show when page option about is selected
    conditionalPanel(
      condition = "input.pageOption == 'About'",
      fluidRow(
        h1("About Page"),
        p("The data is from https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f"),
        p("and https://data.cityofchicago.org/Transportation/CTA-System-Information-List-of-L-Stops/8pix-ypme"),
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
  compNamedf <- reactive({
    if(input$compNames == 'All'){
      return(data)
    }
    else{
      return(subset(data,Company==input$compNames))
    }
  })
  mileagedf <- reactive({
    df<-compNamedf()

    if(input$distUnit == 'mile')
    {
      df <- data.frame(df$`Trip Miles`)
      colnames(df) = c("miles")
      df <- df %>% mutate(bin = cut(miles, breaks=c(.49,1,1.5,2,2.5,3,4,5,10,15,20,25,50,75,100)))
      df <- group_by(df,bin) %>% summarise(rides = length(miles))
      colnames(df) = c("unit_dist","rides")
      return(df)
    }
    else{
      df <- data.frame(df$`Trip Miles`)
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
  
  timeform <- reactive({
    df <- group_by(data,Hour) %>% summarise(rides = length(`Trip Seconds`))
    colnames(df) = c("hour","rides")
    if(input$timeFormat == '12 hour'){
      ampm <- c('12 AM', '1 AM', '2 AM', '3 AM', '4 AM', '5 AM', '6 AM', '7 AM', '8 AM', '9 AM', '10 AM', '11 AM','12 PM', '1 PM', '2 PM', '3 PM', '4 PM', '5 PM', '6 PM', '7 PM', '8 PM', '9 PM', '10 PM', '11 PM' )
      df$hour <- ampm
      df$hour <- factor(df$hour,levels =  ampm)
      return(df)
    }
    else{
      return(df)
    }
  })
  
  

    output$all_rides_year_day <- renderUI({
      df <- group_by(data,`Trip Start Timestamp`) %>% summarise(rides = length(`Trip Seconds`))
      colnames(df) = c("date","rides")
      df$date = ymd(df$date)
      verticalLayout(
        renderPlot({
        ggplot(df, aes(x=date,y=rides)) + geom_bar( stat='identity', fill='steelblue') +
          labs(x="Day", y="Rides")+ scale_y_continuous(label=comma) +ggtitle(label = 'Ridership for each day')
        })
      )
    })
    
    output$all_rides_hour_day <- renderPlot({
      df <- timeform()
      ggplot(df, aes(x=hour,y=rides)) + geom_bar( stat='identity', fill='steelblue') +
        labs(x="Hour", y="Rides")+ scale_y_continuous(label=comma) +ggtitle(label = 'Ridership for each hour') + scale_x_discrete(guide = guide_axis(angle = 90)) 
      
    })
    
    output$all_rides_weekday <- renderPlot({
      df <- group_by(data,`Trip Start Timestamp`) %>% summarise(rides = length(`Trip Seconds`))
      colnames(df) = c("date","rides")
      df$date <- ymd(df$date)
      df$date<- lubridate::wday(df$date,abbr = TRUE, label = TRUE)
      colnames(df) = c("weekday","rides")
      ggplot(df, aes(x=weekday,y=rides)) + geom_bar( stat='identity', fill='steelblue') +
        labs(x="Day", y="Rides")+ scale_y_continuous(label=comma) +ggtitle(label = 'Ridership for each week day')
      
      
    })
    
    output$all_rides_monthly <- renderPlot({
      df <- group_by(data,`Trip Start Timestamp`) %>% summarise(rides = length(`Trip Seconds`))
      colnames(df) = c("date","rides")
      df$date = substr(df$date,5,6)
      df <- group_by(df,date) %>% summarise(rides = sum(rides))
      colnames(df) = c("date","rides")
      df$date <-c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec')
      df$date <- factor(df$date,levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec'))
      
      ggplot(df, aes(x=date,y=rides)) + geom_bar( stat='identity', fill='steelblue') +
        labs(x="Month", y="Rides")+ scale_y_continuous(label=comma) +ggtitle(label = 'Ridership for each month')
      
    })
    
    output$all_binned_mileage <- renderPlot({
      df<-mileagedf()
      ggplot(df, aes(x=unit_dist,y=rides)) + geom_bar( stat='identity', fill='steelblue') +
        labs(x=input$distUnit, y="Rides")+ scale_y_continuous(label=comma) +ggtitle(label = 'Ridership for distance') +scale_x_discrete(guide = guide_axis(angle = 90))    
    })
    
    output$all_trip_time <- renderPlot({
      df <- data.frame(data$`Trip Seconds`)
      colnames(df) = c("seconds")
      df <- df %>% mutate(bin = cut(seconds, breaks=c(59,180,360,600,900,1800,3600,5400,7200,10800,18000)))
      df<- group_by(df,bin) %>% summarise(rides = length(seconds))
      colnames(df) = c("seconds","rides")
      df$seconds <- c('(59,180]','(180,360]','(360,600]','(600,900]','(900,1800]','(1800,3600]','(3600,5400]','(5400,7200]','(7200,10800]','(10800,18000]')
      df$seconds <- factor(df$seconds,levels =  c('(59,180]','(180,360]','(360,600]','(600,900]','(900,1800]','(1800,3600]','(3600,5400]','(5400,7200]','(7200,10800]','(10800,18000]'))
      ggplot(df, aes(x=seconds,y=rides)) + geom_bar( stat='identity', fill='steelblue') +
        labs(x="Seconds", y="Rides")+ scale_y_continuous(label=comma) +ggtitle(label = 'Ridership for time of ride') +scale_x_discrete(guide = guide_axis(angle = 90))    
    })
    
    output$leaflet <- renderLeaflet({
      leaflet()  %>% addTiles() %>% 
        setView(lng = -87.683177, lat = 41.921832, zoom = 11) %>% 
        addPolygons(data=shapeData,weight=5,col = 'grey')
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
