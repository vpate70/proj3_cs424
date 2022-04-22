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

files <- list.files(path = "./data",pattern = ".csv")
for(i in 1:length(files)){
  files[i] = paste('./data',files[i],sep='/')
}

temp <- lapply(files, fread, sep=",")
data <- rbindlist( temp )
rm(temp)
gc()

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
                   menuItem("Right options",
                            selectInput("rstation_name", "Select the station name", c('test'), selected = "test"),
                            selectInput("rtype_x", "Select the constraint", c('test'), selected = "test"),
                            selectInput("rtableCheck", "Show Table Values",c('test'), selected = "test"),
                            conditionalPanel(
                              condition = "input.rtype_x != 'Default'",
                              selectInput("rYear", "Select the year to visualize", c('test'), selected = "test")
                            )
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
                   plotOutput("all_rides_year_day",width="100%"),
                   plotOutput("all_rides_weekday",width="100%"),
                   plotOutput("all_rides_monthly",width="100%")
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
               
        ),
        
        column(6,
               # uiOutput("rightUI")
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
        p("Created: Spring 2022, March"),
        p("The application was created for Project 2 of Spring 2022 CS 424 with Dr. Johnson")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$all_rides_year_day <- renderPlot({
      #Fix x labels
      df <- aggregate(data$`Trip Seconds`, by=list(Category=data$`Trip Start Timestamp`), FUN=length)
      colnames(df) = c("date","rides")
      df$date = substr(df$date,5,8)
      ggplot(df, aes(x=date,y=rides)) + geom_bar( stat='identity', fill='steelblue') +
        labs(x="Day", y="Rides")+ scale_y_continuous(label=comma) +ggtitle(label = 'Ridership for each day')
        
    })
    
    output$all_rides_hour_day <- renderPlot({
      df <- aggregate(data$`Trip Seconds`, by=list(Category=data$Hour), FUN=length)
      colnames(df) = c("hour","rides")
      ggplot(df, aes(x=hour,y=rides)) + geom_bar( stat='identity', fill='steelblue') +
        labs(x="Hour", y="Rides")+ scale_y_continuous(label=comma) +ggtitle(label = 'Ridership for each hour')
      
    })
    
    output$all_rides_weekday <- renderPlot({
      df <- aggregate(data$`Trip Seconds`, by=list(Category=data$`Trip Start Timestamp`), FUN=length)
      colnames(df) = c("date","rides")
      df$date <- ymd(df$date)
      df$date<- lubridate::wday(df$date,abbr = TRUE, label = TRUE)
      colnames(df) = c("weekday","rides")
      ggplot(df, aes(x=weekday,y=rides)) + geom_bar( stat='identity', fill='steelblue') +
        labs(x="Day", y="Rides")+ scale_y_continuous(label=comma) +ggtitle(label = 'Ridership for each week day')
      
    })
    
    output$all_rides_monthly <- renderPlot({
      df <- aggregate(data$`Trip Seconds`, by=list(Category=data$`Trip Start Timestamp`), FUN=length)
      colnames(df) = c("date","rides")
      df$date = substr(df$date,5,6)
      df <- aggregate(df$rides, by=list(Category=df$date), FUN=sum)
      colnames(df) = c("date","rides")
      df$date <-c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec')
      df$date <- factor(df$date,levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec'))
      
      ggplot(df, aes(x=date,y=rides)) + geom_bar( stat='identity', fill='steelblue') +
        labs(x="Month", y="Rides")+ scale_y_continuous(label=comma) +ggtitle(label = 'Ridership for each month')
      
    })
    
    output$binned_mileage <- renderPlot({
      df <- aggregate(data$`Trip Seconds`, by=list(Category=data$`Trip Start Timestamp`), FUN=length)
      colnames(df) = c("date","rides")
      df$date = substr(df$date,5,6)
      df <- aggregate(df$rides, by=list(Category=df$date), FUN=sum)
      colnames(df) = c("date","rides")
      df$date <-c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec')
      df$date <- factor(df$date,levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec'))
      
      ggplot(df, aes(x=date,y=rides)) + geom_bar( stat='identity', fill='steelblue') +
        labs(x="Month", y="Rides")+ scale_y_continuous(label=comma) +ggtitle(label = 'Ridership for each month')
      
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
