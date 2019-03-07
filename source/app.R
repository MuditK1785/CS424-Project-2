######################################################################################################################################
# Authors: Rami Masoud, Jeet Raol, Mudit Kumar, Jonathan Munoz                                                                       #
# Class  : UIC CS 424, Spring 2019                                                                                                   #
# Project: #2 Every Breath You Take                                                                                                  #
# About  : A web-based application that visualizes data in multiple ways using R, Shiny, ggplot2, & Shiny Dashboard.                 #
#                                                                                                                                    #
# This is a Shiny web application. You can run the application by clicking the 'Run App' button above.                               #
# Find out more about building applications with Shiny here: http://shiny.rstudio.com/                                               #
######################################################################################################################################

# Importing Libraries
library(DT)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(plyr)
library(lubridate)
library(readr)
library(fst)
library(forcats)
library(leaflet)
library(scales)
library(ggrepel)
library(tidyverse)
library(reshape2)

# Read in the 2 pre-processed feather files
dailyData  <- read_fst('dailyData.fst')
#hourlyData <- read_fst('hourlyData.fst')

# Create a year frame for the daily data frame
dailyData$Year <- year(as.Date(dailyData$Date, format = "%Y-%m-%d"))

# Get list of counites and rename the columns
listOfCounties <- dailyData[c(1,2)]

# Remove the duplicate rows in the listOfCounties dataframe
listOfCounties <- listOfCounties %>% distinct

# Dropdown items for map - Part C
aqiPollutantList <- c("AQI", "Ozone", "SO2", "CO", "NO2", "PM2.5", "PM10")

# Read in the location data for the map using the aqs_sites.csv
sites <- read.table(file = "aqs_sites.csv", sep=",",header = TRUE)

# Create a years dataframe & rename the column
listOfYears<-as.data.frame(c(1990:2018))
# Rename the column
names(listOfYears)[1]<-"Year"

# -----------------------------------------------------------------------------
# Dashboard UI
# -----------------------------------------------------------------------------
# Set the UI as a dashboardPage type
ui <- dashboardPage(
  
  # Set up the header
  dashboardHeader(
    
    # Expand the title space
    titleWidth = 300,
    
    # Apply the custom logo 
    title = "CS 424 Project 2"
    
  ),
  
  # Set up sidebar
  dashboardSidebar(
    
    # Expand sidebar width
    width = 300,
    
    # Enable sidebar, set to open at app start
    disable = FALSE, collapsed = FALSE,
    
    
    # Here we can add side bar widgets
    sidebarMenu(id = "sidebarmenu",
                
                # Set tab side menu pages
                menuItem("Part C 1st Bullet Point A", tabName = "P1A"), # Dashboard displays all county data, charts, and graphs
                menuItem("Part C 1st Bullet Point B", tabName = "P1B"), # Dashboard displays all county data, charts, and graphs
                menuItem("Part C 3rd Bullet Point", tabName = "3BP"), # Choose year and county then be able to see daily AQI line chart, bar chart, and table
                menuItem("Part C 4th Bullet Point", tabName = "map"), # Displays the map for top 100 counties of AQI or a pollutant type
                menuItem("About", tabName = "about"), # Find more about this project.
                
                conditionalPanel("input.sidebarmenu === 'P1A' || input.sidebarmenu === '3BP'",
                                 selectizeInput("State", "Select State", choices = unique(listOfCounties$State), selected = "Illinois"),
                                 selectizeInput("County", "Select County", choices = unique(listOfCounties$County), selected = "Cook"),
                                 
                                 # Set up the year select input
                                 selectizeInput("Year", "Select the Year to See Data For", choices = unique(listOfYears$Year), selected = "2018")
                ),
                
                conditionalPanel("input.sidebarmenu === 'P1B'",
                                 selectizeInput("State", "Select State", choices = unique(listOfCounties$State), selected = "Illinois"),
                                 selectizeInput("County", "Select County", choices = unique(listOfCounties$County), selected = "Cook")
                ),
                conditionalPanel("input.sidebarmenu === 'map'",
                                 # Set up the year select input
                                 selectizeInput("mapYear", "Select the Year to See Data For", choices = unique(listOfYears$Year), selected = "2018"),
                                 selectizeInput("Type", "Pick how you want to categorize the map (AQI or a Pollutant)", choices = unique((aqiPollutantList)), selected = "AQI")
                                 
                )
                
    )
  ),
  
  # Set up the dashboard body
  dashboardBody(
    
    tabItems(
      tabItem("P1A",
              fluidRow(
                box( title = "Air Quality Pie", solidHeader = TRUE, status = "primary", width = 6, plotOutput("pieChartAQI")),
                
                box( title = "Air Quality Bar", solidHeader = TRUE, status = "primary", width = 6, plotOutput("barChartAQI")),
                
                box(title = "Air Quality Table", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("tableAQI"))
              ),
              
              fluidRow(
                box( title = "Days When Main Pollutant is CO", solidHeader = TRUE, status = "primary", width = 4, plotOutput("pieChartCO")),
                
                box( title = "Days When Main Pollutant is NO2", solidHeader = TRUE, status = "primary", width = 4, plotOutput("pieChartNO2")),
                
                box( title = "Days When Main Pollutant is OZONE", solidHeader = TRUE, status = "primary", width = 4, plotOutput("pieChartOZONE")),
                
                box( title = "Days When Main Pollutant is SO2", solidHeader = TRUE, status = "primary", width = 4, plotOutput("pieChartSO2")),
                
                box( title = "Days When Main Pollutant is PM2.5", solidHeader = TRUE, status = "primary", width = 4, plotOutput("pieChartPM2")),
                
                box( title = "Days When Main Pollutant is PM10", solidHeader = TRUE, status = "primary", width = 4, plotOutput("pieChartPM10"))
                
              ),
              
              fluidRow(
                box( title = "Main Pollutant Days Bar", solidHeader = TRUE, status = "primary", width = 6, plotOutput("barChartPollutant")),
                box( title = "Main Pollutant Days Table", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("tablePollutant"))
              )
      ),
      tabItem("P1B",
              fluidRow(
                box(title = "AQI Readings from 1990-2018", solidHeader = TRUE, status = "primary", width = 12, plotOutput("lineChart")),
                box(title = "AQI Type Percentages from 1990-2018", solidHeader = TRUE, status = "primary", width = 12, plotOutput("lineChart2")),
                box(title = "Location", solidHeader = TRUE, status = "primary", width = 12, leafletOutput("leaf", height = 300))
              )
      ),
      tabItem("3BP",
              fluidRow(
                box( title = "Daily AQI Data Line Chart with Leading Pollutant", solidHeader = TRUE, status = "primary", width = 12, plotOutput("lineChartAQI")),
                box( title = "Stacked Bar Chart - AQI", solidHeader = TRUE, status = "primary", width = 6, plotOutput("stackedBarChartAQI")),
                box( title = "Monthly AQI of Year", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("tableMonthlyAQI"))
              )
      ),
      tabItem("map",
              fluidRow(
                
              )
      ),
      tabItem("about",
              a("Find out more about this project on the official website", href="https://sites.google.com/uic.edu/everybreath/")
      )
    )
  )
)
server <- function(input, output, session) 
{
  # Once a state is selected update the the county selectize input to match the counties inside the state
  observeEvent( input$State,
                updateSelectizeInput(session, "County", "County", 
                                     choices = (listOfCounties$County[listOfCounties$State==input$State]), server = TRUE))
  
  ###################################################################################################################################
  filteredData <- reactive({subset(dailyData, (State  ==  input$State)
                                   &    (County ==  input$County)
                                   &    (Year   ==  input$Year))})
  # Return the AQI status for the users selection
  AQIstatus <- reactive({ data <- filter(dailyData, (State  ==  input$State)
                                         &    (County ==  input$County)
                                         &    (Year   ==  input$Year))
  
  # For the returned data frame only keep the column that we need in this case the "Category"
  data <- data[c(5)]
  
  # Update the data frame with the number of days for each Category
  data <- data.frame("AQI" = c("Good", "Moderate", "Unhealthy for Sensitive", "Unhealthy", "Very Unhealthy", "Hazardous"),
                     "Days"= c(length(which(data == "Good")), length(which(data == "Moderate")), length(which(data == "Unhealthy for Sensitive Groups")), 
                               length(which(data == "Unhealthy")), length(which(data == "Very Unhealthy")), length(which(data == "Hazardous"))))
  
  # From the Days column create a percentage column
  data <- data %>% mutate(Percent = percent(Days / sum(Days)))
  })
  ###################################################################################################################################
  #PART B FROM PROJECT 1 MATERIAL FIND MAX, MIN, 90 PERCENTILE
  AQIvalue <- reactive({ 
    
    data1990 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1990)) 
    max1990<-data1990 %>% slice(which.max(AQI)) 
    min1990<-data1990 %>% slice(which.min(AQI)) 
    per1990<-as.data.frame(quantile(data1990$AQI, probs = c(0.90))) 
    names(per1990)[1]<-"AQI"
    data1991 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1991)) 
    max1991<-data1991 %>% slice(which.max(AQI)) 
    min1991<-data1991 %>% slice(which.min(AQI))
    per1991<-as.data.frame(quantile(data1991$AQI, probs = c(0.90))) 
    names(per1991)[1]<-"AQI"
    data1992 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1992)) 
    max1992<-data1992 %>% slice(which.max(AQI)) 
    min1992<-data1992 %>% slice(which.min(AQI))
    per1992<-as.data.frame(quantile(data1992$AQI, probs = c(0.90))) 
    names(per1992)[1]<-"AQI"
    data1993 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1993))
    max1993<-data1993 %>% slice(which.max(AQI)) 
    min1993<-data1993 %>% slice(which.min(AQI))
    per1993<-as.data.frame(quantile(data1993$AQI, probs = c(0.90))) 
    names(per1993)[1]<-"AQI"
    data1994 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1994))
    max1994<-data1994 %>% slice(which.max(AQI)) 
    min1994<-data1994 %>% slice(which.min(AQI))
    per1994<-as.data.frame(quantile(data1994$AQI, probs = c(0.90))) 
    names(per1994)[1]<-"AQI"
    data1995 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1995))
    max1995<-data1995 %>% slice(which.max(AQI)) 
    min1995<-data1995 %>% slice(which.min(AQI)) 
    per1995<-as.data.frame(quantile(data1995$AQI, probs = c(0.90))) 
    names(per1995)[1]<-"AQI"
    data1996 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1996)) 
    max1996<-data1996 %>% slice(which.max(AQI)) 
    min1996<-data1996 %>% slice(which.min(AQI))
    per1996<-as.data.frame(quantile(data1996$AQI, probs = c(0.90))) 
    names(per1996)[1]<-"AQI"
    data1997 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1997)) 
    max1997<-data1997 %>% slice(which.max(AQI)) 
    min1997<-data1997 %>% slice(which.min(AQI))
    per1997<-as.data.frame(quantile(data1997$AQI, probs = c(0.90))) 
    names(per1997)[1]<-"AQI"
    data1998 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1998)) 
    max1998<-data1998 %>% slice(which.max(AQI)) 
    min1998<-data1998 %>% slice(which.min(AQI))
    per1998<-as.data.frame(quantile(data1998$AQI, probs = c(0.90))) 
    names(per1998)[1]<-"AQI"
    data1999 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1999)) 
    max1999<-data1999 %>% slice(which.max(AQI)) 
    min1999<-data1999 %>% slice(which.min(AQI))
    per1999<-as.data.frame(quantile(data1999$AQI, probs = c(0.90))) 
    names(per1999)[1]<-"AQI"
    data2000 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2000)) 
    max2000<-data2000 %>% slice(which.max(AQI)) 
    min2000<-data2000 %>% slice(which.min(AQI))
    per2000<-as.data.frame(quantile(data2000$AQI, probs = c(0.90))) 
    names(per2000)[1]<-"AQI"
    data2001 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2001)) 
    max2001<-data2001 %>% slice(which.max(AQI)) 
    min2001<-data2001 %>% slice(which.min(AQI))
    per2001<-as.data.frame(quantile(data2001$AQI, probs = c(0.90))) 
    names(per2001)[1]<-"AQI"
    data2002 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2002))
    max2002<-data2002 %>% slice(which.max(AQI)) 
    min2002<-data2002 %>% slice(which.min(AQI))
    per2002<-as.data.frame(quantile(data2002$AQI, probs = c(0.90))) 
    names(per2002)[1]<-"AQI"
    data2003 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2003)) 
    max2003<-data2003 %>% slice(which.max(AQI)) 
    min2003<-data2003 %>% slice(which.min(AQI))
    per2003<-as.data.frame(quantile(data2003$AQI, probs = c(0.90))) 
    names(per2003)[1]<-"AQI"
    data2004 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2004))
    max2004<-data2004 %>% slice(which.max(AQI)) 
    min2004<-data2004 %>% slice(which.min(AQI))
    per2004<-as.data.frame(quantile(data2004$AQI, probs = c(0.90))) 
    names(per2004)[1]<-"AQI"
    data2005 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2005)) 
    max2005<-data2005 %>% slice(which.max(AQI)) 
    min2005<-data2005 %>% slice(which.min(AQI))
    per2005<-as.data.frame(quantile(data2005$AQI, probs = c(0.90))) 
    names(per2005)[1]<-"AQI"
    data2006 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2006))
    max2006<-data2006 %>% slice(which.max(AQI)) 
    min2006<-data2006 %>% slice(which.min(AQI))
    per2006<-as.data.frame(quantile(data2006$AQI, probs = c(0.90))) 
    names(per2006)[1]<-"AQI"
    data2007 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2007))
    max2007<-data2007 %>% slice(which.max(AQI)) 
    min2007<-data2007 %>% slice(which.min(AQI))
    per2007<-as.data.frame(quantile(data2007$AQI, probs = c(0.90))) 
    names(per2007)[1]<-"AQI"
    data2008 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2008)) 
    max2008<-data2008 %>% slice(which.max(AQI))
    min2008<-data2008 %>% slice(which.min(AQI))
    per2008<-as.data.frame(quantile(data2008$AQI, probs = c(0.90))) 
    names(per2008)[1]<-"AQI"
    data2009 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2009)) 
    max2009<-data2009 %>% slice(which.max(AQI)) 
    min2009<-data2009 %>% slice(which.min(AQI))
    per2009<-as.data.frame(quantile(data2009$AQI, probs = c(0.90))) 
    names(per2009)[1]<-"AQI"
    data2010 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2010)) 
    max2010<-data2010 %>% slice(which.max(AQI)) 
    min2010<-data2010 %>% slice(which.min(AQI))
    per2010<-as.data.frame(quantile(data2010$AQI, probs = c(0.90))) 
    names(per2010)[1]<-"AQI"
    data2011 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2011)) 
    max2011<-data2011 %>% slice(which.max(AQI)) 
    min2011<-data2011 %>% slice(which.min(AQI))
    per2011<-as.data.frame(quantile(data2011$AQI, probs = c(0.90))) 
    names(per2011)[1]<-"AQI"
    data2012 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2012)) 
    max2012<-data2012 %>% slice(which.max(AQI)) 
    min2012<-data2012 %>% slice(which.min(AQI))
    per2012<-as.data.frame(quantile(data2012$AQI, probs = c(0.90))) 
    names(per2012)[1]<-"AQI"
    data2013 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2013)) 
    max2013<-data2013 %>% slice(which.max(AQI)) 
    min2013<-data2013 %>% slice(which.min(AQI))
    per2013<-as.data.frame(quantile(data2013$AQI, probs = c(0.90))) 
    names(per2013)[1]<-"AQI"
    data2014 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2014)) 
    max2014<-data2014 %>% slice(which.max(AQI)) 
    min2014<-data2014 %>% slice(which.min(AQI))
    per2014<-as.data.frame(quantile(data2014$AQI, probs = c(0.90))) 
    names(per2014)[1]<-"AQI"
    data2015 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2015)) 
    max2015<-data2015 %>% slice(which.max(AQI))
    min2015<-data2015 %>% slice(which.min(AQI)) 
    per2015<-as.data.frame(quantile(data2015$AQI, probs = c(0.90))) 
    names(per2015)[1]<-"AQI"
    data2016 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2016)) 
    max2016<-data2016 %>% slice(which.max(AQI)) 
    min2016<-data2016 %>% slice(which.min(AQI))
    per2016<-as.data.frame(quantile(data2016$AQI, probs = c(0.90))) 
    names(per2016)[1]<-"AQI"
    data2017 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2017))
    max2017<-data2017 %>% slice(which.max(AQI)) 
    min2017<-data2017 %>% slice(which.min(AQI)) 
    per2017<-as.data.frame(quantile(data2017$AQI, probs = c(0.90))) 
    names(per2017)[1]<-"AQI"
    data2018 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2018)) 
    max2018<-data2018 %>% slice(which.max(AQI))
    min2018<-data2018 %>% slice(which.min(AQI))
    per2018<-as.data.frame(quantile(data2018$AQI, probs = c(0.90))) 
    names(per2018)[1]<-"AQI"
    
    #Combine the seperate data files first depending on category then...
    #Combine Max, Min, and 90 Percentile into one data frame  
    #Before we can do this we must make sure each data frame has the same column names for the rbind at the end
    maxData <- rbind(max1990,max1991,max1992,max1993,max1994,max1995,max1996,max1997,max1998,max1999,
                     max2000,max2001,max2002,max2003,max2004,max2005,max2006,max2007,max2008,max2009,
                     max2010,max2011,max2012,max2013,max2014,max2015,max2016,max2017,max2018)
    
    maxData <- maxData[c(4,8)]
    
    names(maxData)[1]<-"AQI"
    names(maxData)[2]<-"Year"
    maxData$Group <- "Max"
    
    minData <- rbind(min1990,min1991,min1992,min1993,min1994,min1995,min1996,min1997,min1998,min1999,
                     min2000,min2001,min2002,min2003,min2004,min2005,min2006,min2007,min2008,min2009,
                     min2010,min2011,min2012,min2013,min2014,min2015,min2016,min2017,min2018)
    
    minData <- minData[c(4,8)]
    
    names(minData)[1]<-"AQI"
    names(minData)[2]<-"Year"
    minData$Group <- "Min"
    
    perData <- rbind(per1990,per1991,per1992,per1993,per1994,per1995,per1996,per1997,per1998,per1999,
                     per2000,per2001,per2002,per2003,per2004,per2005,per2006,per2007,per2008,per2009,
                     per2010,per2011,per2012,per2013,per2014,per2015,per2016,per2017,per2018)
    
    names(perData)[1]<-"AQI"
    perData$Year <- c(1990:2018)
    perData$Group <- "90th Percentile"
    
    data <- rbind(maxData,minData,perData)
    })
  ###################################################################################################################################
  # Line graph data for the POLLUTANT percentages through out 1990-2018
  ParameterPercent <- reactive({ 
    
    data1990 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1990)) 
    data1990 <- data1990[c(6)]
    data1990 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data1990 == "CO")), length(which(data1990 == "NO2")), length(which(data1990 == "OZONE")), length(which(data1990 == "SO2")), length(which(data1990 == "PM2.5")), length(which(data1990 == "PM10"))))
    data1990 <- data1990 %>% mutate(Percent = percent(Days / sum(Days)))
    data1991 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1991)) 
    data1991 <- data1991[c(6)]
    data1991 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data1991 == "CO")), length(which(data1991 == "NO2")), length(which(data1991 == "OZONE")), length(which(data1991 == "SO2")), length(which(data1991 == "PM2.5")), length(which(data1991 == "PM10"))))
    data1991 <- data1991 %>% mutate(Percent = percent(Days / sum(Days)))
    data1992 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1992)) 
    data1992 <- data1992[c(6)]
    data1992 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data1992 == "CO")), length(which(data1992 == "NO2")), length(which(data1992 == "OZONE")), length(which(data1992 == "SO2")), length(which(data1992 == "PM2.5")), length(which(data1992 == "PM10"))))
    data1992 <- data1992 %>% mutate(Percent = percent(Days / sum(Days)))
    data1993 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1993)) 
    data1993 <- data1993[c(6)]
    data1993 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data1993 == "CO")), length(which(data1993 == "NO2")), length(which(data1993 == "OZONE")), length(which(data1993 == "SO2")), length(which(data1993 == "PM2.5")), length(which(data1993 == "PM10"))))
    data1993 <- data1993 %>% mutate(Percent = percent(Days / sum(Days)))
    data1994 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1994)) 
    data1994 <- data1994[c(6)]
    data1994 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data1994 == "CO")), length(which(data1994 == "NO2")), length(which(data1994 == "OZONE")), length(which(data1994 == "SO2")), length(which(data1994 == "PM2.5")), length(which(data1994 == "PM10"))))
    data1994 <- data1994 %>% mutate(Percent = percent(Days / sum(Days)))
    data1995 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1995)) 
    data1995 <- data1995[c(6)]
    data1995 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data1995 == "CO")), length(which(data1995 == "NO2")), length(which(data1995 == "OZONE")), length(which(data1995 == "SO2")), length(which(data1995 == "PM2.5")), length(which(data1995 == "PM10"))))
    data1995 <- data1995 %>% mutate(Percent = percent(Days / sum(Days)))
    data1996 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1996)) 
    data1996 <- data1996[c(6)]
    data1996 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data1996 == "CO")), length(which(data1996 == "NO2")), length(which(data1996 == "OZONE")), length(which(data1996 == "SO2")), length(which(data1996 == "PM2.5")), length(which(data1996 == "PM10"))))
    data1996 <- data1996 %>% mutate(Percent = percent(Days / sum(Days)))
    data1997 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1997)) 
    data1997 <- data1997[c(6)]
    data1997 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data1997 == "CO")), length(which(data1997 == "NO2")), length(which(data1997 == "OZONE")), length(which(data1997 == "SO2")), length(which(data1997 == "PM2.5")), length(which(data1997 == "PM10"))))
    data1997 <- data1997 %>% mutate(Percent = percent(Days / sum(Days)))
    data1998 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1998)) 
    data1998 <- data1998[c(6)]
    data1998 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data1998 == "CO")), length(which(data1998 == "NO2")), length(which(data1998 == "OZONE")), length(which(data1998 == "SO2")), length(which(data1998 == "PM2.5")), length(which(data1998 == "PM10"))))
    data1998 <- data1998 %>% mutate(Percent = percent(Days / sum(Days)))
    data1999 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1999)) 
    data1999 <- data1999[c(6)]
    data1999 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data1999 == "CO")), length(which(data1999 == "NO2")), length(which(data1999 == "OZONE")), length(which(data1999 == "SO2")), length(which(data1999 == "PM2.5")), length(which(data1999 == "PM10"))))
    data1999 <- data1999 %>% mutate(Percent = percent(Days / sum(Days)))
    data2000 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2000)) 
    data2000 <- data2000[c(6)]
    data2000 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data2000 == "CO")), length(which(data2000 == "NO2")), length(which(data2000 == "OZONE")), length(which(data2000 == "SO2")), length(which(data2000 == "PM2.5")), length(which(data2000 == "PM10"))))
    data2000 <- data2000 %>% mutate(Percent = percent(Days / sum(Days)))
    data2001 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2001)) 
    data2001 <- data2001[c(6)]
    data2001 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data2001 == "CO")), length(which(data2001 == "NO2")), length(which(data2001 == "OZONE")), length(which(data2001 == "SO2")), length(which(data2001 == "PM2.5")), length(which(data2001 == "PM10"))))
    data2001 <- data2001 %>% mutate(Percent = percent(Days / sum(Days)))
    data2002 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2002)) 
    data2002 <- data2002[c(6)]
    data2002 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data2002 == "CO")), length(which(data2002 == "NO2")), length(which(data2002 == "OZONE")), length(which(data2002 == "SO2")), length(which(data2002 == "PM2.5")), length(which(data2002 == "PM10"))))
    data2002 <- data2002 %>% mutate(Percent = percent(Days / sum(Days)))
    data2003 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2003)) 
    data2003 <- data2003[c(6)]
    data2003 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data2003 == "CO")), length(which(data2003 == "NO2")), length(which(data2003 == "OZONE")), length(which(data2003 == "SO2")), length(which(data2003 == "PM2.5")), length(which(data2003 == "PM10"))))
    data2003 <- data2003 %>% mutate(Percent = percent(Days / sum(Days)))
    data2004 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2004)) 
    data2004 <- data2004[c(6)]
    data2004 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data2004 == "CO")), length(which(data2004 == "NO2")), length(which(data2004 == "OZONE")), length(which(data2004 == "SO2")), length(which(data2004 == "PM2.5")), length(which(data2004 == "PM10"))))
    data2004 <- data2004 %>% mutate(Percent = percent(Days / sum(Days)))
    data2005 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2005)) 
    data2005 <- data2005[c(6)]
    data2005 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data2005 == "CO")), length(which(data2005 == "NO2")), length(which(data2005 == "OZONE")), length(which(data2005 == "SO2")), length(which(data2005 == "PM2.5")), length(which(data2005 == "PM10"))))
    data2005 <- data2005 %>% mutate(Percent = percent(Days / sum(Days)))
    data2006 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2006)) 
    data2006 <- data2006[c(6)]
    data2006 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data2006 == "CO")), length(which(data2006 == "NO2")), length(which(data2006 == "OZONE")), length(which(data2006 == "SO2")), length(which(data2006 == "PM2.5")), length(which(data2006 == "PM10"))))
    data2006 <- data2006 %>% mutate(Percent = percent(Days / sum(Days)))
    data2007 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2007)) 
    data2007 <- data2007[c(6)]
    data2007 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data2007 == "CO")), length(which(data2007 == "NO2")), length(which(data2007 == "OZONE")), length(which(data2007 == "SO2")), length(which(data2007 == "PM2.5")), length(which(data2007 == "PM10"))))
    data2007 <- data2007 %>% mutate(Percent = percent(Days / sum(Days)))
    data2008 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2008)) 
    data2008 <- data2008[c(6)]
    data2008 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data2008 == "CO")), length(which(data2008 == "NO2")), length(which(data2008 == "OZONE")), length(which(data2008 == "SO2")), length(which(data2008 == "PM2.5")), length(which(data2008 == "PM10"))))
    data2008 <- data2008 %>% mutate(Percent = percent(Days / sum(Days)))
    data2009 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2009)) 
    data2009 <- data2009[c(6)]
    data2009 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data2009 == "CO")), length(which(data2009 == "NO2")), length(which(data2009 == "OZONE")), length(which(data2009 == "SO2")), length(which(data2009 == "PM2.5")), length(which(data2009 == "PM10"))))
    data2009 <- data2009 %>% mutate(Percent = percent(Days / sum(Days)))
    data2010 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2010)) 
    data2010 <- data2010[c(6)]
    data2010 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data2010 == "CO")), length(which(data2010 == "NO2")), length(which(data2010 == "OZONE")), length(which(data2010 == "SO2")), length(which(data2010 == "PM2.5")), length(which(data2010 == "PM10"))))
    data2010 <- data2010 %>% mutate(Percent = percent(Days / sum(Days)))
    data2011 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2011)) 
    data2011 <- data2011[c(6)]
    data2011 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data2011 == "CO")), length(which(data2011 == "NO2")), length(which(data2011 == "OZONE")), length(which(data2011 == "SO2")), length(which(data2011 == "PM2.5")), length(which(data2011 == "PM10"))))
    data2011 <- data2011 %>% mutate(Percent = percent(Days / sum(Days)))
    data2012 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2012)) 
    data2012 <- data2012[c(6)]
    data2012 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data2012 == "CO")), length(which(data2012 == "NO2")), length(which(data2012 == "OZONE")), length(which(data2012 == "SO2")), length(which(data2012 == "PM2.5")), length(which(data2012 == "PM10"))))
    data2012 <- data2012 %>% mutate(Percent = percent(Days / sum(Days)))
    data2013 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2013)) 
    data2013 <- data2013[c(6)]
    data2013 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data2013 == "CO")), length(which(data2013 == "NO2")), length(which(data2013 == "OZONE")), length(which(data2013 == "SO2")), length(which(data2013 == "PM2.5")), length(which(data2013 == "PM10"))))
    data2013 <- data2013 %>% mutate(Percent = percent(Days / sum(Days)))
    data2014 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2014)) 
    data2014 <- data2014[c(6)]
    data2014 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data2014 == "CO")), length(which(data2014 == "NO2")), length(which(data2014 == "OZONE")), length(which(data2014 == "SO2")), length(which(data2014 == "PM2.5")), length(which(data2014 == "PM10"))))
    data2014 <- data2014 %>% mutate(Percent = percent(Days / sum(Days)))
    data2015 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2015)) 
    data2015 <- data2015[c(6)]
    data2015 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data2015 == "CO")), length(which(data2015 == "NO2")), length(which(data2015 == "OZONE")), length(which(data2015 == "SO2")), length(which(data2015 == "PM2.5")), length(which(data2015 == "PM10"))))
    data2015 <- data2015 %>% mutate(Percent = percent(Days / sum(Days)))
    data2016 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2016)) 
    data2016 <- data2016[c(6)]
    data2016 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data2016 == "CO")), length(which(data2016 == "NO2")), length(which(data2016 == "OZONE")), length(which(data2016 == "SO2")), length(which(data2016 == "PM2.5")), length(which(data2016 == "PM10"))))
    data2016 <- data2016 %>% mutate(Percent = percent(Days / sum(Days)))
    data2017 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2017)) 
    data2017 <- data2017[c(6)]
    data2017 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data2017 == "CO")), length(which(data2017 == "NO2")), length(which(data2017 == "OZONE")), length(which(data2017 == "SO2")), length(which(data2017 == "PM2.5")), length(which(data2017 == "PM10"))))
    data2017 <- data2017 %>% mutate(Percent = percent(Days / sum(Days)))
    data2018 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2018)) 
    data2018 <- data2018[c(6)]
    data2018 <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), "Days"= c(length(which(data2018 == "CO")), length(which(data2018 == "NO2")), length(which(data2018 == "OZONE")), length(which(data2018 == "SO2")), length(which(data2018 == "PM2.5")), length(which(data2018 == "PM10"))))
    data2018 <- data2018 %>% mutate(Percent = percent(Days / sum(Days)))
    # Create a new column in each of the data sets to represent each year.
    data1990$Year <- "1990"
    data1991$Year <- "1991"
    data1992$Year <- "1992"
    data1993$Year <- "1993"
    data1994$Year <- "1994"
    data1995$Year <- "1995"
    data1996$Year <- "1996"
    data1997$Year <- "1997"
    data1998$Year <- "1998"
    data1999$Year <- "1999"
    data2000$Year <- "2000"
    data2001$Year <- "2001"
    data2002$Year <- "2002"
    data2003$Year <- "2003"
    data2004$Year <- "2004"
    data2005$Year <- "2005"
    data2006$Year <- "2006"
    data2007$Year <- "2007"
    data2008$Year <- "2008"
    data2009$Year <- "2009"
    data2010$Year <- "2010"
    data2011$Year <- "2011"
    data2012$Year <- "2012"
    data2013$Year <- "2013"
    data2014$Year <- "2014"
    data2015$Year <- "2015"
    data2016$Year <- "2016"
    data2017$Year <- "2017"
    data2018$Year <- "2018"
    # Combine all the datasets into one
    data <- rbind(data1990,data1991,data1992,data1993,data1994,data1995,data1996,data1997,data1998,data1999,
                  data2000,data2001,data2002,data2003,data2004,data2005,data2006,data2007,data2008,data2009,
                  data2010,data2011,data2012,data2013,data2014,data2015,data2016,data2017,data2018)
  })
  ###################################################################################################################################
  # get data for showing location on a map
  map_values <- reactive({subset(sites, (sites$State.Name) == (input$State) & (sites$County.Name) == (input$County), select = Latitude:Longitude)})
  ###################################################################################################################################
  # Return the data for when CO is the main pollutant
  pieCO <- reactive({ data <- filter(dailyData, (State  ==  input$State)
                                     &    (County ==  input$County)
                                     &    (Year   ==  input$Year))
  
  # For the returned data frame only keep the column that we need in this case the "Parameter"
  data <- data[c(6)]
  
  # Update the data frame with the number of days for each Category
  data <- data.frame("Pollutant" = c("CO", "Other"),
                     "Days"= c(length(which(data == "CO")), sum(length(which(data == "NO2")), length(which(data == "Ozone")), 
                                                                length(which(data == "SO2")), length(which(data == "PM2.5")), length(which(data == "PM10")))))
  
  # From the Days column create a percentage column
  data <- data %>% mutate(Percent = percent(Days / sum(Days)))
  }) 
  ###################################################################################################################################
  # Return the data for when NO2 is the main pollutant
  pieNO2 <- reactive({ data <- filter(dailyData, (State  ==  input$State)
                                      &    (County ==  input$County)
                                      &    (Year   ==  input$Year))
  
  # For the returned data frame only keep the column that we need in this case the "Parameter"
  data <- data[c(6)]
  
  # Update the data frame with the number of days for each Category
  data <- data.frame("Pollutant" = c("NO2", "Other"),
                     "Days"= c(length(which(data == "NO2")), sum(length(which(data == "CO")), length(which(data == "Ozone")), 
                                                                 length(which(data == "SO2")), length(which(data == "PM2.5")), length(which(data == "PM10")))))
  
  # From the Days column create a percentage column
  data <- data %>% mutate(Percent = percent(Days / sum(Days)))
  }) 
  ###################################################################################################################################
  # Return the data for when Ozone is the main pollutant
  pieOzone <- reactive({ data <- filter(dailyData, (State  ==  input$State)
                                        &    (County ==  input$County)
                                        &    (Year   ==  input$Year))
  
  # For the returned data frame only keep the column that we need in this case the "Parameter"
  data <- data[c(6)]
  
  # Update the data frame with the number of days for each Category
  data <- data.frame("Pollutant" = c("Ozone", "Other"),
                     "Days"= c(length(which(data == "Ozone")), sum(length(which(data == "CO")), length(which(data == "NO2")), 
                                                                   length(which(data == "SO2")), length(which(data == "PM2.5")), length(which(data == "PM10")))))
  
  # From the Days column create a percentage column
  data <- data %>% mutate(Percent = percent(Days / sum(Days)))
  })
  ###################################################################################################################################
  # Return the data for when SO2 is the main pollutant
  pieSO2 <- reactive({ data <- filter(dailyData, (State  ==  input$State)
                                      &    (County ==  input$County)
                                      &    (Year   ==  input$Year))
  
  # For the returned data frame only keep the column that we need in this case the "Parameter"
  data <- data[c(6)]
  
  # Update the data frame with the number of days for each Category
  data <- data.frame("Pollutant" = c("SO2", "Other"),
                     "Days"= c(length(which(data == "SO2")), sum(length(which(data == "CO")), length(which(data == "NO2")), 
                                                                 length(which(data == "Ozone")), length(which(data == "PM2.5")), length(which(data == "PM10")))))
  
  # From the Days column create a percentage column
  data <- data %>% mutate(Percent = percent(Days / sum(Days)))
  })
  ###################################################################################################################################
  # Return the data for when PM2.5 is the main pollutant
  piePM2 <- reactive({ data <- filter(dailyData, (State  ==  input$State)
                                      &    (County ==  input$County)
                                      &    (Year   ==  input$Year))
  
  # For the returned data frame only keep the column that we need in this case the "Parameter"
  data <- data[c(6)]
  
  # Update the data frame with the number of days for each Category
  data <- data.frame("Pollutant" = c("PM2.5", "Other"),
                     "Days"= c(length(which(data == "PM2.5")), sum(length(which(data == "CO")), length(which(data == "NO2")), 
                                                                   length(which(data == "Ozone")), length(which(data == "SO2")), length(which(data == "PM10")))))
  
  # From the Days column create a percentage column
  data <- data %>% mutate(Percent = percent(Days / sum(Days)))
  }) 
  ###################################################################################################################################
  # Return the data for when PM10 is the main pollutant
  piePM10 <- reactive({ data <- filter(dailyData, (State  ==  input$State)
                                       &    (County ==  input$County)
                                       &    (Year   ==  input$Year))
  
  # For the returned data frame only keep the column that we need in this case the "Parameter"
  data <- data[c(6)]
  
  # Update the data frame with the number of days for each Category
  data <- data.frame("Pollutant" = c("PM10", "Other"),
                     "Days"= c(length(which(data == "PM10")), sum(length(which(data == "CO")), length(which(data == "NO2")), 
                                                                  length(which(data == "Ozone")), length(which(data == "SO2")), length(which(data == "PM2.5")))))
  
  # From the Days column create a percentage column
  data <- data %>% mutate(Percent = percent(Days / sum(Days)))
  }) 
  ###################################################################################################################################
  # Return the data for the number of CO, NO2, Ozone, SO2, PM2.5, PM10 days as the main pollutant
  piePollutant <- reactive({ data <- filter(dailyData, (State  ==  input$State)
                                            &    (County ==  input$County)
                                            &    (Year   ==  input$Year))
  
  # For the returned data frame only keep the column that we need in this case the "Parameter"
  data <- data[c(6)]
  
  # Update the data frame with the number of days for each Category
  data <- data.frame("Pollutant" = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"),
                     "Days"= c(length(which(data == "CO")), length(which(data == "NO2")), length(which(data == "OZONE")), 
                               length(which(data == "SO2")), length(which(data == "PM2.5")), length(which(data == "PM10"))))
  
  # From the Days column create a percentage column
  data <- data %>% mutate(Percent = percent(Days / sum(Days)))
  }) 
  ###################################################################################################################################
  # Output all of the Pollutant pie charts
  output$pieChartCO <- renderPlot({
    ggplot(pieCO(), aes(x = "", y = Days, fill = fct_inorder(Pollutant))) +
      scale_fill_manual(values = c("CO" = "green",
                                   "Other" = "firebrick1")) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      geom_label_repel(aes(label = Percent), size=5, show.legend = F, nudge_x = 2) +
      guides(fill = guide_legend(title = "Category")) 
  })
  
  output$pieChartNO2 <- renderPlot({
    ggplot(pieNO2(), aes(x = "", y = Days, fill = fct_inorder(Pollutant))) +
      scale_fill_manual(values = c("NO2" = "green",
                                   "Other" = "firebrick1")) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      geom_label_repel(aes(label = Percent), size=5, show.legend = F, nudge_x = 2) +
      guides(fill = guide_legend(title = "Category")) 
  })
  
  output$pieChartOZONE <- renderPlot({
    ggplot(pieOzone(), aes(x = "", y = Days, fill = fct_inorder(Pollutant))) +
      scale_fill_manual(values = c("Ozone" = "green",
                                   "Other" = "firebrick1")) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      geom_label_repel(aes(label = Percent), size=5, show.legend = F, nudge_x = 2) +
      guides(fill = guide_legend(title = "Category")) 
  })
  
  output$pieChartSO2 <- renderPlot({
    ggplot(pieSO2(), aes(x = "", y = Days, fill = fct_inorder(Pollutant))) +
      scale_fill_manual(values = c("SO2" = "green",
                                   "Other" = "firebrick1")) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      geom_label_repel(aes(label = Percent), size=5, show.legend = F, nudge_x = 2) +
      guides(fill = guide_legend(title = "Category")) 
  })
  
  output$pieChartPM2 <- renderPlot({
    ggplot(piePM2(), aes(x = "", y = Days, fill = fct_inorder(Pollutant))) +
      scale_fill_manual(values = c("PM2.5" = "green",
                                   "Other" = "firebrick1")) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      geom_label_repel(aes(label = Percent), size=5, show.legend = F, nudge_x = 2) +
      guides(fill = guide_legend(title = "Category")) 
  })
  
  output$pieChartPM10 <- renderPlot({
    ggplot(piePM10(), aes(x = "", y = Days, fill = fct_inorder(Pollutant))) +
      scale_fill_manual(values = c("PM10" = "green",
                                   "Other" = "firebrick1")) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      geom_label_repel(aes(label = Percent), size=5, show.legend = F, nudge_x = 2) +
      guides(fill = guide_legend(title = "Category")) 
  })
  ###################################################################################################################################
  output$pieChartAQI <- renderPlot({
    ggplot(AQIstatus(), aes(x = "", y = Days, fill = fct_inorder(AQI))) +
      scale_fill_manual(values = c("Good" = "green",
                                   "Moderate" = "yellow",
                                   "Unhealthy for Sensitive" = "goldenrod1",
                                   "Unhealthy" = "pink",
                                   "Very Unhealthy" = "red",
                                   "Hazardous" = "red4")) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      geom_label_repel(aes(label = Percent), size=5, show.legend = F, nudge_x = 2) +
      guides(fill = guide_legend(title = "Category")) 
    
  })
  
  # Output dynamic AQI percent bar chart
  output$barChartAQI <- renderPlot({
    ggplot(data = AQIstatus()) + geom_bar(mapping= aes(x=AQI, y=Days, fill= fct_inorder(AQI)) , stat = "identity", width = 1) +
      scale_x_discrete(breaks = 1:6, labels=c("","","","","","")) +
      scale_fill_manual("Category", values = c("Good" = "green",
                                               "Moderate" = "yellow",
                                               "Unhealthy for Sensitive" = "goldenrod1",
                                               "Unhealthy" = "pink",
                                               "Very Unhealthy" = "red",
                                               "Hazardous" = "red4")) 
  })
  
  # Output dynamic AQI percent table
  output$tableAQI <- DT::renderDataTable(
    DT::datatable(
      {AQIstatus()},
      options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(1, 'asc'))
      ) 
    )
  )
  
  # Output dynamic Pollutant percent bar chart
  output$barChartPollutant <- renderPlot({
    ggplot(data = piePollutant()) + geom_bar(mapping= aes(x=Pollutant, y=Days, fill= fct_inorder(Pollutant)) , stat = "identity", width = 1) +
      scale_x_discrete(breaks = 1:6, labels=c("","","","","","")) +
      scale_fill_manual("Category", values = c("CO" = "green",
                                               "NO2" = "yellow",
                                               "Ozone" = "goldenrod1",
                                               "SO2" = "pink",
                                               "PM2.5" = "red",
                                               "PM10" = "red4")) 
  })
  
  # Output dynamic Pollutant percent table
  output$tablePollutant <- DT::renderDataTable(
    DT::datatable(
      {piePollutant()},
      options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(1, 'asc'))
      ) 
    )
  )
  
  # Output Max, Min, 90
  output$lineChart <- renderPlot({
    lineData <- AQIvalue()
    ggplot(data = lineData, mapping = aes(x = Year, y = AQI, color = Group)) +
      geom_line()
  })
  
  # Output AQI type percentages
  output$lineChart2 <- renderPlot({
    lineData <- ParameterPercent()
    ggplot(data = lineData, mapping = aes(x = Year, y = Percent, group = Pollutant, color = Pollutant)) +
     geom_line()
  })
  
  
  output$lineChartAQI <- renderPlot({
    lineData <- filteredData()
    ggplot() +
      geom_point(data=lineData, aes(x=Date, y=AQI, fill=Parameter), size=4, shape=21) +
      geom_line(data=lineData, aes(x=Date, y=AQI))
  })
  
  # map to show the location of the selected area
  output$leaf <- renderLeaflet({
    map <- leaflet()
    map <- addTiles(map)
    map_data <- map_values()
    map <- setView(map, lng = map_data[1, 2], lat = map_data[1, 1], zoom = 9)
    map <- addMarkers(map, lng = map_data[1, 2], lat = map_data[1, 1], popup = "Here")
    map
  })
  
  output$stackedBarChartAQI <- renderPlot({
    stackedBarData <- filteredData()
    ggplot() + geom_bar(aes(y = AQI, x = fct_inorder(months.Date(Date)), fill = Category), data = stackedBarData,
                        stat="identity")
  })
  
  output$tableMonthlyAQI <- DT::renderDataTable(
    DT::datatable(
      {monthDataAQI <- group_by(monthsData, month(Date))
      janData <- subset(monthDataAQI, month(Date) == 1)
      janVec <- janData[, "Category"]
      janFrame <- as.data.frame(table(janVec))
      cast(janFrame, Freq ~ janVec)
      
      febData <- subset(monthDataAQI, month(Date) == 2)
      febVec <- febData[, "Category"]
      febFrame <- as.data.frame(table(febVec))
      cast(febFrame, Freq ~ febVec)
      
      marchData <- subset(monthDataAQI, month(Date) == 3)
      marchVec <- marchData[, "Category"]
      marchFrame <- as.data.frame(table(marchVec))
      cast(marchFrame, Freq ~ marchVec)
      
      aprilData <- subset(monthDataAQI, month(Date) == 4)
      aprilVec <- aprilData[, "Category"]
      aprilFrame <- as.data.frame(table(aprilVec))
      cast(aprilFrame, Freq ~ aprilVec)
      
      mayData <- subset(monthDataAQI, month(Date) == 5)
      mayVec <- mayData[, "Category"]
      mayFrame <- as.data.frame(table(mayVec))
      cast(mayFrame, Freq ~ mayVec)
      
      juneData <- subset(monthDataAQI, month(Date) == 6)
      juneVec <- juneData[, "Category"]
      juneFrame <- as.data.frame(table(juneVec))
      cast(juneFrame, Freq ~ juneVec)
      
      julyData <- subset(monthDataAQI, month(Date) == 7)
      julyVec <- julyData[, "Category"]
      julyFrame <- as.data.frame(table(julyVec))
      cast(julyFrame, Freq ~ julyVec)
      
      augData <- subset(monthDataAQI, month(Date) == 8)
      augVec <- augData[, "Category"]
      augFrame <- as.data.frame(table(augVec))
      cast(augFrame, Freq ~ augVec)
      
      septData <- subset(monthDataAQI, month(Date) == 9)
      septVec <- septData[, "Category"]
      septFrame <- as.data.frame(table(septVec))
      cast(septFrame, Freq ~ septVec)
      
      octData <- subset(monthDataAQI, month(Date) == 10)
      octVec <- octData[, "Category"]
      octFrame <- as.data.frame(table(octVec))
      cast(octFrame, Freq ~ octVec)
      
      novData <- subset(monthDataAQI, month(Date) == 11)
      novVec <- novData[, "Category"]
      novFrame <- as.data.frame(table(novVec))
      cast(novFrame, Freq ~ novVec)
      
      decData <- subset(monthDataAQI, month(Date) == 12)
      decVec <- decData[, "Category"]
      decFrame <- as.data.frame(table(decVec))
      cast(decFrame, Freq ~ decVec)
      
      do.call(rbind, list(janFrame = janFrame, febFrame = febFrame, marchFrame = marchFrame, aprilFrame = aprilFrame, mayFrame = mayFrame, juneFrame = juneFrame, julyFrame = julyFrame, augFrame = augFrame, septFrame = septFrame, novFrame = novFrame, decFrame = decFrame))
      })
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
