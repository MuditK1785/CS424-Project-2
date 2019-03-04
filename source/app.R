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
      menuItem("Part C 1st Bullet Point", tabName = "P1"), # Dashboard displays all county data, charts, and graphs
      menuItem("Part C 3rd Bullet Point", tabName = "3BP"), # Choose year and county then be able to see daily AQI line chart, bar chart, and table
      menuItem("Part C 4th Bullet Point", tabName = "map"), # Displays the map for top 100 counties of AQI or a pollutant type
      menuItem("About", tabName = "about"), # Find more about this project.
      
      conditionalPanel("input.sidebarmenu === 'P1' || input.sidebarmenu === '3BP'",
                       selectizeInput("State", "Select State", choices = unique(listOfCounties$State), selected = "Illinois"),
                       selectizeInput("County", "Select County", choices = unique(listOfCounties$County), selected = "Cook"),
                       
                       # Set up the year select input
                       selectizeInput("Year", "Select the Year to See Data For", choices = unique(listOfYears$Year), selected = "2018")
      ),
      conditionalPanel("input.sidebarmenu === 'map'",
                       # Set up the year select input
                       selectizeInput("mapYear", "Select the Year to See Data For", choices = unique(listOfYears$Year), selected = "2018"),
                       selectizeInput("Type", "Pick how you want to categorize the map (AQI or a Pollutant)", choices = unique((aqiPollutantList)), selected = "AQI")
                       
      )
      # Set up 2 way search, By State -> County, By Alphabetical List or Type To Search
      
    )
  ),
  
  # Set up the dashboard body
  dashboardBody(
    
    tabItems(
      tabItem("P1",
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
                
                #box(title = "AQI Category from 1980-2018", solidHeader = TRUE, status = "primary", width = NULL, plotOutput("line1"))
              )
      ),
      tabItem("3BP",
              fluidRow(
                box( title = "Daily AQI Data Line Chart with Leading Pollutant", solidHeader = TRUE, status = "primary", width = 12, plotOutput("lineChartAQI"))
              )
      ),
      tabItem("about",
              a("Find out more about this project on the official website", href="")
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
  # Return the AQI values for the users selection
  
  #PART B FOR PROJECT 1 MATERIAL FIND MAX, still under construction.
  AQIvalue <- reactive({ 
    
  data1990 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1990))
  data1991 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1991))
  data1992 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1992))
  data1993 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1993))
  data1994 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1994))
  data1995 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1995))
  data1996 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1996))
  data1997 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1997))
  data1998 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1998))
  data1999 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 1999))
  data2000 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2000))
  data2001 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2001))
  data2002 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2002))
  data2003 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2003))
  data2004 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2004))
  data2005 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2005))
  data2006 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2006))
  data2007 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2007))
  data2008 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2008))
  data2009 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2009))
  data2010 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2010))
  data2011 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2011))
  data2012 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2012))
  data2013 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2013))
  data2014 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2014))
  data2015 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2015))
  data2016 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2016))
  data2017 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2017))
  data2018 <- filter(dailyData, (State == input$State) & (County == input$County) & (Year == 2018))
  
  data <- rbind(data1990,data1991,data1992,data1993,data1994,data1995,data1996,data1997,data1998,data1999,
                data2000,data2001,data2002,data2003,data2004,data2005,data2006,data2007,data2008,data2009,
                data2010,data2011,data2012,data2013,data2014,data2015,data2016,data2017,data2018)
  
  })
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
                               length(which(data == "SO2")), length(which(data == "PM2.5")), length(which(data == "PM210"))))
  
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
  
  output$lineChartAQI <- renderPlot({
    lineData <- filteredData()
    ggplot() +
      geom_point(data=lineData, aes(x=Date, y=AQI, fill=Parameter), size=4, shape=21) +
      geom_line(data=lineData, aes(x=Date, y=AQI))
  })
  
  
}
  
# Run the application 
shinyApp(ui = ui, server = server)

