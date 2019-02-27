######################################################################################################################################
# Authors: Rami Masoud, Jeet Roal, Mudit Kumar, Jonathan Munoz                                                                       #
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
library(readr)
library(fst)
library(geojson)
# library(geojsonio)
library(leaflet)
# Read in the 2 pre-processed feather files
dailyData  <- read_fst('dailyData.fst')
# hourlyData <- read_fst('hourlyData.fst')

#select state and county columns
counties <- dailyData[c(1,2)]

Date <- dailyData[c(5)]

# Combine the Counties and States to create another column.
counties$CountyXState <- paste(counties$`county Name`, counties$`State Name`, sep=', ')

# Read in the location data for the map using the aqs_sites.csv
sites <- read.table(file = "aqs_sites.csv", sep=",",header = TRUE)

# Create the menu items to select the different years
years<-c(1990:2018)

county_states <- c("Cook, Illinois", "Hawaii, Hawaii","New York, New York","Los Angeles, California","King, Washington", "Harris, Texas", 
                   "Miami-Dade, Florida", "San Juan, New Mexico","Hennepin, Minnesota", "Wake, North Carolina", "Elko, Nevada","Ottawa, Ohio")


# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("proj2"),
   
   dashboardSidebar(
     selectInput("Year", "Select the year to visualize", years, selected = 2018),
     selectInput("County", "Select the county to visualize", county_states, selected = "Cook, Illinois")
   ),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(

      ),
      
      # Show a plot of the generated distribution
      mainPanel(
      
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # calculate the values one time and re-use them in multiple charts to speed things up
  justOneYearReactive <- reactive({subset(dailyData, year(dailyData$Date) == input$Year)})
   
}

# Run the application 
shinyApp(ui = ui, server = server)

