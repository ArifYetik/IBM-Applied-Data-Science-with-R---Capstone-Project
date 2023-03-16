# Load required libraries
library(leaflet)
library(raster)
library(dplyr)
# Create a RShiny UI
shinyUI(
  fluidPage(padding=5,
  titlePanel("Bike-Sharing Demand Prediction"), 
  # Create a side-bar layout
  sidebarLayout(
    # Create a main panel to show cities on a leaflet map
    mainPanel(
      # leaflet output with id = 'city_bike_map', height = 1000
      leafletOutput("city_bike_map", height = 1000)
    ),
    # Create a side bar to show detailed plots for a city
    sidebarPanel(
      # select drop down list to select city
      selectInput(inputId="city_dropdown", label = "Cities:", 
                  choices = c("All", "Seoul", "Suzhou", "London", "New York", "Paris")),
      plotOutput("temp_line", height=200,width=400),
      br(),
      plotOutput("bike_prediction",height=200,width=400,click = "plot_click"),
      verbatimTextOutput("bike_date_output"),
      
      plotOutput("humidity_pred_chart", height=200,width=400)
    ))
))



