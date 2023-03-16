# Install and import required libraries
library(shiny)
library(lubridate)
library(ggthemes)
library(tidymodels)
library(scales)
library(glmnet)
library(httr)
library(leaflet)
library(raster)
library(terra)
# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")

test_weather_data_generation<-function(){
  #Test generate_city_weather_bike_data() function
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}


# Create a RShiny server
shinyServer(function(input, output){
  # Define a city list
 
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  
  city_weather_bike_df <- test_weather_data_generation()
  
  city_weather_bike_df$FORECASTDATETIME=ymd_hms(city_weather_bike_df$FORECASTDATETIME)
  city_weather_bike_df$DATE=date(city_weather_bike_df$FORECASTDATETIME)
  
  # Create another data frame called `cities_max_bike` with each row contains city location info and max bike
  # prediction for the city
  cities_max_bike <- city_weather_bike_df %>% group_by(CITY_ASCII,DATE) %>% arrange(desc(BIKE_PREDICTION)) %>% slice(1) 
  
  cities_today_status=cities_max_bike %>% filter(DATE==min(cities_max_bike$DATE))
  cities_today_status=cities_today_status %>% mutate(CIRCLE=case_when(BIKE_PREDICTION_LEVEL=="large"~12,
                                                                      BIKE_PREDICTION_LEVEL=="medium"~10,
                                                                      TRUE~6))
  cities_today_status=cities_today_status %>% mutate(COLOR=case_when(BIKE_PREDICTION_LEVEL=="large"~"red",
                                                                     BIKE_PREDICTION_LEVEL=="medium"~"yellow",
                                                                     TRUE~"green"))
    
  # Observe drop-down event
    observeEvent(input$city_dropdown, {
      if(input$city_dropdown == 'All') {
        # If All was selected from dropdown, then render a leaflet map with circle markers
        # and popup weather LABEL for all five cities
        output$city_bike_map <- renderLeaflet({
          leaflet(cities_max_bike) %>% addTiles()%>%
            addCircleMarkers(data = cities_max_bike, lng = cities_max_bike$LNG, lat = cities_max_bike$LAT, 
                             popup = cities_max_bike$LABEL,
                             radius= ~ifelse(cities_max_bike$BIKE_PREDICTION_LEVEL=='small', 6, 12),
                             color = ~color_levels(cities_max_bike$BIKE_PREDICTION_LEVEL))
        })}
        
        #Render the city overview map
      
      else {
        selected_city=reactive({ cities_today_status %>% filter(CITY_ASCII==input$city_dropdown) }) 
        
        selected_city_5_day=reactive({city_weather_bike_df %>% filter(CITY_ASCII==input$city_dropdown)})
    }
 
      selected_city=reactive({ cities_today_status %>% filter(CITY_ASCII==input$city_dropdown) })
  
      # Then render output plots with an id defined in ui.R
     
      output$city_bike_map=renderLeaflet ({
        leaflet() %>% addTiles() %>% setView(lng=selected_city()$LNG, lat=selected_city()$LAT, zoom=15) %>% 
          addMarkers(lng=selected_city()$LNG, lat=selected_city()$LAT, 
                     popup=selected_city()$DETAILED_LABEL)
      })
      # Complete this function to render a leaflet map
    
    
    
  # If just one specific city was selected, then render a leaflet map with one marker
  # on the map and a popup with DETAILED_LABEL displayed
    
      output$temp_line=renderPlot({
        ggplot(selected_city_5_day(),aes(x=FORECASTDATETIME,y=TEMPERATURE))+geom_line(color="brown")+
          geom_point(color="red")+geom_text(aes(label=TEMPERATURE),size=3)+
          labs(title=paste("Temperature forcast of next 5 days in",input$city_dropdown))+xlab('Date (3hrs interval)')+
          ylab("Temperature in C")+theme_clean() })
      
      output$bike_prediction=renderPlot({
        ggplot(selected_city_5_day(),aes(x=FORECASTDATETIME,y=BIKE_PREDICTION))+geom_line(color="blue")+
          geom_point(color="blue")+geom_text(aes(label=BIKE_PREDICTION),size=3)+
          labs(title=paste("Bike prediction of next 5 days in",input$city_dropdown))+xlab('Date (3hrs interval)')+
          ylab("Bike No.")+theme_clean() })
      
      output$bike_date_output=renderText({paste0("Time=", as_datetime(input$plot_click$x),
                                                 "\nBike Count Prediction=", input$plot_click$y) })
      
      output$humidity_pred_chart=renderPlot({
        ggplot(selected_city_5_day(),aes(x=HUMIDITY,y=BIKE_PREDICTION))+
          geom_smooth(method=lm,formula=y~poly(x,4),color="green")+
          geom_point(color="brown")+
          labs(title=paste("Relationship between Humidity & Bike prediction in",input$city_dropdown))+xlab('Humidity')+
          ylab("Bike No.")+theme_clean() })
        
    })  
    
})


