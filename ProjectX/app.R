#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(countrycode)
library(hash)

overall <- read.csv('global_power_plant_database.csv')
data <- overall %>% select(country, country_long, name, capacity_mw, latitude, longitude, primary_fuel,  estimated_generation_gwh)


continents <- c('North America', 'South America', 'Africa', 'Europe', 'Asia', 'Oceania', 'Antarctica')

sources <- c('Biomass', 'Coal', 'Cogeneration', 'Gas', 'Geothermal', 'Hydro', 'Nuclear', 'Oil', 'Other', 'Petcoke', 'Solar', 'Storage', 'Waste', 'Wave and Tidal', 'Wind')
colors_list <- c('#9bd245', '#d35c51', '#914221', '#579214', '#1c529b', '#8c297c', '#6213fa', '#0ce25a', '#de2d4b', '#84d4ef', '#d6db7e', '#452fa3', '#e5e5c8', '#898f29', '#670149')

data$continent <- countrycode(sourcevar = data[, "country_long"], origin = "country.name", destination = "continent")

data$continent[13997] <- 'Europe'
data$continent[13998] <- 'Europe'

data <- data %>% replace_na(list(continent = 'Antarctica', estimated_generation_gwh = 0))

source_colors <- hash()
source_colors[['Biomass']] <- '#9bd245'
source_colors[['Coal']] <- '#d35c51'
source_colors[['Cogeneration']] <- '#914221'
source_colors[['Gas']] <- '#579214'
source_colors[['Geothermal']] <- '#1c529b'
source_colors[['Hydro']] <- '#8c297c'
source_colors[['Nuclear']] <- '#6213fa'
source_colors[['Oil']] <- '#0ce25a'
source_colors[['Other']] <- '#de2d4b'
source_colors[['Petcoke']] <- '#84d4ef'
source_colors[['Solar']] <- '#d6db7e'
source_colors[['Storage']] <- '#452fa3'
source_colors[['Waste']] <- '#e5e5c8'
source_colors[['Wave and Tidal']] <- '#898f29'
source_colors[['Wind']] <- '#670149'


# 
# data$capacity_mw <- as.numeric(data$capacity_mw)
# data$data$latitude <- as.numeric(data$latitude)
# data$data$longitude <- as.numeric(data$longitude)
# data$data$estimated_generation_gwh <- as.numeric(data$estimated_generation_gwh)





# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    #create dashboard and elements
    dashboardHeader(title = "CS 424 Project X"),
    
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     #menu bar with world panel and about page
                     sidebarMenu(
                         menuItem("World Plantations", tabName = "World", icon = NULL),
                         menuItem("About Page", tabName = "AboutPage", icon = NULL)
                     )
    ),
    dashboardBody(
        tabItems(
                #country comparison tab modelled after the first tab but includes a bar to sort through the data 
            #contains a checkboxgroup, year, and slider for manipulating the data
            tabItem(tabName = "World",
                    fluidRow(
                        column(10, offset=1,
                               checkboxGroupInput("World_Energy",
                                                  h3("Select Plantation Type:"),
                                                  choices = c('All', sources),
                                                  selected = "All",
                                                  inline = TRUE
                               )),
                        
                        column(1),
                        
                        
                        
                        column(2, offset=1,
                               selectInput("World_Continent", h5("Select Continent Focus:"),
                                           choices = continents,
                                           selected = 'North America')),
                        
                        column(2,
                               sliderInput("World_Production", "Plant Production Range: (MWh):", 
                                           min = 0, max = 22500,
                                           value = c(0, 22500),
                                           width = 400)
                        ), 
                        
                    ),
                    box(title = "Leaflet World", solidHeader = TRUE, status = "primary", width = 13,
                        leafletOutput("World_Leaflet", height = 650)
                        
                    )
            ),
            
            #about page to display necessary info
            tabItem(tabName = "AboutPage",
                    h2("About Page"),
                    verbatimTextOutput("AboutOut")
            )
        )
    )
)

get_plant_popup <- function(data) {
    
    plant_info <- "" 
        plant_info <- paste(
            paste("Plant Name: ", data$name, sep =''),
            paste("Country Name: ", data$country_long, sep =''),
            paste("Capacity:", data$capacity_mw, sep =''), 
            paste("Primary Source: ", data$primary_fuel, sep =''), 
            sep="<br>")
    return (plant_info)
    
}
    

# Define server logic required to draw a histogram
server <- function(input, output) {

    observe({
            
        world_lat = 40.5260
        world_lng = -100.2551
        world_view = 4
        
        map <- leaflet(data)  %>%
            
            addProviderTiles(
                providers$CartoDB.Positron, group = "Light")   %>% 
            addProviderTiles(
                providers$CartoDB.Positron, group = "Dark")   %>% 
            
            setView(
                lat = world_lat,
                lng = world_lng,
                zoom = world_view
            ) %>%
            
            addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[1] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[1], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[1]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[1] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[1], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
            addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[2] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[2], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[2]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[2] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[2], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
            addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[3] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[3], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[3]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[3] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[3], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
            addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[4] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[4], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[4]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[4] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[4], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
            addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[5] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[5], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[5]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[5] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[5], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
            addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[6] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[6], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[6]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[6] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[6], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
            addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[7] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[7], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[7]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[7] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[7], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
            addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[8] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[8], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[8]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[8] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[8], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
            addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[9] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[9], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[9]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[9] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[9], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
            addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[10] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[10], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[10]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[10] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[10], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
            addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[11] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[11], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[11]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[11] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[11], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
            addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[12] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[12], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[12]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[12] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[12], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
            addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[13] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[13], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[13]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[13] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[13], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
            addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[14] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[14], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[14]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[14] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[14], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
            addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[15] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[15], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[15]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[15] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[15], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
            
            
            
            
            
            
            addLegend("bottomright", colors = colors_list,  labels=sources, title="Energy Source") 
        
        
        output$World_Leaflet <- renderLeaflet({
            map
        })
        
    })
    
    observeEvent(input$World_Continent, {
        
        
        
        world_lat = 40.5260
        world_lng = -100.2551
        world_view = 4
        
        cont <- input$World_Continent
        
        if(input$World_Continent == 'North America') {
            world_lat = 40.5260
            world_lng = -100.2551
            world_view = 4
            cont <- "Americas"
        } else if (input$World_Continent == 'South America') {
            world_lat = 0
            world_lng = -55.4915
            world_view = 3
            cont <- "Americas"
        }  else if (input$World_Continent == 'Africa') {
            world_lat = 8.7832
            world_lng = 34.5085
            world_view = 3
        }  else if (input$World_Continent == 'Europe') {
            world_lat = 54.5260
            world_lng = 15.2551
            world_view = 4
        }  else if (input$World_Continent == 'Asia') {
            world_lat = 34.0479
            world_lng = 100.6197
            world_view = 3
        }  else if (input$World_Continent == 'Oceania') {
            world_lat = -22.7359
            world_lng = 140.0188
            world_view = 4
        } else if (input$World_Continent == 'Antarctica') {
            world_lat = -77.8470
            world_lng = 166.6605
            world_view = 2
        }
        
        
        leafletProxy("World_Leaflet") %>% setView(lat = world_lat, lng = world_lng, zoom = world_view)
    })
    
    
    observeEvent(input$World_Energy, {
       
        leafletProxy(mapId="World_Leaflet") %>% clearMarkers()
        
        if ("All" %in% input$World_Energy) {
            leafletProxy(mapId="World_Leaflet") %>%
                addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[1] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[1], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[1]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[1] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[1], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
                addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[2] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[2], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[2]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[2] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[2], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
                addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[3] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[3], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[3]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[3] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[3], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
                addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[4] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[4], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[4]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[4] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[4], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
                addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[5] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[5], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[5]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[5] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[5], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
                addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[6] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[6], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[6]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[6] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[6], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
                addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[7] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[7], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[7]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[7] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[7], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
                addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[8] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[8], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[8]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[8] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[8], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
                addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[9] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[9], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[9]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[9] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[9], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
                addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[10] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[10], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[10]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[10] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[10], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
                addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[11] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[11], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[11]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[11] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[11], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
                addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[12] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[12], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[12]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[12] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[12], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
                addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[13] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[13], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[13]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[13] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[13], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
                addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[14] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[14], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[14]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[14] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[14], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) %>%
                addCircleMarkers(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[15] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[15], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[15]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[15] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[15], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) 
        }
        
        
        for (source in sources) {
            if (source %in% input$World_Energy & !("All" %in% input$World_Energy)) {
                leafletProxy(mapId="World_Leaflet") %>%
                    addCircles(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (source %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel ==source, ],  lng=~longitude, lat=~latitude, color = source_colors[[source]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (source %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == source, ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1)                
            }
        }
        
        
        # if(sources[1] %in% input$World_Energy & !("All" %in% input$World_Energy)) {
            # leafletProxy(mapId="World_Leaflet") %>%
            #     addCircles(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[1] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[1], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[1]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[1] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[1], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1)
        # }
        # 
        # if(sources[2] %in% input$World_Energy & !("All" %in% input$World_Energy)) {
        #     leafletProxy(mapId="World_Leaflet") %>%
        #         addCircles(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[2] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[1], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[2]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[1] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[2], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1)
        # }
        # 
        # if(sources[3] %in% input$World_Energy & !("All" %in% input$World_Energy)) {
        #     leafletProxy(mapId="World_Leaflet") %>%
        #         addCircles(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[3] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[3], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[3]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[3] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[3], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1)
        # }
        # 
        # if(sources[4] %in% input$World_Energy & !("All" %in% input$World_Energy)) {
        #     leafletProxy(mapId="World_Leaflet") %>%
        #         addCircles(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[4] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[4], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[4]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[4] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[4], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1)
        # }
        # 
        # if(sources[5] %in% input$World_Energy & !("All" %in% input$World_Energy)) {
        #     leafletProxy(mapId="World_Leaflet") %>%
        #         addCircles(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[5] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[5], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[5]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[5] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[5], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1)
        # }
        # 
        # if(sources[6] %in% input$World_Energy & !("All" %in% input$World_Energy)) {
        #     leafletProxy(mapId="World_Leaflet") %>%
        #         addCircles(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[6] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[6], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[6]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[6] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[6], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) 
        # }
        # 
        # if(sources[7] %in% input$World_Energy & !("All" %in% input$World_Energy)) {
        #     leafletProxy(mapId="World_Leaflet") %>%
        #         addCircles(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[6] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[6], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[6]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[6] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[6], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) 
        # }
        # 
        # if(sources[8] %in% input$World_Energy & !("All" %in% input$World_Energy)) {
        #     leafletProxy(mapId="World_Leaflet") %>%
        #         addCircles(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[6] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[6], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[6]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[6] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[6], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) 
        # }
        # 
        # if(sources[9] %in% input$World_Energy & !("All" %in% input$World_Energy)) {
        #     leafletProxy(mapId="World_Leaflet") %>%
        #         addCircles(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[6] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[6], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[6]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[6] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[6], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) 
        # }
        # 
        # if(sources[10] %in% input$World_Energy & !("All" %in% input$World_Energy)) {
        #     leafletProxy(mapId="World_Leaflet") %>%
        #         addCircles(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[6] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[6], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[6]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[6] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[6], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) 
        # }
        # 
        # if(sources[11] %in% input$World_Energy & !("All" %in% input$World_Energy)) {
        #     leafletProxy(mapId="World_Leaflet") %>%
        #         addCircles(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[6] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[6], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[6]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[6] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[6], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) 
        # }
        # 
        # if(sources[12] %in% input$World_Energy & !("All" %in% input$World_Energy)) {
        #     leafletProxy(mapId="World_Leaflet") %>%
        #         addCircles(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[6] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[6], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[6]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[6] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[6], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) 
        # }
        # 
        # if(sources[13] %in% input$World_Energy & !("All" %in% input$World_Energy)) {
        #     leafletProxy(mapId="World_Leaflet") %>%
        #         addCircles(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[6] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[6], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[6]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[6] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[6], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) 
        # }
        # 
        # if(sources[14] %in% input$World_Energy & !("All" %in% input$World_Energy)) {
        #     leafletProxy(mapId="World_Leaflet") %>%
        #         addCircles(data=data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[6] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[6], ],  lng=~longitude, lat=~latitude, color = source_colors[[sources[6]]], group = ~primary_fuel, popup=get_plant_popup(data[(data$capacity_mw <= input$World_Production[2] & data$capacity_mw >= input$World_Production[1]) & (sources[6] %in% input$World_Energy | 'All' %in% input$World_Energy) & data$primary_fuel == sources[6], ]), fillOpacity = 0.3, stroke = FALSE, radius = ~log(capacity_mw) , weight = 1) 
        # }
        
        
        
        
    })
    
    
    
    output$AboutOut <- renderText({
        "Created by: Vivek Bhatt\n
         Created: 4/30/2021\n
         Data Source: https://datasets.wri.org/dataset/globalpowerplantdatabase\n
         Intended for visualizing the energy sources of the world."   
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
