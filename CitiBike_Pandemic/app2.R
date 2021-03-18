# This is a Shiny web application. You can run the application by clicking the 'Run App' button above.
# It displays the locations and analyzes the utilization of Citi Bike in NYC.

#### API ####   

## CitiBike NYC https://www.citibikenyc.com/system-data
#location, status and current availability for all stations in the New York City bike sharing imitative. 
library(tidyverse)
library(jsonlite)
library(leaflet)

## station_info- lat/long
station_information <- fromJSON("https://gbfs.citibikenyc.com/gbfs/en/station_information.json")
info <-station_information$data$stations

station_status<- fromJSON("https://gbfs.citibikenyc.com/gbfs/en/station_status.json")
status <- station_status$data$stations

system_regions <- fromJSON("https://gbfs.citibikenyc.com/gbfs/en/system_regions.json")
region <-system_regions$data$regions

## merge all files
citibikedt<- info %>% 
    merge(status,by="station_id") %>% 
    merge(region, by="region_id")



#### SHINY APP #### 
library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Location and Usage of CitiBike NYC"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput('region', 'Select Regions', selected= "NYC District", choices = c("NYC District","JC District")),
            checkboxGroupInput('status', 'Select Station Status', selected= "active", choices = c("active","out_of_service" )),
            plotOutput("hist_utilization")
        ),
        # Show a map location
        mainPanel(

          leafletOutput("mapPlot",height = 550, width = 800),
          tableOutput("summary"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    dt <- reactive({
        citibikedt %>%
            filter(name.y %in% input$region &
                   station_status %in% input$status)
    
        
    })

        dt_summary<-reactive({
            dt() %>% 
                # group_by(Region=input$region) %>% 
                summarise(
                    `Total(bikes+ebikes)`= sum(capacity),
                    `Available(bikes)`= sum(num_bikes_available),
                    `Available(ebikes)`= sum(num_ebikes_available)) %>% 
                mutate(
                    `Utilization Rate`= (`Available(bikes)`+`Available(ebikes)`)/`Total(bikes+ebikes)`) 
    })
        dt_utilization<-reactive({
            dt() %>% 
                group_by(station_id) %>%
                summarise(
                    `Total`= sum(capacity),
                    `Available(bikes)`= sum(num_bikes_available)-sum(num_ebikes_available),
                    `Available(ebikes)`= sum(num_ebikes_available)) %>% 
                mutate(
                    `Utilization`= 1-(`Available(bikes)`+`Available(ebikes)`)/`Total` )
                    }) 
        
    output$hist_utilization<- renderPlot({
        hist(
            as.numeric(dt_utilization()$`Utilization`),
            # xlim = c(0,1),
            xlab="Utilization Rate (rented/total)", 
            main="Utilization Rate",
            )
    },height = 250, width = 300)
    
    output$mapPlot<-renderLeaflet({
        pal=colorFactor(palette = c("blue","grey"),domain=dt()$station_status)
        
        leaflet(dt()) %>%
            addTiles() %>%
            setView(lng = -73.96 ,lat = 40.74, zoom = 12) %>% 
            addCircleMarkers(
                lat= ~lat,
                lng= ~lon,
                color = ~pal(station_status),
                radius = ~capacity/15,
                fillOpacity = 1)
        
    })
    
    output$summary <- renderTable({
        dt_summary()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)




