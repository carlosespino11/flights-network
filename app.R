library(shiny)
library(leaflet)
library(RColorBrewer)
library(lubridate)
library(dplyr)
library(maptools)
library(ggplot2)
library(ggthemes)
library(readr)
library(scales)
library(geosphere)
library(igraph)
library(forcats)
# setwd("~/Documents/Columbia/Exploratory Data Analysis and Visualization/HW2/EDAV_Proect_NOAA/")

flights = readRDS("filtered_flights3.rds")
carriers = read_csv("carriers.csv") %>% na.omit()
airports = read_csv("airports.csv") 

  
carrier_options = unique(flights$Carrier)

carrier_map = list()

for(carrier in carrier_options){
  name = carriers %>% filter(Code==carrier) %>% dplyr::select(Description)%>%unlist() %>% unname()
  carrier_map[[name]] = carrier
}


ui <- bootstrapPage(
  tags$head(
    includeCSS("styles.css")
  ),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10, id = "controls", class = "panel panel-default", 
                fixed = TRUE, draggable = TRUE,
                sliderInput("month", "Pick a month range", 
                            min=1, max=12, value=c(1,12)),
                sliderInput("day", "Pick a day of week range", 
                            min=1, max=7, value=c(1,7)),
                selectInput("airline", "Pick an airline", 
                            choices = carrier_map, selected = 1),
                h5("Betweeness centrality"),
                plotOutput("eigenplot", height = 200)
                # plotOutput("magnitudeMonth", height = 200),
                # plotOutput("cause", height = 200)
  ),
  absolutePanel(top = 10, left = 25,headerPanel("Flight Network by Carrier and Date"))
)

server <- function(input, output, session) {
  # Reactive expression to subset data
  filteredData <- reactive({
  #   
    selected_months = as.numeric(input$month)
    selected_days = as.numeric(input$day)
    carrier = input$airline
      
    months = selected_months[1]:selected_months[2]
    days = selected_days[1]:selected_days[2]

  #   
  #   
    filtered = flights %>% filter(Month %in% months, DayOfWeek %in% days, Carrier == carrier) 
    
    flights_by_route = filtered %>% group_by(Origin, Dest) %>% 
      summarize(n = n()) %>% 
      mutate(w = rescale(n,to=c(0,0.7))) %>%
      left_join(airports, by = c("Origin" = "iata")) %>%
      left_join(airports, by = c("Dest" = "iata")) %>% na.omit() %>%
      add_rownames("id")
    

    flights_by_route
  #   
  })
  
  output$map <- renderLeaflet({
    
    xlim <- c(-130.738281, -70.601563)
    ylim <- c(25.039321, 50.856229)
    
    # Aaspects of the map that  won't need to change dynamically
    leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter")%>%
      fitBounds(xlim[1],ylim[1],xlim[2],ylim[2]) 
    
  })
  
  # Incremental changes to the map
  observe({
    flights_by_route = filteredData()
    pal_fn = colorNumeric(c("#333333", "#99FFCC"), domain = flights_by_route$n, na.color = "white")

    
    lines_list = apply(flights_by_route, 1,function(row){
      Lines(
        Line(
          gcIntermediate(as.numeric(c(row["long.x"], row["lat.x"])),
                         as.numeric(c(row["long.y"], row["lat.y"])))
        ),
        ID= row['id']
      )
    })
    
    flights_lines = lines_list %>% SpatialLines() %>% SpatialLinesDataFrame(flights_by_route)
    leafletProxy("map", data= flights_lines) %>%
      clearShapes() %>%
      addPolylines(color = pal_fn(flights_by_route$n),weight = flights_by_route$w)
    
    output$eigenplot = renderPlot({
      flight_routes_graph = flights_by_route %>%
        dplyr::select(Origin,Dest,n,w, airport.x, airport.y) %>%
        graph_from_data_frame()
      
      # close = closeness(flight_routes_graph, weights = E(flight_routes_graph)$n, normalized = T)
      close = betweenness(flight_routes_graph, weights = E(flight_routes_graph)$n, normalized = T)
      # bet = eigen_centrality(flight_routes_graph)
      
      centrality_measures = data.frame(airport = names(close), 
                                       betweenness = unname(close)
                                       # betweenness= unname(bet)
      ) %>%
        left_join(airports, by = c("airport" = "iata"))
      
      centrality_measures_long = centrality_measures %>% 
        top_n(10, betweenness) 

      
      ggplot(centrality_measures_long ) + 
        aes(y = fct_reorder(airport.y, betweenness, .desc = FALSE),x =betweenness)+
        # aes(y = airport.y,x = value)+
        geom_point(color = "white")+ 
        geom_segment(aes(yend=airport.y), xend=0, color='grey50', color="white") +
        theme_fivethirtyeight() +
        theme(
              # axis.text = element_blank(),
              # axis.title = element_blank(), 
              # axis.ticks.y = element_blank(),
              panel.background = element_rect(fill = "black"),
              plot.background = element_rect(fill = "black"),
              axis.title.y = element_blank(),
              axis.title = element_text(colour = "white"),
              axis.text = element_text(colour = "white"),
              panel.grid = element_blank(),
              strip.background = element_rect(fill = "black"),
              strip.text = element_text(colour = "white")
        )
      
    })
  })
}

shinyApp(ui, server)
