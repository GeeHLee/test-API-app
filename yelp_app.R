library(shiny)

ui <- fluidPage(
  titlePanel("Search servers"),
  
  sidebarPanel(
    textInput("address", label = h5("address departure"), value="107 rue sadi carnot, vanves"),
    textInput("term", label = h5("servers"), value="restaurants"),
    textInput("rayon", label=h5("radius"), value=3000),
    actionButton("search", label = "search")
  ),
  mainPanel(
    leafletOutput("view")
  )
)

server <- function(input, output){
  source("/home/username/API/YelpSearch/yelpFunctions.R")
  library(ggmap)
  library(leaflet)
  
  
  Data_input <- eventReactive(input$search, {
      Data_yelp <-  Find_server(term = input$term,
                    address_departure = input$address,
                    perimeters_m = input$rayon)
  })
  
  output$view <- renderLeaflet({
    Data <- Data_input()
    coord_depart <- eventReactive(input$search, {geocode(input$address)})
    
    coord <- coord_depart()
    lon_city <- coord$lon
    lat_city <- coord$lat
    
    fond <- get_map(location = c(lon=lon_city,lat=lat_city),maptype = "terrain",zoom=10)
    
    pal <- colorFactor(c("navy", "green","black"), domain = c("unknown", "open", "close"))
    
    pop <- paste0("name : ",Data$names,"<br>", 
                  "status : ", Data$status,"<br>", 
                  "address : ", Data$add, "<br>",
                  "distance : ", Data$dist, " km", "<br>",
                  "You need ", round(Data$time,1), " minutes to get there", "<br>")
    
    m <- leaflet(fond)%>%
      addTiles() %>%
      setView(lng = lon_city, lat = lat_city, zoom=13) %>%
      addCircleMarkers(data=Data, lng=~lon, lat=~lat, 
                       color=~pal(status),popup =pop) %>%
      addMarkers(lng=coord$lon, lat=coord$lat, popup="You Position") %>%
      addLegend("bottomleft", pal=pal, value=c("unknown", "open", "close"))
    return(m)
  })
}

shinyApp(ui=ui, server = server)
