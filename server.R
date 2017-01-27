library(shiny)
library(leaflet)
library(ggmap)
library(httr)
require(jsonlite)

get_data_Frame <- function(list){
  names <- unlist(list$businesses.name)
  ID <- unlist(list$businesses.id)
  lon <- unlist(list$businesses.coordinates$longitude)
  lat <- unlist(list$businesses.coordinates$latitude)
  add <- unlist(list$businesses.location$address1)
  city <- unlist(list$businesses.location$city)
  rate <- unlist(list$businesses.rating)
  count <- unlist(list$businesses.review_count)
  price <- list$businesses.price
  price[sapply(price, is.null)]<-NA
  price <- unlist(price)
  out <- data.frame(names, ID, lon, lat, add, city, rate, count, price)
  return(out)
}


Find_server <- function(term, address_departure, perimeters_m){
  require(httr)
  require(ggmap)
  
  consumerKey <- "FVTmbf7WFgcWDmu7nk-KGw"
  consumerSecret <- "oLPrIex3Ycubgsx39RYG5TyfMMpT4d7beasMmylmr8fgeBc4Hxk8w5JpthwdlDTF"
  Gettoken <- POST("https://api.yelp.com/oauth2/token",
                   body = list(grant_type = "client_credentials",
                               client_id = consumerKey,
                               client_secret = consumerSecret))
  token <- content(Gettoken)$access_token
  
  
  yelp <- "https://api.yelp.com"
  coords <- geocode(address_departure)
  lon <- as.numeric(coords$lon)
  lat <- as.numeric(coords$lat)
  
  url <- modify_url(yelp, path = c("v3", "businesses", "search"),
                    query = list(term = term, limit=20,
                                 latitude=lat, longitude=lon,
                                 radius=perimeters_m))
  Post_data <- GET(url, add_headers('Authorization' = paste("bearer", token)))
  if(http_status(Post_data)$category=="Success"){
    data_Contents <- content(Post_data)
    LocationData <-data.frame(fromJSON(toJSON(data_Contents)))
    Data_Near <- get_data_Frame(LocationData)
    from <- address_departure
    to <- paste0(Data_Near$add, ",", Data_Near$city)
    Distance <- mapdist(from, to, mode = c("walking"))
    Data_Near$dist<-Distance$km
    Data_Near$time <- Distance$minutes
    NEAR <- Data_Near[order(Data_Near$dist),][1:round(nrow(Data_Near)/2),]
    NEAR$status <- sapply(NEAR$ID, open_or_close)
    return(NEAR)
  }else {
    print("error connection")
  }
}

open_or_close <- function(ID){
  library(httr)
  library(lubridate)
  
  consumerKey <- "FVTmbf7WFgcWDmu7nk-KGw"
  consumerSecret <- "oLPrIex3Ycubgsx39RYG5TyfMMpT4d7beasMmylmr8fgeBc4Hxk8w5JpthwdlDTF"
  Gettoken <- POST("https://api.yelp.com/oauth2/token",
                   body = list(grant_type = "client_credentials",
                               client_id = consumerKey,
                               client_secret = consumerSecret))
  token <- content(Gettoken)$access_token
  
  yelp <- "https://api.yelp.com"
  url2 <- modify_url(yelp, path = c("v3","businesses",paste0(ID)))
  dataBuss <- GET(url2, add_headers('Authorization' = paste("bearer", token)))
  dataBussContent <- content(dataBuss)
  now <- Sys.time()
  limit_day <- as.numeric(length(dataBussContent$hours[[1]]$open))
  nday <- as.numeric(factor(weekdays(now), levels = c("Monday", "Tuesday", "Wednesday", 
                                                      "Thursday", "Friday", "Saturday","Sunday"),
                            ordered = TRUE))
  
  if(is.null(dataBussContent$hours)==TRUE){results <- "unknow"}
  if(nday > limit_day){results <- "close"}else{
    hour_now <- hour(now)
    start <-as.numeric(dataBussContent$hours[[1]]$open[[nday]]$start)/100
    end <- ifelse(as.numeric(dataBussContent$hours[[1]]$open[[nday]]$end)/100==0, 24,
                  as.numeric(dataBussContent$hours[[1]]$open[[nday]]$end)/100)
    if(hour_now >= start & hour_now <= end){results <- "open"}else{
      results <- "close"
    }
  }
  return(results)
}

get_coordinate_server <- function(server = "tabac", 
                                  position = "107 rue sadi carnot, vanves", 
                                  perimeters= 3000){
  library(ggmap)
  library(leaflet)
  
  Data_yelp <- Find_server(term = server, address_departure = position,
                           perimeters_m = perimeters)
  Data_yelp$status <- sapply(Data_yelp$ID, open_or_close)
  
  coord_depart <- geocode(position) 
  lon_city <- coord_depart$lon
  lat_city <- coord_depart$lat
  fond <- get_map(location = c(lon=lon_city,lat=lat_city),maptype = "terrain",zoom=10)
  pal <- colorFactor(c("navy", "green","black"), domain = c("unknown", "open", "close"))
  
  pop <- paste0("name : ",Data_yelp$names,"<br>", 
                "status : ", Data_yelp$status,"<br>", 
                "address : ", Data_yelp$add, "<br>",
                "distance : ", Data_yelp$dist, " km", "<br>",
                "You need ", round(Data_yelp$time,1), " minutes to get there", "<br>")
  m <- leaflet(fond)%>%
    addTiles() %>%
    setView(lng = lon_city, lat = lat_city, zoom=13) %>%
    addCircleMarkers(data=Data_yelp, lng=~lon, lat=~lat, 
                     color=~pal(status),popup =pop) %>%
    addMarkers(lng=coord_depart$lon, lat=coord_depart$lat, popup="You Position") %>%
    addLegend("bottomleft", pal=pal, value=c("unknown", "open", "close"))
  
  return(m)
}


consumerKey <- "FVTmbf7WFgcWDmu7nk-KGw"
consumerSecret <- "oLPrIex3Ycubgsx39RYG5TyfMMpT4d7beasMmylmr8fgeBc4Hxk8w5JpthwdlDTF"
Gettoken <- POST("https://api.yelp.com/oauth2/token",
                 body = list(grant_type = "client_credentials",
                             client_id = consumerKey,
                             client_secret = consumerSecret))
token <- content(Gettoken)$access_token

function(input, output){
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
    
    if (input$name %in% Data$names == TRUE) {
      library(dplyr)
      arrival <- Data %>%
        filter(names==input$name) %>%
        mutate(address=paste0(add, ", ", city))%>%
        select(address)
      path <- route(from=input$address, to=arrival$address, mode = "walking")
      
      p <- m %>% 
        addPolylines(data = path, lng=~startLon, lat=~startLat, color="red")
      return(p)
    } else {
      return(m)
    }
  })
}
