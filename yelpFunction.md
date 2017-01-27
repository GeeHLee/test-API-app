# test-API-app
exploitation l'api yelp et web scrape

require(httr)
require(httpuv)
require(jsonlite)
library(purrr)


get_yelp_data <- function(term, location, address=NULL, limit=100){
  
  url <- modify_url(yelp, path = c("v3", "businesses", "search"),
                    query = list(term = term, location = location, limit = limit))
  
  datayelp <- GET(url, add_headers('Authorization' = paste("bearer", token)))
  http_status(datayelp)
  datayelpContent <- content(datayelp)
  
  YelpTab <- as.data.frame(fromJSON(toJSON(datayelpContent)))
  if (is.null(address)==FALSE) {
    to <- paste0(YelpTab$businesses.location$address1,", ", YelpTab$businesses.location$city)
    from <- paste0(address)
    YelpTab$distance <- mapdist(from, to)$km
    YelpTab$time <- mapdist(from, to)$minutes
    Near <- YelpTab[order(YelpTab$distance), ][1:5,]
    Businiess_data <- cbind(name = unlist(Near$businesses.name), 
                            ID = unlist(Near$businesses.id),
                            Near$businesses.coordinates, 
                            add = paste0(Near$businesses.location$address1,", ", Near$businesses.location$city),
                            distance = Near$distance,
                            rate = unlist(Near$businesses.rating), 
                            review_count = unlist(Near$businesses.review_count))
    Businiess_data$latitude <- as.numeric(Businiess_data$latitude)
    Businiess_data$longitude <- as.numeric(Businiess_data$longitude)
    return(Businiess_data)
  } else {
    return(YelpTab)  
  }
}

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
    NEAR <- Data_Near[order(Data_Near$dist),][1:5,]
    NEAR$status <- sapply(NEAR$ID, open_or_close)
    return(NEAR)
  }else {
    print("error connection")
  }
}

open_or_close <- function(ID){
  library(httr)
  library(lubridate)
  
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
