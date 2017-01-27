library(shiny)
library(leaflet)
library(ggmap)
library(httr)

fluidPage(
  titlePanel("Search servers"),
  
  sidebarPanel(
    textInput("address", label = h5("address departure"), value="107 rue sadi carnot, vanves"),
    textInput("term", label = h5("servers"), value="restaurants"),
    textInput("rayon", label=h5("radius"), value=3000),
    actionButton("search", label = "search"),
    textInput("name", label = h5("get path to : "),value = "NULL")
  ),
  mainPanel(
    leafletOutput("view")
  )
)
