library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
zipdata <- allzips[sample.int(nrow(allzips), 10000),]

rwanda <- read.csv("data/survey.csv")


# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$centile),]

function(input, output, session) {
 vals <- reactiveValues(count = 1)
  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lat =  -2.4871, lng = 29.5167, zoom = 15)
    
  })

  observeEvent(input$change, {
   print("changing map-vew")
   proxy <- leafletProxy("map")
   print(vals$count)
   if(vals$count %% 2 == 1){
     proxy %>% setView(lat = -2.6739, lng = 29.8450, zoom = 15)
   }
   else{
     proxy %>% setView(lat =  -2.4871, lng = 29.5167, zoom = 15)
   }
   vals$count <- vals$count + 1
  
  })
  
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size
    
  
    colorData <- rwanda[[colorBy]]
    pal <- colorBin("viridis", colorData, 7)
    radius <- rwanda[[sizeBy]]
    

    leafletProxy("map", data = rwanda) %>%
      clearShapes() %>%
      addCircles(~x, ~y, radius= radius,
        stroke=FALSE, fillOpacity=.5, color=pal(colorData))
  })
}
