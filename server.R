library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)



function(input, output, session) {
 vals <- reactiveValues(count = 1)
  ## Interactive Map ###########################################

  # Create the map
  mymap <- reactive({
    leaflet( data = rwanda) %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lat =  -2.4867, lng = 29.5187, zoom = 16)
  })
  
  output$map <- renderLeaflet({
    mymap()
  })
  
  output$download <- downloadHandler(
    filename = 'plot.pdf',
    
    content = function(file) {
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
      saveWidget(newmap(), "temp.html", selfcontained = FALSE)
      webshot("temp.html", file = file, cliprect = "viewport")
    }
)
  
  
  newmap <- reactive({
    colorBy <- input$color
    sizeBy <- input$size
    colorData <- rwanda[[colorBy]]
    if(is.factor(colorData)){
      pal <- colorFactor("viridis", colorData)
    }
    else{
      pal <- colorBin("viridis", colorData, 7)
    }
    
    if(input$static_size == TRUE){
      radius <- input$slider1
    }
    else{
      radius <- rwanda[[sizeBy]]
    }
    mymap() %>% addCircles(~x, ~y, radius= radius, layerId = ~object_id,
                           stroke=FALSE, fillOpacity=.5, color=pal(colorData)) %>% addProviderTiles(providers$OpenStreetMap) 
  })
  
  observeEvent(input$change, {
   print("changing map-vew")
   proxy <- leafletProxy("map")

   if(vals$count %% 2 == 1){
     proxy %>% setView(lat = -2.67541, lng = 29.84879, zoom = 17)
   }
   else{
     proxy %>% setView(lat =  -2.4867, lng = 29.5187, zoom = 16)
   }
   vals$count <- vals$count + 1
  
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    
    colorBy <- input$color
    sizeBy <- input$size
    colorData <- rwanda[[colorBy]]
    if(is.factor(colorData)){
      pal <- colorFactor("viridis", colorData)
    }
    else{
      pal <- colorBin("viridis", colorData, 7)
    }
    
    if(input$static_size == TRUE){
      radius <- input$slider1
    }
    else{
      radius <- rwanda[[sizeBy]]
    }
    
    
    leafletProxy("map", data = rwanda) %>%
      clearShapes() %>%
      addCircles(~x, ~y, radius= radius, layerId = ~object_id,
        stroke=FALSE, fillOpacity=.5, color=pal(colorData)) %>%
        clearControls() %>%
        addLegend(position = "bottomright",
                        pal = pal, values = colorData, title = input$color)

  })
  
  showCirclePopup <- function(id, lat, lng) {
    selectedCircle <- rwanda[rwanda$object_id == id,]
    popupTagList <- tagList(
    tags$h4(input$color, eval(parse(text=paste0("selectedCircle$",input$color)))), 
    tags$h4(input$size, eval(parse(text=paste0("selectedCircle$",input$size))))
  )



content <- as.character(popupTagList)    
    leafletProxy("map") %>% addPopups(lng, lat, content)
  }
  
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    isolate({
      showCirclePopup(event$id, event$lat, event$lng)
    })
  })
  
  
  
  
}
