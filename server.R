library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(rpart)
library(e1071)
library(nnet)
library(rpart.plot)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')


# Leaflet bindings are a bit slow; for now we'll just sample to compensate




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
  
  observeEvent(input$run_models_regressions,{
    data1 <- rwanda
    if(input$camps_reg == "mugombwa"){
      data1 <- mug
      
    }
    else if(input$camps_reg == "kigeme"){
      data1 <- kim
     }
    regression_IV <- input$IV_regression
    regression_IV_str <- ""
    for(ele in regression_IV){
      regression_IV_str <- paste(regression_IV_str, ele , "+")
    }
    regression_IV_str <- substr(regression_IV_str,2,nchar(regression_IV_str)-2)
    data1$camp_name <- NULL
    if (regression_IV_str == ""){
      model <- eval(parse(text=paste0("lm(as.formula(",input$DV_regression," ~ . ), data = data1)")))
    }
    else{
      model <- eval(parse(text=paste0("lm(as.formula(",input$DV_regression," ~ ",regression_IV_str, "), data = data1)")))
    }
    
    output$accuracy_regression_model <- renderPrint({
    summary(model)
    })
  })
  
  observeEvent(input$run_models,
               {
                data <- rwanda
                if(input$camps == "mugombwa"){
                   data <- mug
                   
                }
                else if(input$camps == "kigeme"){
                  data <- kim
                  data$cash_food_local <- NULL
                  data$sell_food_assistance <- NULL

                }
                data$camp_name <- NULL
                data$market_security <- NULL
                data$finance_care <- NULL
                data$business_start <- NULL
                set.seed(100);
                if(input$algo == "Suppor Vector Machines"){
                  model <- eval(parse(text=paste0("tune.svm(",input$DV," ~ . , data = data)")))
                }
                else if(input$algo == "Decision Trees"){
                  model <- eval(parse(text=paste0("tune.rpart(",input$DV," ~ . , data = data)")))
                }
                else{
                  model <- eval(parse(text=paste0("tune.nnet(",input$DV," ~ . , data = data, ,linout=FALSE, size=20, trace = FALSE)")))

                }
                val <- round((1-model$best.performance),3)
                val <- val * 100
                algorithm <- input$algo
                dv <- input$DV
                val <- round(val,3)
                output$accuracy_model <- renderText({sprintf("%s Accuracy is %s percent for %s",algorithm,val,dv)})
                output$plot_model <- renderPlot({ 
                 
                  if(algorithm == "Suppor Vector Machines"){
                    plot(model$best.model, x~ y, data = data)
                  }
                  else if(algorithm == "Decision Trees"){
                    rpart.plot(model$best.model) 
                  }
                  else{
                   plot.nnet(model$best.model)
                }})})
  
  output$map <- renderLeaflet({
    mymap()
  })
  
  

  
  
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
      addCircles(~x, ~y, radius= radius, layerId = rwanda$object_id,
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
