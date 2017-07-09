library(leaflet)

# Choices for drop-downs
#color_vars <- colnames(rwanda)

#removeFrom = c("object_id","global_id","row_id","parent_row_id","x", "y","ObjectID","GlobalID")
#for(ele in removeFrom){ 
#  color_vars <- color_vars[color_vars != ele]
#  }



algos <- c("Suppor Vector Machines", "Decision Trees") 

navbarPage("Rwanda", id="nav",

  tabPanel("Interactive map",
    div(class="outer",
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),
      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "20", right = "auto", bottom = "auto",
        width = 330, height = "auto",
        h2("Rwanda explorer"),
      
        selectInput("color", "Color", vars),
        conditionalPanel(
          condition = "input.static_size == false",
          selectInput("size", "Size", vars, selected = "num_employee")),
        actionButton("change", "Change map"),
        downloadLink("download", "Download"),
        checkboxInput("static_size", "Static Radius", FALSE),
        conditionalPanel(
        condition = "input.static_size == true",
        sliderInput("slider1", label = h3("Slider"), min = 1, 
                    max = 20, value = 5))
      ))),
    tabPanel("Run Models",
             sidebarPanel(id = "controls", class = "panel panel-default",
                          selectInput("algo", "Algorithm", algos),
                          selectInput("DV", "Dependent Variables", non_numeric_vals),
                          checkboxInput("all_variables", "All Variables Included", FALSE),
                          conditionalPanel(
                            condition = "input.all_variables == false",
                          checkboxGroupInput("IV", "Independent Variables", vars)
                          ),
                          actionButton("run_models", "Run Models")
                          ),textOutput("text1"),
                            plotOutput("text2")                  
             )
                )


