library(leaflet)

algos <- c("Suppor Vector Machines", "Decision Trees", "Nueral Network") 

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
        checkboxInput("static_size", "Static Radius", FALSE),
        conditionalPanel(
        condition = "input.static_size == true",
        sliderInput("slider1", label = h3("Slider"), min = 1, 
                    max = 20, value = 5))
      ))),
    tabPanel("Run Classifcation Models",
             sidebarPanel(id = "controls", class = "panel panel-default",
                          selectInput("algo", "Algorithm", algos),
                          selectInput("DV", "Dependent Variables", non_numeric_vals),
                          radioButtons("camps", "Select target Camps:",c("mugombwa", "kigeme", "Both")),
                          actionButton("run_models", "Run Models")
                          ),textOutput("accuracy_model"),
                            plotOutput("plot_model")                  
             ),
  tabPanel("Run Regression Models",
           sidebarPanel(id = "controls", class = "panel panel-default",
                        selectInput("DV_regression", "Dependent Variables", numeric_vals),
                        checkboxInput("all_variables_regressions", "All Variables Included", TRUE),
                        radioButtons("camps_reg", "Select target Camps:",c("mugombwa", "kigeme", "Both")),
                        conditionalPanel(
                          condition = "input.all_variables_regressions == false",
                          checkboxGroupInput("IV_regression", "Independent Variables", vars)
                        ),
                        actionButton("run_models_regressions", "Run Models")
                        ),
           verbatimTextOutput("accuracy_regression_model")))


