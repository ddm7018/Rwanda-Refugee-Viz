library(leaflet)

# Choices for drop-downs
vars <- c(
  "num_employee" = "num_employee",
  "avg_customers" = "avg_customers"
)


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
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Rwanda explorer"),

        selectInput("color", "Color", vars),
        selectInput("size", "Size", vars),
        actionButton("change", "Change map")
        
          
      )

      
    )
  ))


