library(shiny)
library(shinythemes)
library(shinyWidgets)
library(dplyr)
library(sf)
library(lwgeom)
library(ggplot2)

# Prevent weird behavior when cropping
sf_use_s2(FALSE)

# Load global layer of countries
world = st_read("data/World_Countries_Boundaries/World_Countries__Generalized_.shp") %>% 
  st_cast("POLYGON")

# Load valid PROJ projections strings
projections = read.delim("data/projections.csv", header = F, sep = ";", col.names = c("code", "name")) %>% 
  pull(code, name) # Cast to named vector

# Front End
ui <- fluidPage(
  # Theme
  theme = shinytheme("simplex"),
  
  # Application title
  titlePanel("ProjectionPreview"),
  
  hr(),
  
  fluidRow(style = "background-color:#4d3a7d margin-left:50px",
           column(width=4,
                  # PROJECTION
                  selectizeInput("projection", label='Projection', choices = names(projections), selected = "Lat/long (Geodetic)")
           ),
           column(width=4,
                  sliderInput("longitude", label="Longitude", min=-180, max=180, value=c(-180, 180)),
                  sliderInput("latitude", label="Latitude", min=-90, max=90, value=c(-90, 90))
           ),
           column(width=3, offset=1,
                  # DISPLAY
                  tags$label("Display Settings", class="control-label"),
                  checkboxInput("show_grid", label = "Show grid", value=T),
                  checkboxInput("show_borders", label = "Show borders", value=T),
                  actionButton("reset", label = "Reset values")
           )
  ),
  
  hr(),
  # Show projected map
  fluidRow(
    column(width=12,
           plotOutput("map")
    )
  )
)

# Server
server <- function(input, output) {
  map_crop = reactive({
    st_crop(world, y = c(xmin=input$longitude[1], ymin=input$latitude[1], xmax=input$longitude[2], ymax=input$latitude[2]))  
  })
  
  map_dateline_split = reactive({
    lon_split= input$lon_0 - 180
    st_sfc(st_linestring(matrix(c(lon_split,lon_split,-90,90), nrow = 2))) %>% 
      st_set_crs(st_crs(world))
  })
  
  map_proj = reactive({
    map_crop() %>% 
      st_transform(paste0(" +proj=", projections[input$projection])) %>% 
      st_geometry()
  })
  
  theme_showgrid = reactive({
    if(input$show_grid){
      theme(panel.grid.major = element_line(colour="#888888", linetype="dashed"),
            panel.ontop = TRUE)
    } else {
      theme(panel.grid.major = element_blank())
    }
  })
  
  border_color = reactive({
    if(input$show_borders){
      "grey"
    } else {
      "black"
    }
  })
  
  observeEvent(input$reset, {
    updateSelectInput(inputId = "projection", selected = "Lat/long (Geodetic)")
    updateSliderInput(inputId = "longitude", value = c(-180,180))
    updateSliderInput(inputId = "latitude", value = c(-90,90))
    updateCheckboxInput(inputId = "show_grid", value = T)
    updateCheckboxInput(inputId = "show_borders", value = T)
  })
  
  output$map = renderPlot(
    ggplot() +
      geom_sf(data = map_proj(), fill = "black", col = border_color()) +
      theme_minimal() +
      theme_showgrid()
  ) 
}

# Run the application 
shinyApp(ui=ui, server=server)
