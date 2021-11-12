library(shiny)
library(shinythemes)
library(dplyr)
library(sf)
library(rnaturalearth)
library(ggplot2)

# Prevent weird behavior when cropping
sf_use_s2(FALSE)

# Load global layer of countries
world = rnaturalearth::ne_countries(returnclass = "sf")

# Load valid PROJ projection strings
projections = read.delim("data/proj_valid.csv", header=F, sep=";", col.names=c("code", "name")) %>% 
  arrange(name) %>% # Sort by name
  pull(code, name) # Cast to named vector

# Front End
ui <- fluidPage(
  # Theme
  theme = shinytheme("cerulean"),
  
  # Layout
  column(width = 2),
  column(width = 8,
         fluidRow(
           # Application title
           titlePanel("ProjectionPreview"),
           p("This is a simple Shiny Application to preview geographic projections. The Code is available on my ",
             tags$a(href = 'https://github.com/ChrKoenig/ProjectionPreview', 'Github', target = "_blank", .noWS = "outside"),
             ".")
         ),
         
         hr(),
         
         # Control Widgets
         fluidRow(
           column(width=4,
                  # Projection
                  selectInput("projection", label='Projection', choices=names(projections), selected="Lat/long (Geodetic)"),
                  # Region
                  selectInput("region", label="Region", selected="World", choices=list("World", 
                                                                                       Continent=sort(world$continent),
                                                                                       Subregion=sort(world$subregion),
                                                                                       Country=sort(world$name)))
           ),
           column(width=4,
                  # Lon/Lat Sliders
                  sliderInput("longitude", label="Longitude", min=-180, max=180, value=c(-180, 180), step = 0.1),
                  sliderInput("latitude", label="Latitude", min=-90, max=90, value=c(-90, 90), step = 0.1)
           ),
           column(width=3, offset=1,
                  # Display Toggles
                  tags$label("Display Settings", class="control-label"),
                  checkboxInput("show_grid", label="Show grid", value=T),
                  checkboxInput("show_borders", label="Show borders", value=T),
                  actionButton("reset", label="Reset values")
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
)

# Server
server <- function(input, output) {
  
  # Function to slightly enlarge bbox of focal region 
  scale_bbox = function(bbox, scaling=1.1){
    ext_lon = bbox["xmax"] - bbox["xmin"]
    ext_lat = bbox["ymax"] - bbox["ymin"]
    scaled_bbox = bbox + c(-ext_lon*(scaling-1),
                           -ext_lat*(scaling-1),
                           ext_lon*(scaling-1),
                           ext_lat*(scaling-1))
    
    # Make sure scaled bbox does not exceed global bbox
    world_bbox = world %>% st_bbox()
    values_replace = abs(scaled_bbox) > abs(world_bbox) 
    scaled_bbox[values_replace] = world_bbox[values_replace]
    return(scaled_bbox)
  }
  
  # Create region bbox
  observeEvent(input$region, handlerExpr = {
    if(input$region %in% world$name){ # Countries
      region_bbox = filter(world, name == input$region) %>% st_bbox() %>% scale_bbox()
    } else if(input$region %in% world$subregion){ # Subregions
      region_bbox = filter(world, subregion == input$region) %>% st_bbox() %>% scale_bbox()
    } else if(input$region %in% world$continent){ # Continents
      region_bbox = filter(world, continent == input$region) %>%  st_bbox() %>% scale_bbox()
    } else {
      region_bbox = world %>% st_bbox()
    }
    
    updateSliderInput(inputId="longitude", value=unname(region_bbox[c("xmin","xmax")]))
    updateSliderInput(inputId="latitude", value=unname(region_bbox[c("ymin","ymax")]))
  })
  
  map_crop = reactive({
    world %>% st_crop(c(xmin = input$longitude[1],
                                     ymin = input$latitude[1],
                                     xmax = input$longitude[2],
                                     ymax = input$latitude[2]))
  })

  map_proj = reactive({
    map_crop() %>%
      st_transform(paste0(" +proj=", projections[input$projection]))
  })
  
  theme_showgrid = reactive({
    if(input$show_grid){
      theme(panel.grid.major=element_line(colour="#888888", linetype="dashed"), panel.ontop=TRUE)
    } else {
      theme(panel.grid.major=element_blank())
    }
  })
  
  border_color = reactive({
    if(input$show_borders){
      "grey"
    } else {
      "black"
    }
  })

  fill_color = reactive({
    map_proj = map_proj()
    fill_color = rep("black", nrow(map_proj))
    fill_color[map_proj$name == input$region | map_proj$subregion == input$region | map_proj$continent == input$region] = "#438ae0"
    fill_color
  })
  
  observeEvent(input$reset, {
    updateSelectInput(inputId="projection", selected="Lat/long (Geodetic)")
    updateSelectInput(inputId="region", selected="World")
    updateSliderInput(inputId="longitude", value=c(-180,180))
    updateSliderInput(inputId="latitude", value=c(-90,90))
    updateCheckboxInput(inputId="show_grid", value=T)
    updateCheckboxInput(inputId="show_borders", value=T)
  })

  output$map = renderPlot(
    if(is.null(input$projection)){
      ggplot() + theme_void()
    } else {
      ggplot() +
        geom_sf(data=map_proj(), fill=fill_color(), col=border_color()) +
        theme_minimal() +
        theme_showgrid()
    }
  )
}

# Run the application 
shinyApp(ui=ui, server=server)