library(shiny)
library(leaflet)
library(sf)
library(dplyr)
#library(rsconnect)

# Define the base path for your dataset
base_path <- file.path("..", "Data", "Transport and Schools", "Transport")

# Define file paths based on your folder structure
bus_stops_shp <- file.path(base_path, "Bus", "bus-stops.json")
train_data_csv <- file.path(base_path, "Train", "TrainStationNew_1.csv")
train_icon_path <- file.path(base_path, "Train", "Train photo.jpg")
tram_stops_shp <- file.path(base_path, "Tram", "tram_stops.json")
tram_tracks_geojson <- file.path(base_path, "Tram", "tram-tracks.geojson")
tram_outline_geojson <- file.path(base_path, "Tram", "tram_track_outlines.geojson")
postcodes_geojson <- file.path(base_path, "map_postcodes.geojson")
bus_icon_path <- file.path(base_path, "Bus", "kisspng-bus-emoji-unicode-noto-fonts-sms-mount-fuji-5ac74656f23745.6236168615230091109921.png")

# Load your data
bus_stops <- st_read(bus_stops_shp)
tram_stops <- st_read(tram_stops_shp)  # Load tram stops
train_data <- read.csv(train_data_csv)  # Load train data
tram_tracks <- st_read(tram_tracks_geojson)  # Load tram tracks
tram_outlines <- st_read(tram_outline_geojson)  # Load tram outlines (this was missing)
postcodes <- st_read(postcodes_geojson)  # Load the postcode GeoJSON

# Load the postcode GeoJSON
postcodes <- st_read(postcodes_geojson)

# Filter the train data to include only necessary columns
train_stations <- train_data %>%
  select(Stop_long, Stop_lat, Stop_name)

# Extract POINT geometries from GEOMETRYCOLLECTION for Bus stops
bus_stops_points <- st_cast(bus_stops, "POINT") %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(lng = X, lat = Y)

# Extract POINT geometries from GEOMETRYCOLLECTION for Tram stops
tram_stops_points <- st_cast(tram_stops, "POINT") %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(lng = X, lat = Y)

# Custom icons for bus and train markers
bus_icon <- makeIcon(iconUrl = bus_icon_path, iconWidth = 18, iconHeight = 18)
train_icon <- makeIcon(iconUrl = train_icon_path, iconWidth = 24, iconHeight = 24)

# Define UI
ui <- fluidPage(
  # Add inline CSS to use Heiti SC font and set a fallback
  tags$style(HTML("
    body {
      font-family: 'Heiti SC', sans-serif;
    }
  ")),
  
  # Use a div with flexbox to align the title and selectInput on the same line
  div(
    style = "display: flex; align-items: center; justify-content: space-between;", 
    h3("Public Transport Map", 
       style = "margin: 0; font-size: 20px; font-family: 'Heiti SC', sans-serif; color: #000000;"),  # Apply Heiti SC and color
    div(
      style = "float: right; margin-top: 2px;",  # Add margin-top to bring it down
      div(
        style = "font-size: 12px;",  # Reduce font size for the label
        selectInput(
          inputId = "map_type", 
          label = "Select Transport Mode", 
          choices = c("Bus", "Train", "Tram")
        )
      )
    )
  ),
  
  # Output the map
  leafletOutput("transport_map", height = 550)
)

# Define server
server <- function(input, output, session) {
  
  # Render the leaflet map with postcode boundaries initially
  output$transport_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      # Add postcode boundaries as transparent polygons with borders
      addPolygons(
        data = postcodes,
        fillColor = "transparent",  # Make the fill transparent
        weight = 2,  # Border thickness
        color = "#2C5E80",  # Border color
        fillOpacity = 0,  # No fill, just borders
        label = ~Suburb,  # Label each boundary with its suburb name
        labelOptions = labelOptions(
          style = list(
            "font-size" = "15px",          # Increase label font size
            "font-weight" = "bold",        # Make the label bold
            "font-family" = "Heiti SC",    # Use Heiti SC font
            "color" = "#2C5E80"            # Set font color to #2C5E80
          ),
          direction = "auto"  # Automatically place the label around the polygon
        ),
        highlightOptions = highlightOptions(  # Highlight on hover
          color = "red", 
          weight = 3,
          bringToFront = TRUE
        )
      )
  })
  
  # Dynamically update only the transport-specific features (markers, polylines)
  observe({
    leafletProxy("transport_map") %>%
      clearMarkers() %>%  # Clear only the markers
      clearGroup("transport")  # Clear transport-specific shapes
    
    if (input$map_type == "Bus") {
      leafletProxy("transport_map") %>%
        addMarkers(
          data = bus_stops_points,
          lng = ~lng,  # Use extracted longitude
          lat = ~lat,  # Use extracted latitude
          popup = "Bus Stop",  # Simple popup
          icon = bus_icon  # Set bus icon
        )
      
    } else if (input$map_type == "Train") {
      leafletProxy("transport_map") %>%
        addMarkers(
          lng = train_stations$Stop_long,  # Use filtered longitude
          lat = train_stations$Stop_lat,   # Use filtered latitude
          popup = train_stations$Stop_name,  # Use train station name as popup
          icon = train_icon  # Set train icon
        )
      
    } else if (input$map_type == "Tram") {
      leafletProxy("transport_map") %>%
        # Add tram tracks as polylines in a specific group
        addPolylines(
          data = tram_tracks,
          color = "tomato",
          weight = 4,
          opacity = 2,
          group = "transport"
        ) %>%
        
        # Add tram stops as circle markers in a specific group
        addCircleMarkers(
          data = tram_stops_points,
          lng = ~lng,  # Use extracted longitude for tram
          lat = ~lat,  # Use extracted latitude for tram
          radius = 5,  # Adjust the size of the circle
          color = "blue",  # Stroke color for the circle (blue border)
          fillColor = "yellow",  # Fill color (you can change to any color like 'red', 'green', etc.)
          fillOpacity = 0.8,  # Set opacity for the circle
          weight = 2,  # Border width (stroke thickness)
          popup = "Tram Stop",  # Simple popup for tram stops
          group = "transport"  # Assign to a group
        )
    }
  })
}

# RUN SHINY 
shinyApp(ui, server, options = list(port=6247))