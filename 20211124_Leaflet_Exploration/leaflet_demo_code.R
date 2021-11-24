# --------------------------- #
#
# Developing Interactive Maps using R and Leaflet
#
# Description: R code for the demo
# Author: Tom Jenkins
# Organisation: Natural England
# Email: Tom.Jenkins@naturalengland.org.uk
# GitHub: https://github.com/Tom-Jenkins
# Last modified: 2021.11.23
#
# --------------------------- #

# Load main packages
library(leaflet)
library(leafpop)
library(leaflet.extras)
library(leafem)
library(htmltools)
library(sf)
library(tidyverse)
library(rnaturalearth)
library(lattice)
library(htmlwidgets)


# ----------------- #
#
# 1. Simple leaflet map ####
#
# ----------------- #

# Coordinates of the centre point of the UK: Whitendale Hanging Stones
whitendale = c(-2.547855, 54.00366)


l1 = leaflet() %>%
  # Centre map on Whitendale Hanging Stones
  setView(lng = whitendale[1], lat = whitendale[2], zoom = 6) %>% 
  # Add OSM basemap
  addProviderTiles(providers$OpenStreetMap, group = "Open Street Map")
l1


# ----------------- #
#
# 2. Add and switch basemap layers ####
#
# ----------------- #

# Basemap options
?addProviderTiles
# http://leaflet-extras.github.io/leaflet-providers/preview/


leaflet() %>%
  # Centre map on Whitendale Hanging Stones
  setView(lng = whitendale[1], lat = whitendale[2], zoom = 6) %>% 
  # Add OSM basemap
  addProviderTiles(providers$OpenStreetMap) %>% 
  # Add additional basemap layers
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addProviderTiles(providers$Esri.OceanBasemap)


l2 = l1 %>%
  # Add additional basemap layers
  addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery") %>% 
  addProviderTiles(providers$Esri.OceanBasemap, group = "ESRI Ocean Basemap") %>% 
  # Add a User-Interface (UI) control to switch layers
  addLayersControl(
    baseGroups = c("Open Street Map","ESRI World Imagery","ESRI Ocean Basemap"),
    options = layersControlOptions(collapsed = FALSE)
  )
l2


# ----------------- #
#
# 3. Add point data to map ####
#
# ----------------- #

# Download a polygon outline of the UK
uk_outline = ne_countries(country = "United Kingdom", scale = "medium", returnclass = "sf") %>% 
  # Keep only geometry
  st_geometry() %>% 
  # Transform coordinate reference system (CRS) to WGS84
  st_transform(crs = 4326)
plot(uk_outline)

# Sample 100 random points within the UK polygon
set.seed(1234)
uk_pts = st_sample(uk_outline, size = 100)
plot(uk_pts, pch = 21, col = "black", bg = "yellow", add = TRUE)


l2 %>%
  # Add point data
  addMarkers(data = uk_pts)


l2 %>%
  # Add point data with a label
  addMarkers(
    data = uk_pts,
     # Label appears on mouse hover
     label = paste("Label", seq(1:100)),
     # Options to customise the label text and size
     labelOptions = labelOptions(
       textsize = "14px", # text size
       style = list(
        "font-weight" = "bold", # bold text
        padding = "5px" # padding around text
        )
       )
    )



# Prepare icons to add to map
tree_icon = "https://cdn-icons-png.flaticon.com/512/490/490091.png"

l3 = l2 %>%
  # Add point data with a label
  addMarkers(
    data = uk_pts,
    # Label appears on mouse hover
    label = paste("Label", seq(1:100)),
    # Options to customise the label text and size
    labelOptions = labelOptions(
      textsize = "14px", # text size
      style = list(
        "font-weight" = "bold", # bold text
        padding = "5px" # padding around text
      )
    ),
    # Change markers to custom icons
    icon = list(iconUrl = tree_icon,
                iconSize = c(30, 30)
                )
    )
l3



# ----------------- #
#
# 4. Add polygon data to map ####
#
# ----------------- #

# Create a polygon using a bounding box of four coordinates
poly_df = data.frame(lon = c(-1.5,-0.9), lat = c(52.0,52.2))
poly1 = st_as_sf(poly_df, coords = c("lon","lat"), crs = 4326) %>%
  st_bbox %>% 
  st_as_sfc
plot(poly1)


l3 %>%
  # Add polygon to map
  addPolygons(
    data = poly1,
    group = "Polygon 1",
    # stroke parameters
    weight = 3, color = "blue",
    # fill parameters
    fillColor = "blue", fillOpacity = 0.1
  )


l4 = l3 %>%
  # Add polygon to map
  addPolygons(
    data = poly1,
    group = "Polygon 1",
    # stroke parameters
    weight = 3, color = "blue",
    # fill parameters
    fillColor = "blue", fillOpacity = 0.1,
    # Popup label
    popup = "Polygon 1",
    # Options to customise polygon highlighting
    highlightOptions = highlightOptions(
      # Highlight stroke parameters
      weight = 3, color = "white",
      # Highlight fill parameters
      fillColor = "blue", fillOpacity = 0.1
    )
  )
l4


# ----------------- #
#
# 5. Add popup graphs and images ####
#
# ----------------- #

# Sample 2 random points within the UK polygon
set.seed(1234)
uk_pts2 = st_sample(uk_outline, size = 2)
plot(uk_outline)
plot(uk_pts, pch = 21, col = "black", bg = "yellow", add = TRUE)
plot(uk_pts2, pch = 21, col = "black", bg = "red", add = TRUE)

# Example graph to show
graph1 = levelplot(t(volcano), col.regions = terrain.colors(100), xlab = "", ylab = "")

# Example image to show (NE logo)
img1 = "https://naturalengland.blog.gov.uk/wp-content/uploads/sites/183/2019/08/NatEng_logo_New-Green-RGB.jpg"


l5 = l4 %>%
  # Add circle points with a popup graph on mouse click
  addCircles(
    data = uk_pts2[1],
    # group argument essential for linking to popup graph
    group = "Circle point 1",
    # circle size
    radius = 10000,
    # circle stroke parameters
    weight = 1, color = "black",
    # circle fill parameterse
    fillColor = "red", fillOpacity = 0.8,
    # highlight options
    highlightOptions = highlightOptions(
      weight = 1, color = "white", fillOpacity = 0.8
      )
    ) %>% 
  # Add popup graph
  addPopupGraphs(
    graph = list(graph1), width = 400, height = 300,
    group = "Circle point 1"
  )
l5  


l6 = l5 %>%
  # Add circle points with a popup image on mouse click
  addCircles(
    data = uk_pts2[2],
    # group argument essential for linking to popup graph
    group = "Circle point 2",
    # circle size
    radius = 10000,
    # circle stroke parameters
    weight = 1, color = "black",
    # circle fill parameterse
    fillColor = "red", fillOpacity = 0.8,
    # highlight options
    highlightOptions = highlightOptions(
      weight = 1, color = "white", fillOpacity = 0.8
    )
  ) %>% 
  # Add popup graph
  addPopupImages(
    image = img1, width = 150, height = 150,
    group = "Circle point 2"
  )
l6



# ----------------- #
#
# 6. Extra customisation options ####
#
# ----------------- #


l6 %>%
  # Reset map to default setting
  addResetMapButton() %>% 
  # Add an inset minimap
  addMiniMap(
    position = "topright",
    tiles = providers$Esri.WorldStreetMap,
    toggleDisplay = TRUE,
    minimized = FALSE
    ) %>%
  # Add measurement tool
  addMeasure(
    position = "topleft",
    primaryLengthUnit = "meters",
    secondaryLengthUnit = "kilometers",
    primaryAreaUnit = "sqmeters"
    ) %>%
  # Add scalebar
  addScaleBar(
    position = "bottomright",
    options = scaleBarOptions(imperial = FALSE)
    ) %>% 
  # Add a User-Interface (UI) control to switch layers
  addLayersControl(
    position = "topright",
    baseGroups = c("Open Street Map","ESRI World Imagery","ESRI Ocean Basemap"),
    # Add an option in layer control to toggle polygon layer
    overlayGroups = c("Polygon 1", "Circle point 1", "Circle point 2"),
    options = layersControlOptions(collapsed = FALSE)
  )
  
  

# ----------------- #
#
# 7. Final map code block ####
#
# ----------------- #

final_map = leaflet() %>%
  # Centre map on Whitendale Hanging Stones
  setView(lng = whitendale[1], lat = whitendale[2], zoom = 6) %>% 
  # Add OSM basemap
  addProviderTiles(providers$OpenStreetMap, group = "Open Street Map") %>% 
  # Add additional basemap layers
  addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery") %>% 
  addProviderTiles(providers$Esri.OceanBasemap, group = "ESRI Ocean Basemap") %>% 
  # Add point data with a label
  addMarkers(
    data = uk_pts,
    # Label appears on mouse hover
    label = paste("Label", seq(1:100)),
    # Options to customise the label text and size
    labelOptions = labelOptions(
      textsize = "14px", # text size
      style = list(
        "font-weight" = "bold", # bold text
        padding = "5px" # padding around text
      )
    ),
    # Change markers to custom icons
    icon = list(iconUrl = tree_icon,
                iconSize = c(30, 30)
    )
  ) %>% 
  # Add polygon to map
  addPolygons(
    data = poly1,
    group = "Polygon 1",
    # stroke parameters
    weight = 3, color = "blue",
    # fill parameters
    fillColor = "blue", fillOpacity = 0.1,
    # Popup label
    popup = "Polygon 1",
    # Options to customise polygon highlighting
    highlightOptions = highlightOptions(
      # Highlight stroke parameters
      weight = 3, color = "white",
      # Highlight fill parameters
      fillColor = "blue", fillOpacity = 0.1
    )
  ) %>% 
  # Add circle points with a popup graph on mouse click
  addCircles(
    data = uk_pts2[1],
    # group argument essential for linking to popup graph
    group = "Circle point 1",
    # circle size
    radius = 10000,
    # circle stroke parameters
    weight = 1, color = "black",
    # circle fill parameterse
    fillColor = "red", fillOpacity = 0.8,
    # highlight options
    highlightOptions = highlightOptions(
      weight = 1, color = "white", fillOpacity = 0.8
    )
  ) %>% 
  # Add popup graph
  addPopupGraphs(
    graph = list(graph1), width = 400, height = 300,
    group = "Circle point 1"
  ) %>% 
  # Add circle points with a popup image on mouse click
  addCircles(
    data = uk_pts2[2],
    # group argument essential for linking to popup graph
    group = "Circle point 2",
    # circle size
    radius = 10000,
    # circle stroke parameters
    weight = 1, color = "black",
    # circle fill parameters
    fillColor = "red", fillOpacity = 0.8,
    # highlight options
    highlightOptions = highlightOptions(
      weight = 1, color = "white", fillOpacity = 0.8
    )
  ) %>% 
  # Add popup graph
  addPopupImages(
    image = img1, width = 150, height = 150,
    group = "Circle point 2"
  ) %>% 
  # Reset map to default setting
  addResetMapButton() %>% 
  # Add an inset minimap
  addMiniMap(
    position = "topright",
    tiles = providers$Esri.WorldStreetMap,
    toggleDisplay = TRUE,
    minimized = FALSE
  ) %>%
  # Add measurement tool
  addMeasure(
    position = "topleft",
    primaryLengthUnit = "meters",
    secondaryLengthUnit = "kilometers",
    primaryAreaUnit = "sqmeters"
  ) %>%
  # Add scalebar
  addScaleBar(
    position = "bottomright",
    options = scaleBarOptions(imperial = FALSE)
  ) %>% 
  # Add a User-Interface (UI) control to switch layers
  addLayersControl(
    position = "topright",
    baseGroups = c("Open Street Map","ESRI World Imagery","ESRI Ocean Basemap"),
    # Add an option in layer control to toggle polygon layer
    overlayGroups = c("Polygon 1", "Circle point 1", "Circle point 2"),
    options = layersControlOptions(collapsed = FALSE)
  )
final_map


# Export map as html file
# setwd("D:/Natural Capital and Ecosystem Assessment/Demos/Leaflet_demo")
saveWidget(final_map, "final_map_leaflet_demo.html", selfcontained = TRUE)
