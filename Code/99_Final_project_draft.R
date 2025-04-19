# Spatial Ecology Project: NDVI & Drought Impact - Municipalities of interest (in the Province of Sondrio)
# Author: Tommaso Magarotto
# Goal: Analyze the impact of drought on vegetation using NDVI (Sentinel-2) 
# and climate data, aggregated by season over the last 5 years

# The period we will focus on is from 2019 to 2023. 
# Italy experienced several extreme events during this period, including droughts and floods. 
# Our goal is to assess the impact of these extreme events on mountainous ecosystems, 
# particularly concerning their resilience and overall functioning.

# Load required libraries
library(sf)         # for handling spatial vector data (e.g., polygons, shapefiles)
library(geodata)    # for downloading administrative boundaries (GADM dataset)

# Download the administrative boundaries of Italy - level 3 (i.e., municipalities)
# 'level = 3' gives you the most detailed administrative unit (municipalities)
# 'path = tempdir()' stores the data in a temporary directory
italy_admin3 <- gadm(country = "ITA", level = 3, path = tempdir())

# Convert the downloaded object to a simple feature (sf) object
italy_admin3 <- st_as_sf(italy_admin3)

# Define the municipalities of interest (in the Province of Sondrio)
# These are selected because they are key mountain municipalities often impacted by drought
target_municipalities <- c("Livigno", "Valdidentro", "Valdisotto", "Bormio",
                           "Valfurva", "Grosio", "Sondalo", "Grosotto")

# Filter the dataset to keep only the selected municipalities
# 'NAME_3' is the GADM field for municipality names
selected_municipalities <- italy_admin3[italy_admin3$NAME_3 %in% target_municipalities, ]

# Ensure that geometries are valid (sometimes features may have topology issues)
# This is a good practice before doing spatial operations like bounding boxes or overlays
selected_municipalities <- st_make_valid(selected_municipalities)

# Create a bounding box that fully contains the selected municipalities
# This generates a rectangle (bbox) that minimally wraps all selected areas
bbox <- st_bbox(selected_municipalities)

# Convert the bounding box to a spatial feature (polygon)
bbox_polygon <- st_as_sfc(bbox)

# Wrap the geometry into an sf object and assign it the same CRS (coordinate system) 
# as the municipalities — very important for consistent plotting and spatial analysis
bbox_polygon <- st_sf(geometry = bbox_polygon, crs = st_crs(selected_municipalities))

# Plot the bounding box (in red) and the selected municipalities (in transparent blue)
# This helps visualize how the municipalities are spatially distributed inside the bbox
plot(st_geometry(bbox_polygon), border = "red", lwd = 2, 
     main = "Bounding Box + Selected Municipalities (GADM)")
plot(st_geometry(selected_municipalities), add = TRUE, 
     col = rgb(0, 0, 1, 0.3), border = "blue")

# Export or print the bounding box in WKT (Well-Known Text) format
# Useful for logging, sharing geometry in text form, or using in GIS software
st_as_text(st_geometry(bbox_polygon))


#THIS WAY
#Image format:

#TIFF (32-bit float)
#Image resolution: HIGH
#2500 x 2308 px
#Coordinate system:
#UTM 32N (EPSG:32632)
#Projected resolution: 18 m/px
