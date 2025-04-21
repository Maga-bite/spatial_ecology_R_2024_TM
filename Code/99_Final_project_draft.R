# Spatial Ecology Project: NDVI & Drought Impact - Province of Sondrio
# Author: Tommaso Magarotto
# Goal: Analyze the impact of drought on vegetation using NDVI (Sentinel-2) 
# and climate data, aggregated by season over the last 5 years

# The time period we will focus on is from 2019 to 2023. 
# During this period, Italy experienced several extreme events, including droughts and floods. 
# Our goal is to assess the impact of these extreme events on mountainous ecosystems, 
# particularly in relation to their resilience and overall functioning.

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


# Plant seasonal phases:

# Avoiding the dormancy period cause NDVI dosen't see the photosynthetic activity since it's too low or even absent

# Vegetative awakening (Spring): May 10 – June 9
# During this phase, the first leaves start to appear, with flowering and the beginning of growth. 
# The NDVI (Normalized Difference Vegetation Index) shows rapid growth.
#
# Maximum activity (Early Summer): June 25 – July 24
# This is the period of maximum photosynthesis, with full leaf coverage. NDVI reaches its peak.
#
# Summer stress (Late Summer): August 9 – September 9
# During this phase, plants may experience water stress. NDVI stabilizes or shows a slight decrease.
#
# Early senescence (Pre-Autumn): September 25 – October 25
# Leaves begin to yellow, and photosynthetic activity decreases. NDVI shows a noticeable drop.
#
# The selected time intervals for each phase correspond to approximately 30 days, representing the critical periods of 
# plant growth, stress, and senescence. These will be used to analyze the impact of drought on vegetation over the years. 
# This approach helps monitor the seasonal evolution of NDVI and identify anomalies related to extreme events (drought, floods) 
# observed in Italy between 2022 and 2024.


#Image format:

#In sintesi:
#Usa i layer: True Color, NDVI, False Color (urban)
#Scarica B04, B08, B02, B03, B11, B12
#Risoluzione: 10 m dove possibile


#TIFF (32-bit float)
#Image resolution: HIGH
#2500 x 2308 px
#Coordinate system:
#UTM 32N (EPSG:32632)
#Projected resolution: 18 m/px

#ADD DATAMASK TO THE NEWLY DOWNLOADED IMAGES

#---------------------------------------------------------------
#Now we are trying to load the images in Tiff on R

getwd()
#"C:/Users/Tommy/Documents/altavaltellian"

list.files()

tif_files <- list.files(pattern = "\\.tiff$", full.names = TRUE)
tif_files_sorted <- sort(tif_files)  # Assumendo che l'ordine alfabetico rispecchi quello temporale


#MOMENTO SMANETTONE
# Carica il pacchetto raster
library(raster)

# Carica l'immagine .tiff
image_path_trial <- "2022-06-25-00_00_2022-07-24-23_59_Sentinel-2_L2A_True_color.tiff"
raster_image_trial <- raster(image_path_trial)

image_path_trial2 <-"2022-06-25-00_00_2022-07-24-23_59_Sentinel-2_L2A_False_color.tiff"
raster_image_trial2 <- raster(image_path_trial2)

# Visualizza l'immagine
plot(raster_image_trial, main = "Sentinel-2 True Color Image")

#ESEMPIO DI COME ABBIAMO FATTO A LEZIONE MA VA FATTO CON LE BANDE
difgr = raster_image_trial - raster_image_trial2
plot(difgr)


#ESEMPIO DI COME SOVRAPPORRE I PLOT

# Trasforma le geometrie nel CRS del raster
selected_municipalities <- st_transform(selected_municipalities, crs = crs(raster_image_trial))
bbox_polygon <- st_transform(bbox_polygon, crs = crs(raster_image_trial))

# Plot raster
plot(raster_image_trial, main = "Sentinel-2 True Color Image")

# Aggiungi geometrie
plot(st_geometry(bbox_polygon), add = TRUE, border = "red", lwd = 2)
plot(st_geometry(selected_municipalities), add = TRUE, 
     col = NA, border = "blue")

