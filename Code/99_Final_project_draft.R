# Spatial Ecology Project: NDVI & Drought Impact - Province of Sondrio
# Author: Tommaso Magarotto

# Goal: Analyze the impact of drought on vegetation using NDVI (Sentinel-2) 
# and climate data, aggregated by season over the last 3 years

# The time period we will focus on is from 2022 to 2024
# During this period, Italy experienced several extreme events, including droughts and floods. 
# The goal is to assess the impact of these extreme events on mountainous ecosystems, 
# particularly in relation to their resilience and overall functioning.

# Download the administrative boundaries

# Load required libraries
library(sf)         # for handling spatial vector data (e.g., polygons, shapefiles)
library(geodata)    # for downloading administrative boundaries (GADM dataset)
# Palette viridis
library(viridis)
clv <- viridis(100)

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
#"POLYGON ((10.03806 46.26057, 10.63152 46.26057, 10.63152 46.63806, 10.03806 46.63806, 10.03806 46.26057))"

#_______________________________________________________________________________

# Plant seasonal phases:

# Avoiding the dormancy period because NDVI dosen't see the photosynthetic activity since it's too low or even absent

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
# observed in Italy between 2022 and 2023.


#Image format:

#In sintesi:
#Usa i layer: True Color, NDVI, False Color (urban)
#Scarica B03, B04, B05, B06 B08, B08A B11, B12
#Risoluzione: 10 m dove possibile

#TIFF (32-bit float)
#Image resolution: HIGH
#2500 x 2308 px
#Coordinate system:
#UTM 32N (EPSG:32632)
#Projected resolution: 18 m/px
# Data mask

#_______________________________________________________________________________

#Now we are trying to load the images in Tiff on R
setwd("C:/Users/Tommy/Documents/altavaltellian")

getwd()
#"C:/Users/Tommy/Documents/altavaltellian"

list.files()

tif_files <- list.files(pattern = "\\.tiff$", full.names = TRUE)

library(raster)
library(terra)

# List all .tiff files in the current directory
tif_files <- list.files(pattern = "\\.tiff$", full.names = TRUE)
print(tif_files)

# 2. Separa i file in base al nome
tif_true_color <- tif_files[grepl("True_color", tif_files)]
tif_NDVI <- tif_files[grepl("NDVI", tif_files)]
tif_false_color <- tif_files[grepl("False_color", tif_files)]

# 3. Crea liste di raster
rasters_true_color <- lapply(tif_true_color, terra::rast)
rasters_NDVI <- lapply(tif_NDVI, terra::rast)
rasters_false_color <- lapply(tif_false_color, terra::rast)

rasters_true_color[[1]]
plotRGB(rasters_true_color[[1]], r = 1, g = 2, b = 3, stretch = "lin")

# Now a check on the state of the tiff files and the possibility to plot them

#raster of all the files
rasters_files <- lapply(tif_files, terra::rast)

rasters_files

# 3-----------------------------------------------------------------------------
rasters_false_color[[3]]
#source: 2022-08-09-00_00_2022-09-09-23_59_Sentinel-2_L2A_False_color.tiff

names(rasters_files[[26]])
names(rasters_files[[27]])
names(rasters_files[[30]])

b4_2289_2299 <- rasters_files[[26]][[1]] # red
b3_2289_2299 <- rasters_files[[27]][[1]] # green
b8_2289_2299 <- rasters_files[[30]][[1]] # (NIR) (8)

# Seleziona il primo layer (ad esempio se ogni file ha 2 layer temporali)
nir_2289_2299 <- b8_2289_2299[[1]]
red_2289_2299 <- b4_2289_2299[[1]]
green_2289_2299 <- b3_2289_2299[[1]]

# Stack delle bande per il false color
false_color_2289_2299 <- c(nir_2289_2299, red_2289_2299, green_2289_2299)

# Plot false color maxcell=inf serve per forzare a usare tutti i pixel. 
plotRGB(false_color_2289_2299, r = 1, g = 2, b = 3, scale = 10000, stretch = "lin", maxcell = Inf)

# 3-----------------------------------------------------------------------------
rasters_true_color[[3]]
#source: 2022-08-09-00_00_2022-09-09-23_59_Sentinel-2_L2A_True_color.tiff
names(rasters_files[[25]])
names(rasters_files[[26]])
names(rasters_files[[27]])

# loading rasters corresponding to the bands of true color
b2_2289_2299 <- rasters_files[[25]][[1]]  # Blu
b3_2289_2299 <- rasters_files[[26]][[1]]  # Verde
b4_2289_2299 <- rasters_files[[27]][[1]]  # Rosso

# Seleziona il primo layer (ad esempio se ogni file ha 2 layer temporali)
red_2289_2299 <- b4_2289_2299[[1]]
green_2289_2299 <- b3_2289_2299[[1]]
blue_2289_2299 <- b2_2289_2299[[1]]

# Stack delle bande RGB
true_color_2289_2299 <- c(red_2289_2299, green_2289_2299, blue_2289_2299)

# Plot RGB
plotRGB(true_color_2289_2299, r = 1, g = 2, b = 3, scale=10000, stretch = "hist")

#maxcel=inf serve per forzare a usare tutti i pixel. Di default ne usa solo un sottoinsieme se l'immagine è grande.
plotRGB(true_color_2289_2299, r = 1, g = 2, b = 3, scale = 10000, stretch = "hist", maxcell = Inf)

# 6-----------------------------------------------------------------------------
rasters_true_color[[6]]
#source: 2023-06-25-00_00_2023-07-25-23_59_Sentinel-2_L2A_True_color.tiff 
names(rasters_files[[61]])
names(rasters_files[[62]])
names(rasters_files[[63]])

# loading rasters corresponding to the bands of true color
b2_23625_23725 <- rasters_files[[61]][[1]]  # Blu
b3_23625_23725 <- rasters_files[[62]][[1]]  # Verde
b4_23625_23725 <- rasters_files[[63]][[1]]  # Rosso

# Seleziona il primo layer (ad esempio se ogni file ha 2 layer temporali)
red_23625_23725 <- b4_23625_23725[[1]]
green_23625_23725 <- b3_23625_23725[[1]]
blue_23625_23725 <- b2_23625_23725[[1]]

# Stack delle bande RGB
true_color_23625_23725 <- c(red_23625_23725, green_23625_23725, blue_23625_23725)

# Plot RGB
plotRGB(true_color_23625_23725, r = 1, g = 2, b = 3, scale=10000, stretch = "hist")

#maxcel=inf serve per forzare a usare tutti i pixel. Di default ne usa solo un sottoinsieme se l'immagine è grande.
plot_truecolor_6 <- plotRGB(true_color_23625_23725, r = 1, g = 2, b = 3, scale = 10000, stretch = "hist", maxcell = Inf)

dev.off()

#_______________________________________________________________________________

# True Color Final
par(mfrow = c(2, 4), mar = c(1, 2, 3, 1), oma = c(0, 3, 3, 2))  # Margini e layout

# 2022
plotRGB(rasters_true_color[[1]], r = 1, g = 2, b = 3, stretch = "lin")
mtext("05.10–06.09", side = 3, line = 1, cex = 1)
mtext("2022", side = 2, line = 2, cex = 1.3, las = 3)
plotRGB(rasters_true_color[[2]], r = 1, g = 2, b = 3, stretch = "lin")
mtext("06.25–07.25", side = 3, line = 1, cex = 1)
plotRGB(true_color_2289_2299, r = 1, g = 2, b = 3, scale = 10000, stretch = "hist", maxcell = Inf)
mtext("08.09–09.09", side = 3, line = 1, cex = 1)
plotRGB(rasters_true_color[[4]], r = 1, g = 2, b = 3, stretch = "lin")
mtext("09.25–10.25", side = 3, line = 1, cex = 1)

# 2023
plotRGB(rasters_true_color[[5]], r = 1, g = 2, b = 3, stretch = "lin")
mtext("05.10–06.09", side = 3, line = 1, cex = 1)
mtext("2023", side = 2, line = 2, cex = 1.3, las = 3)
plotRGB(true_color_23625_23725, r = 1, g = 2, b = 3, scale = 10000, stretch = "hist", maxcell = Inf)
mtext("06.25–07.25", side = 3, line = 1, cex = 1)
plotRGB(rasters_true_color[[7]], r = 1, g = 2, b = 3, stretch = "lin")
mtext("08.09–09.09", side = 3, line = 1, cex = 1)
plotRGB(rasters_true_color[[8]], r = 1, g = 2, b = 3, stretch = "lin")
mtext("09.25–10.25", side = 3, line = 1, cex = 1)

# General title
mtext("True Color Sentinel-2", outer = TRUE, side = 3, line = 1, cex = 1.5)

dev.off()


#False color final

par(mfrow = c(2, 4), mar = c(1, 2, 3, 1), oma = c(0, 3, 3, 2))

# 2022
plotRGB(rasters_false_color[[1]], r = 1, g = 2, b = 3, stretch = "lin")
mtext("05.10–06.09", side = 3, line = 1, cex = 1)
mtext("2022", side = 2, line = 2, cex = 1.3, las = 3)
plotRGB(rasters_false_color[[2]], r = 1, g = 2, b = 3, stretch = "lin")
mtext("06.25–07.25", side = 3, line = 1, cex = 1)
plotRGB(false_color_2289_2299, r = 1, g = 2, b = 3, scale = 10000, stretch = "lin", maxcell = Inf)
mtext("08.09–09.09", side = 3, line = 1, cex = 1)
plotRGB(rasters_false_color[[4]], r = 1, g = 2, b = 3, stretch = "lin")
mtext("09.25–10.25", side = 3, line = 1, cex = 1)

# 2023
plotRGB(rasters_false_color[[5]], r = 1, g = 2, b = 3, stretch = "lin")
mtext("05.10–06.09", side = 3, line = 1, cex = 1)
mtext("2023", side = 2, line = 2, cex = 1.3, las = 3)
plotRGB(rasters_false_color[[6]], r = 1, g = 2, b = 3, stretch = "lin")
mtext("06.25–07.25", side = 3, line = 1, cex = 1)
plotRGB(rasters_false_color[[7]], r = 1, g = 2, b = 3, stretch = "lin")
mtext("08.09–09.09", side = 3, line = 1, cex = 1)
plotRGB(rasters_false_color[[8]], r = 1, g = 2, b = 3, stretch = "lin")
mtext("09.25–10.25", side = 3, line = 1, cex = 1)

# General title
mtext("False Color Sentinel-2", outer = TRUE, side = 3, line = 1, cex = 1.5)

dev.off()


#NDVI final 

par(mfrow = c(2, 4), mar = c(1, 2, 3, 1), oma = c(0, 3, 3, 2))

# 2022
plotRGB(rasters_NDVI[[1]], r = 1, g = 2, b = 3, stretch = "lin")
mtext("05.10–06.09", side = 3, line = 1, cex = 0.8)
mtext("2022", side = 2, line = 2, cex = 1, las = 3)
plotRGB(rasters_NDVI[[2]], r = 1, g = 2, b = 3, stretch = "lin")
mtext("06.25–07.25", side = 3, line = 1, cex = 0.8)
plotRGB(rasters_NDVI[[3]], r = 1, g = 2, b = 3, stretch = "lin")
mtext("08.09–09.09", side = 3, line = 1, cex = 0.8)
plotRGB(rasters_NDVI[[4]], r = 1, g = 2, b = 3, stretch = "lin")
mtext("09.25–10.25", side = 3, line = 1, cex = 0.8)

# 2023
plotRGB(rasters_NDVI[[5]], r = 1, g = 2, b = 3, stretch = "lin")
mtext("05.10–06.09", side = 3, line = 1, cex = 0.8)
mtext("2023", side = 2, line = 2, cex = 0.9, las = 3)
plotRGB(rasters_NDVI[[6]], r = 1, g = 2, b = 3, stretch = "lin")
mtext("06.25–07.25", side = 3, line = 1, cex = 0.8)
plotRGB(rasters_NDVI[[7]], r = 1, g = 2, b = 3, stretch = "lin")
mtext("08.09–09.09", side = 3, line = 1, cex = 0.8)
plotRGB(rasters_NDVI[[8]], r = 1, g = 2, b = 3, stretch = "lin")
mtext("09.25–10.25", side = 3, line = 1, cex = 0.8)

# General title
mtext("NDVI RGB Composite", outer = TRUE, side = 3, line = 1, cex = 1.2)

dev.off()

#we made it and 

#_______________________________________________________________________________


#ok now we have to progress further with the analysis

tif_files <- list.files(pattern = "\\.tiff$", full.names = TRUE)

library(raster)
library(terra)

# List all .tiff files in the current directory
tif_files <- list.files(pattern = "\\.tiff$", full.names = TRUE)
print(tif_files)

tif_B04 <- tif_files[grepl("B04", tif_files)]
tif_B08 <- tif_files[grepl("B08", tif_files)]
tif_B11 <- tif_files[grepl("B11", tif_files)]

rasters_tif_B04 <- lapply(tif_B04, terra::rast)
rasters_tif_B08 <- lapply(tif_B08, terra::rast)
rasters_tif_B11 <- lapply(tif_B11, terra::rast)

#05102*_06092* → dal 10 maggio al 9 giugno
#06252*_07252* → dal 25 giugno al 25 luglio
#08092*_09092* → dal 9 agosto al 9 settembre
#09252*_10252* → dal 25 settembre al 25 ottobre

# Required bands for analysis:
# - NDVI (Normalized Difference Vegetation Index):
#   - Red (Band 4) and NIR (Band 8)
#   - Formula: NDVI = (NIR - Red) / (NIR + Red)
#   - Used to assess vegetation health.
#   - NDVI may already be present in the files, but it can be recalculated if needed.

# red band (B04)
redband04_051022_060922 <- rasters_tif_B04[[1]][[1]]
redband04_062522_072522 <- rasters_tif_B04[[2]][[1]]
redband04_080922_090922 <- rasters_tif_B04[[3]][[1]]
redband04_092522_102522 <- rasters_tif_B04[[4]][[1]]

redband04_051023_060923 <- rasters_tif_B04[[5]][[1]]
redband04_062523_072523 <- rasters_tif_B04[[6]][[1]]
redband04_080923_090923 <- rasters_tif_B04[[7]][[1]]
redband04_092523_102523 <- rasters_tif_B04[[8]][[1]]

# NIR band (B08)
nirband08_051022_060922 <- rasters_tif_B08[[1]][[1]]
nirband08_062522_072522 <- rasters_tif_B08[[2]][[1]]
nirband08_080922_090922 <- rasters_tif_B08[[3]][[1]]
nirband08_092522_102522 <- rasters_tif_B08[[4]][[1]]

nirband08_051023_060923 <- rasters_tif_B08[[5]][[1]]
nirband08_062523_072523 <- rasters_tif_B08[[6]][[1]]
nirband08_080923_090923 <- rasters_tif_B08[[7]][[1]]
nirband08_092523_102523 <- rasters_tif_B08[[8]][[1]]

# SWIR band (B11)
swirband11_051022_060922 <- rasters_tif_B11[[1]][[1]]
swirband11_062522_072522 <- rasters_tif_B11[[2]][[1]]
swirband11_080922_090922 <- rasters_tif_B11[[3]][[1]]
swirband11_092522_102522 <- rasters_tif_B11[[4]][[1]]

swirband11_051023_060923 <- rasters_tif_B11[[5]][[1]]
swirband11_062523_072523 <- rasters_tif_B11[[6]][[1]]
swirband11_080923_090923 <- rasters_tif_B11[[7]][[1]]
swirband11_092523_102523 <- rasters_tif_B11[[8]][[1]]

#NDVI = (NIR - Red) / (NIR + Red)

# Proietta il NIR (in gradi) sul sistema di coordinate del raster Red (in metri)
nir_projected <- terra::project(nirband08_062522_072522, redband04_062522_072522)
NDVI_062522_072522 <- (nir_projected - redband04_062522_072522) / (nir_projected + redband04_062522_072522)

# NDVI 2022
NDVI_051022_060922 <- (nirband08_051022_060922 - redband04_051022_060922) / (nirband08_051022_060922 + redband04_051022_060922)
NDVI_062522_072522 <- (nir_projected - redband04_062522_072522) / (nir_projected + redband04_062522_072522)
NDVI_080922_090922 <- (nirband08_080922_090922 - redband04_080922_090922) / (nirband08_080922_090922 + redband04_080922_090922)
NDVI_092522_102522 <- (nirband08_092522_102522 - redband04_092522_102522) / (nirband08_092522_102522 + redband04_092522_102522)

# NDVI 2023
NDVI_051023_060923 <- (nirband08_051023_060923 - redband04_051023_060923) / (nirband08_051023_060923 + redband04_051023_060923)
NDVI_062523_072523 <- (nirband08_062523_072523 - redband04_062523_072523) / (nirband08_062523_072523 + redband04_062523_072523)
NDVI_080923_090923 <- (nirband08_080923_090923 - redband04_080923_090923) / (nirband08_080923_090923 + redband04_080923_090923)
NDVI_092523_102523 <- (nirband08_092523_102523 - redband04_092523_102523) / (nirband08_092523_102523 + redband04_092523_102523)

plot(NDVI_051022_060922[[1]], col = clv, main = "NDVI 05.10–06.09 2022")
plot(NDVI_062522_072522[[1]], col = clv, main = "NDVI 06.25–07.25 2022")
plot(NDVI_080922_090922[[1]], col = clv, main = "NDVI 08.09–09.09 2022")
plot(NDVI_092522_102522[[1]], col = clv, main = "NDVI 09.25–10.25 2022")

plot(NDVI_051023_060923[[1]], col = clv, main = "NDVI 05.10–06.09 2023")
plot(NDVI_062523_072523[[1]], col = clv, main = "NDVI 06.25–07.25 2023")
plot(NDVI_080923_090923[[1]], col = clv, main = "NDVI 08.09–09.09 2023")
plot(NDVI_092523_102523[[1]], col = clv, main = "NDVI 09.25–10.25 2023")

dev.off()

# - NDWI (Normalized Difference Water Index):
#   - NIR (Band 8) and SWIR1 (Band 11)
#   - Formula: NDWI = (NIR - SWIR1) / (NIR + SWIR1)
#   - Used to distinguish between water bodies and stressed vegetation (e.g., drier areas).

# NDWI = (NIR - SWIR1) / (NIR + SWIR1)
# NDWI 2022

NDWI_051022_060922 <- (nirband08_051022_060922 - swirband11_051022_060922) / (nirband08_051022_060922 + swirband11_051022_060922)
NDWI_062522_072522 <- (nirband08_062522_072522 - swirband11_062522_072522) / (nirband08_062522_072522 + swirband11_062522_072522)
NDWI_080922_090922 <- (nirband08_080922_090922 - swirband11_080922_090922) / (nirband08_080922_090922 + swirband11_080922_090922)
NDWI_092522_102522 <- (nirband08_092522_102522 - swirband11_092522_102522) / (nirband08_092522_102522 + swirband11_092522_102522)

# NDWI 2023
NDWI_051023_060923 <- (nirband08_051023_060923 - swirband11_051023_060923) / (nirband08_051023_060923 + swirband11_051023_060923)
NDWI_062523_072523 <- (nirband08_062523_072523 - swirband11_062523_072523) / (nirband08_062523_072523 + swirband11_062523_072523)
NDWI_080923_090923 <- (nirband08_080923_090923 - swirband11_080923_090923) / (nirband08_080923_090923 + swirband11_080923_090923)
NDWI_092523_102523 <- (nirband08_092523_102523 - swirband11_092523_102523) / (nirband08_092523_102523 + swirband11_092523_102523)

plot(NDWI_051022_060922[[1]], col = clv, main = "NDWI 05.10–06.09 2022")
plot(NDWI_062522_072522[[1]], col = clv, main = "NDWI 06.25–07.25 2022")
plot(NDWI_080922_090922[[1]], col = clv, main = "NDWI 08.09–09.09 2022")
plot(NDWI_092522_102522[[1]], col = clv, main = "NDWI 09.25–10.25 2022")

plot(NDWI_051023_060923[[1]], col = clv, main = "NDWI 05.10–06.09 2023")
plot(NDWI_062523_072523[[1]], col = clv, main = "NDWI 06.25–07.25 2023")
plot(NDWI_080923_090923[[1]], col = clv, main = "NDWI 08.09–09.09 2023")
plot(NDWI_092523_102523[[1]], col = clv, main = "NDWI 09.25–10.25 2023")

# The results of NDVI and NDWI are used to analyze vegetation health, water stress, and drought effects.

# Calcolo delle differenze NDVI tra 2023 e 2022
difNDVI_05 <- NDVI_051023_060923 - NDVI_051022_060922
difNDVI_06 <- NDVI_062523_072523 - NDVI_062522_072522
difNDVI_08 <- NDVI_080923_090923 - NDVI_080922_090922
difNDVI_09 <- NDVI_092523_102523 - NDVI_092522_102522

# Visualizzazione delle mappe di differenza in griglia 2x2
par(mfrow = c(2, 2))
plot(difNDVI_05[[1]], col = clv, main = "NDVI Diff: 05.10–06.09")
plot(difNDVI_06[[1]], col = clv, main = "NDVI Diff: 06.25–07.25")
plot(difNDVI_08[[1]], col = clv, main = "NDVI Diff: 08.09–09.09")
plot(difNDVI_09[[1]], col = clv, main = "NDVI Diff: 09.25–10.25")


dev.off()
































#_______________________________________________________________________________
#this cycle of commands plots even the single bands so it can be seen which are critical
#the plots that gives a warning or an error are re-downloaded but there are images that are not being downloaded correclty for their composites

#for (i in seq_along(rasters_files)) {
#  cat("Plotting raster", i, "\n")
#  tryCatch({
#    plot(rasters_files[[i]], stretch = "lin")
#  }, error = function(e) {
#    cat("Xo Error in raster", i, ":", conditionMessage(e), "\n")
#  }, warning = function(w) {
#    cat(":O Warning in raster", i, ":", conditionMessage(w), "\n")
#  })
#}

## WARNING: The following Sentinel-2 files are corrupted and must be re-downloaded.
# Critical date ranges and required bands/composites:
# - 2022-06-25 to 2022-07-25: Bands B02, B05, B06, B08, B11, B8A + False_color, True_color composites
# - 2023-05-10 to 2023-06-09: Bands B02, B03, B04, B05, B06, B08, B11, B12, B8A + False_color, True_color composites
# Now that the rasters are mostly working,g we need to load and create the composite rasters that are not being downloaded correctly

#_______________________________________________________________________________

#par(mfrow=c(2,4))
#plotRGB(rasters_NDVI[[1]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_NDVI[[2]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_NDVI[[3]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_NDVI[[4]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_NDVI[[5]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_NDVI[[6]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_NDVI[[7]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_NDVI[[8]], r = 1, g = 2, b = 3, stretch = "lin")


#_______________________________________________________________________________

#par(mfrow=c(2,4))
#plotRGB(rasters_false_color[[1]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_false_color[[2]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_false_color[[3]], r = 1, g = 2, b = 3, stretch = "lin") #error
#plotRGB(rasters_false_color[[4]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_false_color[[5]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_false_color[[6]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_false_color[[7]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_false_color[[8]], r = 1, g = 2, b = 3, stretch = "lin")


##par(mfrow=c(2,4))
#plotRGB(rasters_false_color[[1]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_false_color[[2]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(false_color_2289_2299, r = 1, g = 2, b = 3, scale = 10000, stretch = "lin", maxcell = Inf)
#plotRGB(rasters_false_color[[4]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_false_color[[5]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_false_color[[6]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_false_color[[7]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_false_color[[8]], r = 1, g = 2, b = 3, stretch = "lin")

#_______________________________________________________________________________

#par(mfrow=c(2,4))
#plotRGB(rasters_true_color[[1]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_true_color[[2]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_true_color[[3]], r = 1, g = 2, b = 3, stretch = "lin") #error not plotting
#plotRGB(rasters_true_color[[4]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_true_color[[5]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_true_color[[6]], r = 1, g = 2, b = 3, stretch = "lin") #error not plotting
#plotRGB(rasters_true_color[[7]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_true_color[[8]], r = 1, g = 2, b = 3, stretch = "lin")

# working on the natural color trough the bands

# Final multiframe for True color
#par(mfrow=c(2,4))
#plotRGB(rasters_true_color[[1]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_true_color[[2]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(true_color_2289_2299, r = 1, g = 2, b = 3, scale = 10000, stretch = "hist", maxcell = Inf)
#plotRGB(rasters_true_color[[4]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_true_color[[5]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(true_color_23625_23725, r = 1, g = 2, b = 3, scale = 10000, stretch = "hist", maxcell = Inf)
#plotRGB(rasters_true_color[[7]], r = 1, g = 2, b = 3, stretch = "lin")
#plotRGB(rasters_true_color[[8]], r = 1, g = 2, b = 3, stretch = "lin")


#_______________________________________________________________________________

#cosa che sarebbe stata utile ma non ha funzionato

#si riescono a plottare tutti, in alcuni mostra un multiframe in cui c'è il plot e la zona di raster
#ora provo a vedere se i raster dei file creano ancora problemi se vengono rivisti dal seguente cosice
  
# Percorso dell'eseguibile gdal_translate
gdal_path <- "C:/2)UNIBO/roba strana per dati tiff/bin/gdal_translate.exe"
  
# Cartella dove sono i file .tiff originali
input_dir <- "C:/Users/Tommy/Documents/altavaltellian"
  
# Cartella dove salvare i file riparati
output_dir <- file.path(input_dir, "riparati")
  
# Crea la cartella di output se non esiste
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}
  
# Lista dei file .tiff da convertire
tif_files <- list.files(input_dir, pattern = "\\.tiff$", full.names = TRUE)
  
# Ciclo su tutti i file e li converte
for (file in tif_files) {
  filename <- basename(file)
  output_file <- file.path(output_dir, paste0("fix_", filename))
    
  cmd <- sprintf('"%s" "%s" "%s"', gdal_path, file, output_file)
  cat("➡️ Eseguendo:", cmd, "\n")
    
  status <- system(cmd)
  if (status != 0) {
    cat("Errore nel convertire:", filename, "\n")
  } else {
    cat("Fatto:", filename, "\n")
  }
}
  
#non ha funzionato quindi vanno riscaricati
  
#_______________________________________________________________________________
  


#_______________________________________________________________________________

#
# VOLEVO CONTROLLARE SE IL BOUNDING BOX FUNZIONA CON LE IMMAGINI SCARICATE
#

# Load required libraries
library(terra)    # For raster operations
library(sf)       # For vector data handling
library(ggplot2)  # For elegant spatial plotting

# ---------------------------------------------
# STEP 1: Logical Check - Intersections with all rasters
# ---------------------------------------------

# Get the CRS from one of the rasters (assuming all share the same CRS)
crs_raster <- crs(rasters_true_color[[1]])

# Ensure municipalities are projected to the raster's CRS
selected_municipalities_proj <- st_transform(selected_municipalities, crs = crs_raster)

# Logical check: loop over each raster and test intersection
intersection_results <- sapply(seq_along(rasters_true_color), function(i) {
  
  # Convert raster to polygons (heavy operation!)
  rast_poly <- as.polygons(rasters_true_color[[i]])
  rast_poly_sf <- st_as_sf(rast_poly)
  st_crs(rast_poly_sf) <- crs_raster
  
  # Check intersection
  intersection <- st_intersects(
    st_geometry(selected_municipalities_proj),
    st_geometry(rast_poly_sf),sparse = FALSE)
  
  any(intersection)
})

# Show logical vector of which rasters intersect
print(intersection_results)

#print(intersection_results)
#[1]  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE


# ---------------------------------------------
# STEP 2: Plotting Example (only for the first intersecting raster)
# ---------------------------------------------

# Find the index of the first intersecting raster (if any)
first_valid_index <- which(intersection_results)[1]

if (!is.na(first_valid_index)) {
  
  # Convert that raster to polygons
  example_raster <- rasters_true_color[[first_valid_index]]
  rast_poly <- as.polygons(example_raster)
  rast_poly_sf <- st_as_sf(rast_poly)
  st_crs(rast_poly_sf) <- crs_raster
  
  # Plot with ggplot2 — EXAMPLE ONLY
  ggplot() +
    geom_sf(data = selected_municipalities_proj, aes(fill = "Municipalities"), 
            color = "blue", alpha = 0.3) + 
    geom_sf(data = rast_poly_sf, aes(fill = "Raster"), 
            color = "red", alpha = 0.1) +
    theme_minimal() +
    labs(title = paste("EXAMPLE - Raster", first_valid_index, "and Municipalities of Sondrio Province"),
         subtitle = "Raster-to-polygon is computationally intensive — done only once here",
         fill = "Legend") +
    scale_fill_manual(values = c("Municipalities" = "blue", "Raster" = "red"))
  
} else {
  message("No rasters intersect the selected municipalities.")
}

#_______________________________________________________________________________

#
# VOLEVO CONTROLLARE SE IL BOUNDING BOX FUNZIONA CON LE IMMAGINI SCARICATE
#

# Load required libraries
library(terra)    # For raster operations
library(sf)       # For vector data handling
library(ggplot2)  # For elegant spatial plotting

# ---------------------------------------------
# STEP 1: Logical Check - Intersections with all rasters
# ---------------------------------------------

# Get the CRS from one of the rasters (assuming all share the same CRS)
crs_raster <- crs(rasters_true_color[[1]])

# Ensure municipalities are projected to the raster's CRS
selected_municipalities_proj <- st_transform(selected_municipalities, crs = crs_raster)

# Logical check: loop over each raster and test intersection
intersection_results <- sapply(seq_along(rasters_true_color), function(i) {
  
  # Convert raster to polygons (heavy operation!)
  rast_poly <- as.polygons(rasters_true_color[[i]])
  rast_poly_sf <- st_as_sf(rast_poly)
  st_crs(rast_poly_sf) <- crs_raster
  
  # Check intersection
  intersection <- st_intersects(
    st_geometry(selected_municipalities_proj),
    st_geometry(rast_poly_sf),sparse = FALSE)
  
  any(intersection)
})

# Show logical vector of which rasters intersect
print(intersection_results)

#print(intersection_results)
#[1]  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE


# ---------------------------------------------
# STEP 2: Plotting Example (only for the first intersecting raster)
# ---------------------------------------------

# Find the index of the first intersecting raster (if any)
first_valid_index <- which(intersection_results)[1]

if (!is.na(first_valid_index)) {
  
  # Convert that raster to polygons
  example_raster <- rasters_true_color[[first_valid_index]]
  rast_poly <- as.polygons(example_raster)
  rast_poly_sf <- st_as_sf(rast_poly)
  st_crs(rast_poly_sf) <- crs_raster
  
  # Plot with ggplot2 — EXAMPLE ONLY
  ggplot() +
    geom_sf(data = selected_municipalities_proj, aes(fill = "Municipalities"), 
            color = "blue", alpha = 0.3) + 
    geom_sf(data = rast_poly_sf, aes(fill = "Raster"), 
            color = "red", alpha = 0.1) +
    theme_minimal() +
    labs(title = paste("EXAMPLE - Raster", first_valid_index, "and Municipalities of Sondrio Province"),
         subtitle = "Raster-to-polygon is computationally intensive — done only once here",
         fill = "Legend") +
    scale_fill_manual(values = c("Municipalities" = "blue", "Raster" = "red"))
  
} else {
  message("No rasters intersect the selected municipalities.")
}

#_______________________________________________________________________________

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

#_______________________________________________________________________________

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

