# Spatial Ecology Project: NDVI & Drought Impact - Province of Sondrio
# Author: Tommaso Magarotto

# Goal: Analyze the impact of drought on vegetation using NDVI (Sentinel-2) 
# and climate data, aggregated by season

# The time period we will focus on is from 2022 to 2024
# During this period, Italy experienced several extreme events, including droughts and floods. 
# The goal is to assess the impact of these extreme events on mountainous ecosystems.

#_______________________________________________________________________________

# Uncomment and run the following line if you haven't installed the packages yet:
# install.packages(c("sf", "geodata", "viridis", "imageRy", "tidyr", "ggplot2", "patchwork", "raster", "terra"))


library(sf)         # For handling spatial vector data (e.g., polygons, shapefiles)
library(geodata)    # For downloading administrative boundaries (e.g., GADM dataset)
library(viridis)    # Color palettes optimized for visibility and accessibility
library(imageRy)    # Used for working with raster/image data, in this case for working with the classification of true color images
library(tidyr)      # For data wrangling and reshaping
library(ggplot2)    # For creating elegant data visualizations
library(patchwork)  # For combining multiple ggplot2 plots
library(raster)     # For working with raster data (e.g., satellite images)
library(terra)      # Newer alternative to 'raster' for raster and vector data

clv <- viridis(100)  # Create a continuous viridis color palette with 100 values

#_______________________________________________________________________________

# === Download the administrative boundaries ===
# Required libraries: sf and geodata

# Download the administrative boundaries of Italy - level 3 (i.e., municipalities)
# 'level = 3' gives you the most detailed administrative unit (municipalities)
# 'path = tempdir()' stores the data in a temporary directory
italy_admin3 <- gadm(country = "ITA", level = 3, path = tempdir())

#I dati scaricati non sono direttamente in formato sf (spatial feature)
# Downloaded data are not directly ready for spatial analysis so
# Convert the downloaded object to a simple feature (sf) object
italy_admin3 <- st_as_sf(italy_admin3)

# Define the municipalities of interest (in the Province of Sondrio)
# These are selected because they are key mountain municipalities
target_municipalities <- c("Livigno", "Valdidentro", "Valdisotto", "Bormio",
                           "Valfurva", "Grosio", "Sondalo", "Grosotto")

# Filter the dataset to keep only the selected municipalities
# 'NAME_3' is the GADM field for municipality names
selected_municipalities <- italy_admin3[italy_admin3$NAME_3 %in% target_municipalities, ]

# Ensure that geometries are valid (sometimes features may have topology issues)
# This is a good practice before doing spatial operations like bounding boxes or overlays
selected_municipalities <- st_make_valid(selected_municipalities)

# Create a bounding box that fully contains the selected municipalities
# st_bbox() generates a rectangle (bbox) that minimally wraps all selected areas 
bbox <- st_bbox(selected_municipalities)

# st_as_sfc() converts the bounding box to a spatial feature (polygon)
bbox_polygon <- st_as_sfc(bbox)

# Wrap the geometry into an sf object and assign it the same CRS (coordinate system) 
bbox_polygon <- st_sf(geometry = bbox_polygon, crs = st_crs(selected_municipalities))

# Plot the bounding box (in red) and the selected municipalities (in transparent blue)
# This helps visualize how the municipalities are spatially distributed inside the bbox
# st_geometry() gives back the polygon  without the extra collumns, usefull for drawing and analyzing  spatial components
plot(st_geometry(bbox_polygon), border = "red", lwd = 2, 
     main = "Bounding Box + Selected Municipalities (GADM)")
plot(st_geometry(selected_municipalities), add = TRUE, 
     col = rgb(0, 0, 1, 0.3), border = "blue")

# Export or print the bounding box in WKT (Well-Known Text) format
# Useful for logging, sharing geometry in text form, or using in GIS software
st_as_text(st_geometry(bbox_polygon))

#"POLYGON ((10.03806 46.26057, 10.63152 46.26057, 10.63152 46.63806, 10.03806 46.63806, 10.03806 46.26057))"

#_______________________________________________________________________________

#Images format:

#Summary of the downloaded images:
#TIFF (32-bit float)
#layer: True Color, NDVI, False Color (urban)
#single bands B02, B03, B04, B05, B06 B08, B08A B11, B12
#Image resolution: HIGH 2500 x 2308 px
#Coordinate system: UTM 32N (EPSG:32632)
#Projected resolution: 18 m/px
# Data mask

# Plant seasonal phases considerations
################################################################################

# Plant seasonal phases:
# Avoiding the dormancy period because NDVI don't see the photosynthetic activity since it's too low or even absent

# Vegetative awakening (Spring): May 10 – June 9
# During this phase, the first leaves appear, with flowering and the beginning of growth. 
# The NDVI (Normalized Difference Vegetation Index) should show low intensity but followed to a rapid growth.
#
# Maximum activity (Early Summer): June 25 – July 24
# This is the period of maximum photosynthesis, with full leaf coverage. NDVI reaches its peak.
#
# Summer stress (Late Summer): August 9 – September 9
# During this phase, plants may experience water or heat stress. NDVI stabilizes or shows a slight decrease.
#
# Early senescence (Pre-Autumn): September 25 – October 25
# Leaves begin to yellow, and photosynthetic activity decreases. NDVI shows a noticeable drop.
#
# The selected time intervals for each phase correspond to approximately 30 days, representing the critical periods of 
# plant growth, stress, and senescence. These will be used to analyze the impact of drought on vegetation over the years. 
# This approach helps monitor the seasonal evolution of NDVI and identify anomalies in Italy between 2022 and 2023.

################################################################################

#_______________________________________________________________________________

# === Load the images in Tiff on R and visualization ===
# Required libraries: raster and terra

setwd("C:/Users/Tommy/Documents/altavaltellian")
getwd()
#"C:/Users/Tommy/Documents/altavaltellian"

list.files()

# List of all the files in the chosen dirrectory
tif_files <- list.files(pattern = "\\.tiff$", full.names = TRUE)

# Grouping the file based on the layer that we want to analayse
# Searches for a specific string of text inside of the vector and gives back a logical vector that gives which elements arecontined in that string
tif_true_color <- tif_files[grepl("True_color", tif_files)]
tif_NDVI <- tif_files[grepl("NDVI", tif_files)]
tif_false_color <- tif_files[grepl("False_color", tif_files)]

# Creation of the raster applying terra::rast to all the files
# lapply(..., terra::rast): applies the function terra::rast() to every element
rasters_true_color <- lapply(tif_true_color, terra::rast)
rasters_NDVI <- lapply(tif_NDVI, terra::rast)
rasters_false_color <- lapply(tif_false_color, terra::rast)

# Example to see if the plot goes well
# Linear stretching of the pixel values for a better plotting. It is a visual normalization. 
rasters_true_color[[1]]
plotRGB(rasters_true_color[[1]], r = 1, g = 2, b = 3, stretch = "lin")

# After checking the available .tif files and attempting to plot them,
# it became clear that some images are corrupted or incomplete.
# Despite multiple download attempts, some files could not be retrieved correctly.
# Therefore, we need to reconstruct the missing composite layers manually.
# This step is essential in order to later build the complete set of final images.
rasters_files <- lapply(tif_files, terra::rast)

# 3-----------------------------------------------------------------------------
rasters_false_color[[3]] # Gives us information on the raster
#source: 2022-08-09-00_00_2022-09-09-23_59_Sentinel-2_L2A_False_color.tiff

names(rasters_files[[26]])
names(rasters_files[[27]])
names(rasters_files[[30]])

b4_2289_2299 <- rasters_files[[26]][[1]] # red
b3_2289_2299 <- rasters_files[[27]][[1]] # green
b8_2289_2299 <- rasters_files[[30]][[1]] # (NIR) (8)

# Extract the first layer of each band to create a single-date RGB image
nir_2289_2299 <- b8_2289_2299[[1]]
red_2289_2299 <- b4_2289_2299[[1]]
green_2289_2299 <- b3_2289_2299[[1]]

# Stack of the bands
false_color_2289_2299 <- c(nir_2289_2299, red_2289_2299, green_2289_2299)

# Plot false color maxcell=inf is needed to force the use of all the pixels from the file 
plotRGB(false_color_2289_2299, r = 1, g = 2, b = 3, scale = 10000, stretch = "lin", maxcell = Inf)

####
# The image appears with an unusual blue tint instead of the expected white.
# This is likely due to differences in how reflectance values are handled in R
# compared to how they are visualized in the original Sentinel-2 platform.
####

# 3-----------------------------------------------------------------------------
rasters_true_color[[3]]
#source: 2022-08-09-00_00_2022-09-09-23_59_Sentinel-2_L2A_True_color.tiff
names(rasters_files[[25]])
names(rasters_files[[26]])
names(rasters_files[[27]])

# loading rasters corresponding to the bands of true color
b2_2289_2299 <- rasters_files[[25]][[1]]  # Blue
b3_2289_2299 <- rasters_files[[26]][[1]]  # Green
b4_2289_2299 <- rasters_files[[27]][[1]]  # Red

# Extract the first layer of each band to create a single-date RGB image
red_2289_2299 <- b4_2289_2299[[1]]
green_2289_2299 <- b3_2289_2299[[1]]
blue_2289_2299 <- b2_2289_2299[[1]]

# Stack of the RGB bands
true_color_2289_2299 <- c(red_2289_2299, green_2289_2299, blue_2289_2299)

# Plot RGB
plotRGB(true_color_2289_2299, r = 1, g = 2, b = 3, scale=10000, stretch = "hist")

# Plot false color maxcell=inf is needed to force the use of all the pixels from the file 
plotRGB(true_color_2289_2299, r = 1, g = 2, b = 3, scale = 10000, stretch = "hist", maxcell = Inf)

# 6-----------------------------------------------------------------------------
rasters_true_color[[6]]
#source: 2023-06-25-00_00_2023-07-25-23_59_Sentinel-2_L2A_True_color.tiff 
names(rasters_files[[61]])
names(rasters_files[[62]])
names(rasters_files[[63]])

# loading rasters corresponding to the bands of true color
b2_23625_23725 <- rasters_files[[61]][[1]]  # Blue
b3_23625_23725 <- rasters_files[[62]][[1]]  # Green
b4_23625_23725 <- rasters_files[[63]][[1]]  # Red

# Extract the first layer of each band to create a single-date RGB image
red_23625_23725 <- b4_23625_23725[[1]]
green_23625_23725 <- b3_23625_23725[[1]]
blue_23625_23725 <- b2_23625_23725[[1]]

# Stack
true_color_23625_23725 <- c(red_23625_23725, green_23625_23725, blue_23625_23725)

# Plot RGB
plotRGB(true_color_23625_23725, r = 1, g = 2, b = 3, scale=10000, stretch = "hist")

dev.off()

#_______________________________________________________________________________

# === Visualization of the composite layers to give an idea of the area of study ===

# True Color Final
par(mfrow = c(2, 4), mar = c(1, 2, 3, 1), oma = c(0, 3, 3, 2))  # Margins and layout

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

#_______________________________________________________________________________

# Now it is time to understand the different impact of the two years
# We want to see the differences in the photosynthetic activity
# So it is needed, the classification of the true color map in a way that let us 
# have a preliminary search on the condition of the forests.

# Later we are going to analyse the values of the NDVI.

# Required libraries: imageRy

par(mfrow = c(1,2))
plotRGB(rasters_true_color[[1]], r = 1, g = 2, b = 3, stretch = "lin")
Bands_TC_051022_060922_class <- im.classify(rasters_true_color[[1]], num_clusters = 3)

Freq_TC_051022_060922_class <- freq(Bands_TC_051022_060922_class)
Freq_TC_051022_060922_class
Tot_TC_051022_060922_class <- ncell(Bands_TC_051022_060922_class)
P_TC_051022_060922_class = Freq_TC_051022_060922_class[3]*100/Tot_TC_051022_060922_class #the third column is the count for each pixel
P_TC_051022_060922_class
#1 41.68983 these are mountains' peak
#2 28.56901 these are denser forests
#3 29.74116 these are high altitude pastures and villages that still have a percentage of cement

par(mfrow = c(1,2))
plotRGB(rasters_true_color[[2]], r = 1, g = 2, b = 3, stretch = "lin")
Bands_TC_062522_072522_class <- im.classify(rasters_true_color[[2]], num_clusters = 3)

Freq_TC_062522_072522_class <- freq(Bands_TC_062522_072522_class)
Freq_TC_062522_072522_class
Tot_TC_062522_072522_class <- ncell(Bands_TC_062522_072522_class)
P_TC_062522_072522_class <- Freq_TC_062522_072522_class[3] * 100 / Tot_TC_062522_072522_class
P_TC_062522_072522_class
#1 24.29951 a bit of cloud coverage and stone
#2 41.14040 denser forests
#3 34.56009 mountain peaks


par(mfrow = c(1,2))
plotRGB(true_color_2289_2299, r = 1, g = 2, b = 3, scale = 10000, stretch = "hist", maxcell = Inf)
Bands_TC_080922_090922_class <- im.classify(true_color_2289_2299, num_clusters = 5)

Freq_TC_080922_090922_class <- freq(Bands_TC_080922_090922_class)
Freq_TC_080922_090922_class
Tot_TC_080922_090922_class <- ncell(Bands_TC_080922_090922_class)
P_TC_080922_090922_class <- Freq_TC_080922_090922_class[3] * 100 / Tot_TC_080922_090922_class
P_TC_080922_090922_class
#1 36.7770191 denser vegetation
#2  4.6405546 cloud coverage on the mountain top
#3  0.3673657 X
#4 23.1041075 stone and villages
#5 35.1109532 montain top

par(mfrow = c(1,2))
plotRGB(rasters_true_color[[4]], r = 1, g = 2, b = 3, stretch = "lin")
Bands_TC_092522_102522_class <- im.classify(rasters_true_color[[4]], num_clusters = 3)

Freq_TC_092522_102522_class <- freq(Bands_TC_092522_102522_class)
Freq_TC_092522_102522_class
Tot_TC_092522_102522_class <- ncell(Bands_TC_092522_102522_class)
P_TC_092522_102522_class <- Freq_TC_092522_102522_class[3] * 100 / Tot_TC_092522_102522_class
P_TC_092522_102522_class
#1 22.27191 high altitude snow
#2 38.59825 denser forests
#3 39.12984 stone and a bit of high altitude pastures

par(mfrow = c(1,2))
plotRGB(rasters_true_color[[5]], r = 1, g = 2, b = 3, stretch = "lin")
Bands_TC_051023_060923_class <- im.classify(rasters_true_color[[5]], num_clusters = 5)

Freq_TC_051023_060923_class <- freq(Bands_TC_051023_060923_class)
Freq_TC_051023_060923_class
Tot_TC_051023_060923_class <- ncell(Bands_TC_051023_060923_class)
P_TC_051023_060923_class <- Freq_TC_051023_060923_class[3] * 100 / Tot_TC_051023_060923_class
P_TC_051023_060923_class
#1 4.911768 it's the outer most margin
#2 14.960087 border of the forest and stone
#3 19.869116 denser vegetation
#4 43.439324 cloud coverage and mountain top
#5 16.819705 less denser vegetation

par(mfrow = c(1,2))
plotRGB(true_color_23625_23725, r = 1, g = 2, b = 3, scale = 10000, stretch = "hist", maxcell = Inf)
Bands_TC_062523_072523_class <- im.classify(true_color_23625_23725, num_clusters = 5)

Freq_TC_062523_072523_class <- freq(Bands_TC_062523_072523_class)
Freq_TC_062523_072523_class
Tot_TC_062523_072523_class <- ncell(Bands_TC_062523_072523_class)
P_TC_062523_072523_class <- Freq_TC_062523_072523_class[3] * 100 / Tot_TC_062523_072523_class
P_TC_062523_072523_class
#1 34.067210 villages and stone
#2  3.285251 bit of cloud coverage
#3 50.429948 denser vegetation
#4  9.132322 mountain top and glacier
#5  3.085269 mountain top and glacier

par(mfrow = c(1,2))
plotRGB(rasters_true_color[[7]], r = 1, g = 2, b = 3, stretch = "lin")
Bands_TC_080923_090923_class <- im.classify(rasters_true_color[[7]], num_clusters = 3)

Freq_TC_080923_090923_class <- freq(Bands_TC_080923_090923_class)
Freq_TC_080923_090923_class
Tot_TC_080923_090923_class <- ncell(Bands_TC_080923_090923_class)
P_TC_080923_090923_class <- Freq_TC_080923_090923_class[3] * 100 / Tot_TC_080923_090923_class
P_TC_080923_090923_class
#1 30.79466 denser vegetation
#2 40.64298 mountain tops and villages
#3 28.56236 glaciers and a bit more cloud coverage

par(mfrow = c(1,2))
plotRGB(rasters_true_color[[8]], r = 1, g = 2, b = 3, stretch = "lin")
Bands_TC_092523_102523_class <- im.classify(rasters_true_color[[8]], num_clusters = 3)

Freq_TC_092523_102523_class <- freq(Bands_TC_092523_102523_class)
Freq_TC_092523_102523_class
Tot_TC_092523_102523_class <- ncell(Bands_TC_092523_102523_class)
P_TC_092523_102523_class <- Freq_TC_092523_102523_class[3] * 100 / Tot_TC_092523_102523_class
P_TC_092523_102523_class
#1 31.53042 denser vegetation
#2 20.30043 glacier and rocks
#3 48.16915 stone and dry pastures

#--------------------------------------------------------------------------------

# Vizualization of the percentage
# Required libraries: ggplot2, patchwork and tidyr

class <- c("Dense vegetation","Baren terrain")

# Percentage of denser vegetation for each period and for the barren terrain
dens_veg <- c(  28.56901,
                41.14040, 
                36.77702,
                38.59825,
                35.68882,
                50.42995,
                30.79466,
                31.53042)

barren_terrain <- 100 - dens_veg

# Names for the different periods
periods <- c("Vegetative awakening",
             "Maximum activity",
             "Summer stress",
             "Early senescence")


# Creation of the dataframes
df23 <- data.frame(class,
                   "Vegetative awakening" = c(dens_veg[5], barren_terrain[5]),
                   "Maximum activity" = c(dens_veg[6], barren_terrain[6]),
                   "Summer stress" = c(dens_veg[7], barren_terrain[7]),
                   "Early senescence" = c(dens_veg[8], barren_terrain[8]))

df22 <- data.frame(class,
                   "Vegetative awakening" = c(dens_veg[1], barren_terrain[1]),
                   "Maximum activity" = c(dens_veg[2], barren_terrain[2]),
                   "Summer stress" = c(dens_veg[3], barren_terrain[3]),
                   "Early senescence" = c(dens_veg[4], barren_terrain[4]))

# Transformation of the dataframe into the long format to make it work on ggplot
df_long22 <- pivot_longer(df22,
                          cols = -class,
                          names_to = "periods",
                          values_to = "percentage")

df_long23 <- pivot_longer(df23,
                          cols = -class,
                          names_to = "periods",
                          values_to = "percentage")


g1 <- ggplot(df_long22, aes(x = periods, y = percentage, fill = class)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(option = "D") +
  scale_x_discrete(limits = c("Vegetative.awakening",
                              "Maximum.activity",
                              "Summer.stress",
                              "Early.senescence")) + ylim(c(0, 100)) +
  labs(title = "Vegetation classification 2022", x = "Periods", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7),
        plot.title = element_text(hjust = 0.5))
g1

g2 <- ggplot(df_long23, aes(x = periods, y = percentage, fill = class)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(option = "D") +
  scale_x_discrete(limits = c("Vegetative.awakening",
                              "Maximum.activity",
                              "Summer.stress",
                              "Early.senescence")) + ylim(c(0, 100)) +
  labs(title = "Vegetation classification 2023", x = "Periods", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7),
        plot.title = element_text(hjust = 0.5))
g2

# Unification of the two plots
g1 + g2

#_______________________________________________________________________________

# === NDVI and NDWI - Analysis Setup ===
# The following section prepares and processes Sentinel-2 imagery data to calculate 
# vegetation and water stress indices (NDVI and NDWI) across different seasonal periods 
#
# Corresponding bands to Red (B04), Near Infrared (B08), and SWIR (B11) bands 
# are filtered and loaded as raster layers and then they are grouped by time periods.
#
# NDVI is computed using Red and NIR bands, while NDWI is computed using NIR and SWIR bands.
# Some rasters needed to be reprojected to a common CRS to allow for correct mathematical 
# operations. Finally, difference maps are created to assess temporal variation in vegetation 
# and water stress between the two years, and results are visualized in a comparative layout.

# 05102*_06092* → from may 10th to ju e 9th
# 06252*_07252* → from 255h of june to 25th of july
# 08092*_09092* → from 9th of august to the 9th of september
# 09252*_10252* → from the 25th of September to the 25th of October

# Required libraries: raster and terra

print(tif_files)

tif_B04 <- tif_files[grepl("B04", tif_files)]
tif_B08 <- tif_files[grepl("B08", tif_files)]
tif_B11 <- tif_files[grepl("B11", tif_files)]

rasters_tif_B04 <- lapply(tif_B04, terra::rast)
rasters_tif_B08 <- lapply(tif_B08, terra::rast)
rasters_tif_B11 <- lapply(tif_B11, terra::rast)

# NDVI (Normalized Difference Vegetation Index):
#   - Red (Band 4) and NIR (Band 8)
#   - Formula: NDVI = (NIR - Red) / (NIR + Red)
#   - Used to assess vegetation health.
#   - NDVI already be present in the files, but it can be recalculated if needed.

# Red band (B04)
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

# NDVI = (NIR - Red) / (NIR + Red)

# From previous errors, some files needed to be reprojected on the CRS
# Mainly the NIR in degrees reprogected on the RED band in meters
nir_projected <- terra::project(nirband08_062522_072522, redband04_062522_072522)

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

par(mfrow = c(2, 4), mar = c(1, 2, 3, 1), oma = c(0, 3, 3, 2))

plot(NDVI_051022_060922[[1]], col = clv, main = "NDVI 05.10–06.09 2022")
plot(NDVI_062522_072522[[1]], col = clv, main = "NDVI 06.25–07.25 2022")
plot(NDVI_080922_090922[[1]], col = clv, main = "NDVI 08.09–09.09 2022")
plot(NDVI_092522_102522[[1]], col = clv, main = "NDVI 09.25–10.25 2022")

plot(NDVI_051023_060923[[1]], col = clv, main = "NDVI 05.10–06.09 2023")
plot(NDVI_062523_072523[[1]], col = clv, main = "NDVI 06.25–07.25 2023")
plot(NDVI_080923_090923[[1]], col = clv, main = "NDVI 08.09–09.09 2023")
plot(NDVI_092523_102523[[1]], col = clv, main = "NDVI 09.25–10.25 2023")

dev.off()

# NDWI (Normalized Difference Water Index):
#   - NIR (Band 8) and SWIR1 (Band 11)
#   - Formula: NDWI = (NIR - SWIR1) / (NIR + SWIR1)
#   - Used to distinguish between water bodies and stressed vegetation (e.g., drier areas).

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

par(mfrow = c(2, 4), mar = c(1, 2, 3, 1), oma = c(0, 3, 3, 2))

plot(NDWI_051022_060922[[1]], col = clv, main = "NDWI 05.10–06.09 2022")
plot(NDWI_062522_072522[[1]], col = clv, main = "NDWI 06.25–07.25 2022")
plot(NDWI_080922_090922[[1]], col = clv, main = "NDWI 08.09–09.09 2022")
plot(NDWI_092522_102522[[1]], col = clv, main = "NDWI 09.25–10.25 2022")

plot(NDWI_051023_060923[[1]], col = clv, main = "NDWI 05.10–06.09 2023")
plot(NDWI_062523_072523[[1]], col = clv, main = "NDWI 06.25–07.25 2023")
plot(NDWI_080923_090923[[1]], col = clv, main = "NDWI 08.09–09.09 2023")
plot(NDWI_092523_102523[[1]], col = clv, main = "NDWI 09.25–10.25 2023")

#_______________________________________________________________________________

#
# The results of NDVI and NDWI are used to analyze vegetation health, water stress, and drought effects.

# === NDVI ===
difNDVI_05 <- NDVI_051023_060923 - NDVI_051022_060922
difNDVI_06 <- NDVI_062523_072523 - NDVI_062522_072522
difNDVI_08 <- NDVI_080923_090923 - NDVI_080922_090922
difNDVI_09 <- NDVI_092523_102523 - NDVI_092522_102522

par(mfrow = c(2, 2))
plot(difNDVI_05[[1]], col = clv, main = "NDVI Diff: 05.10–06.09")
plot(difNDVI_06[[1]], col = clv, main = "NDVI Diff: 06.25–07.25")
plot(difNDVI_08[[1]], col = clv, main = "NDVI Diff: 08.09–09.09")
plot(difNDVI_09[[1]], col = clv, main = "NDVI Diff: 09.25–10.25")

dev.off()

# === NDWI ===

#----------------------------------------------------
# difNDWI_06 <- NDWI_062523_072523 - NDWI_062522_072522
# Error: [-] extents do not match

# Projection 2023 on CRS from 2022
NDWI_062523_072523_proj <- terra::project(NDWI_062523_072523, NDWI_062522_072522)
# Resample for the extent and resolution match
NDWI_062523_072523_resampled <- terra::resample(NDWI_062523_072523_proj, NDWI_062522_072522)
#----------------------------------------------------

difNDWI_05 <- NDWI_051023_060923 - NDWI_051022_060922
difNDWI_06 <- NDWI_062523_072523_resampled - NDWI_062522_072522
difNDWI_08 <- NDWI_080923_090923 - NDWI_080922_090922
difNDWI_09 <- NDWI_092523_102523 - NDWI_092522_102522

par(mfrow = c(2, 2))
plot(difNDWI_05[[1]], col = clv, main = "NDWI Diff: 05.10–06.09")
plot(difNDWI_06[[1]], col = clv, main = "NDWI Diff: 06.25–07.25")
plot(difNDWI_08[[1]], col = clv, main = "NDWI Diff: 08.09–09.09")
plot(difNDWI_09[[1]], col = clv, main = "NDWI Diff: 09.25–10.25")

dev.off()

#_______________________________________________________________________________

# NDVI and NDWI values range from +1.0 to -1.0. 

# Areas of barren rock, sand, or snow usually
# show very low NDVI values (for example, 0.1 or less). Sparse vegetation such as 
# shrubs and grasslands or senescing crops may result in moderate NDVI values 
# (approximately 0.2 to 0.5). High NDVI values (approximately 0.6 to 0.9) 
# correspond to dense vegetation such as that found in temperate and tropical 
# forests or crops at their peak growth stage. 

# Calculation of NDVI difference (ΔNDVI) between periods 
# to quantify changes in vegetation activity or health over time,
# comparing similar phenological periods across 2 different years.

# Interpretation of ΔNDVI values:
#   ≈ 0   --> Vegetation remains stable
#   < 0   --> Decrease in vegetation activity (possible stress or degradation)
#   > 0   --> Increase in vegetation activity (growth, recovery)
#   Values near -1 or -2 indicate drastic decline (e.g., total vegetation loss)
#   Values near +1 or +2 indicate strong increase (e.g., new growth or recovery)

val_difNDVI_05 <- values(difNDVI_05) |> 
  na.omit()
val_difNDVI_06 <- values(difNDVI_06) |> 
  na.omit()
val_difNDVI_08 <- values(difNDVI_08) |> 
  na.omit()
val_difNDVI_09 <- values(difNDVI_09) |> 
  na.omit()

summary(val_difNDVI_05)
summary(val_difNDVI_06)
summary(val_difNDVI_08)
summary(val_difNDVI_09)

####
# Function to categorize the NDVI differences based on the limits
categorize_ndvi_diff <- function(x) {
  cut(x, breaks = seq(-2, 2, length.out = 6), right = TRUE)
}
####

dh1 <- categorize_ndvi_diff(val_difNDVI_05)
dh2 <- categorize_ndvi_diff(val_difNDVI_06)
dh3 <- categorize_ndvi_diff(val_difNDVI_08)
dh4 <- categorize_ndvi_diff(val_difNDVI_09)

# Construction of the dataframe
df_dh1 <- as.data.frame(table(dh1))
colnames(df_dh1) <- c("NDVI_Interval", "Count")

df_dh2 <- as.data.frame(table(dh2))
colnames(df_dh2) <- c("NDVI_Interval", "Count")

df_dh3 <- as.data.frame(table(dh3))
colnames(df_dh3) <- c("NDVI_Interval", "Count")

df_dh4 <- as.data.frame(table(dh4))
colnames(df_dh4) <- c("NDVI_Interval", "Count")

# Order for vegetative interval
df_dh1$NDVI_Interval <- factor(df_dh1$NDVI_Interval, levels = levels(dh1), ordered = TRUE)
df_dh2$NDVI_Interval <- factor(df_dh2$NDVI_Interval, levels = levels(dh2), ordered = TRUE)
df_dh3$NDVI_Interval <- factor(df_dh3$NDVI_Interval, levels = levels(dh3), ordered = TRUE)
df_dh4$NDVI_Interval <- factor(df_dh4$NDVI_Interval, levels = levels(dh4), ordered = TRUE)

# Plotting the various NDVI differences (ΔNDVI)

h1 <- ggplot(df_dh1,  aes(x = NDVI_Interval, y = Count, fill = NDVI_Interval)) +
  geom_bar(stat = "identity",  position = "dodge") + 
  scale_fill_viridis_d(option = "D") + 
  geom_text(aes(label = Count), vjust = -0.3, size = 3) +
  labs(title = "NDVI differences: 05.10–06.09", x = "ΔNDVI interval", y = "Amount of pixels") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5))
h1


h2 <- ggplot(df_dh2, aes(x = NDVI_Interval, y = Count, fill = NDVI_Interval)) +
  geom_bar(stat = "identity",  position = "dodge") + 
  scale_fill_viridis_d(option = "D") + 
  geom_text(aes(label = Count), vjust = -0.3, size = 3) +
  labs(title = "NDVI differences: 06.25–07.25", x = "ΔNDVI interval", y = "Amount of pixels") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5))
h2

h3 <- ggplot(df_dh3, aes(x = NDVI_Interval, y = Count, fill = NDVI_Interval)) +
  geom_bar(stat = "identity",  position = "dodge") + 
  scale_fill_viridis_d(option = "D") + 
  geom_text(aes(label = Count), vjust = -0.3, size = 3) +
  labs(title = "NDVI differences: 08.09–09.09", x = "ΔNDVI interval", y = "Amount of pixels") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5))
h3


h4 <- ggplot(df_dh4, aes(x = NDVI_Interval, y = Count, fill = NDVI_Interval)) +
  geom_bar(stat = "identity",  position = "dodge") + 
  scale_fill_viridis_d(option = "D") + 
  geom_text(aes(label = Count), vjust = -0.3, size = 3) +
  labs(title = "NDVI differences: 09.25–10.25", x = "ΔNDVI interval", y = "Amount of pixels") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5))
h4


h1+h2+h3+h4

#_______________________________________________________________________________


# NDWI (Normalized Difference Water Index) is used to monitor changes in water content 
# of vegetation or detect the presence of surface water. It typically ranges between -1 and +1.

# Interpretation of NDWI values:
#   < 0   --> Typically indicates land surfaces such as soil, dry vegetation, or built-up areas
#   ≈ 0   --> Transitional zones; may correspond to moist soil or sparse vegetation
#   > 0   --> Indicates the presence of water or high moisture content in vegetation
#   Values closer to +1 suggest strong water signals (e.g., open water bodies)
#   Values closer to -1 indicate very dry areas or impervious surfaces

# Calculation of NDWI difference (ΔNDWI) between time periods 
# allows for the detection of hydrological changes, such as:
#   - Reduction in surface water (e.g., due to drought or drainage)
#   - Increase in moisture (e.g., after rainfall or irrigation)
#   - Monitoring vegetation water stress or wetland dynamics

# Interpretation of ΔNDWI values:
#   ≈ 0   --> No significant change in moisture or water presence
#   < 0   --> Decrease in water content (drying trend)
#   > 0   --> Increase in water content (wetting trend)
#   Values near -1 indicate strong water loss (e.g., wetland drying)
#   Values near +1 indicate strong water gain (e.g., flooding or recharge)

val_difNDWI_05 <- values(difNDWI_05) |> 
  na.omit()
val_difNDWI_06 <- values(difNDWI_06) |> 
  na.omit()
val_difNDWI_08 <- values(difNDWI_08) |> 
  na.omit()
val_difNDWI_09 <- values(difNDWI_09) |> 
  na.omit()

summary(val_difNDWI_05)
summary(val_difNDWI_06)
summary(val_difNDWI_08)
summary(val_difNDWI_09)

dhw1 <- categorize_ndvi_diff(val_difNDWI_05)
dhw2 <- categorize_ndvi_diff(val_difNDWI_06)
dhw3 <- categorize_ndvi_diff(val_difNDWI_08)
dhw4 <- categorize_ndvi_diff(val_difNDWI_09)

# Construction of the dataframe
df_dhw1 <- as.data.frame(table(dhw1))
colnames(df_dhw1) <- c("NDWI_Interval", "Count")

df_dhw2 <- as.data.frame(table(dhw2))
colnames(df_dhw2) <- c("NDWI_Interval", "Count")

df_dhw3 <- as.data.frame(table(dhw3))
colnames(df_dhw3) <- c("NDWI_Interval", "Count")

df_dhw4 <- as.data.frame(table(dhw4))
colnames(df_dhw4) <- c("NDWI_Interval", "Count")

# Order for vegetative interval
df_dhw1$NDWI_Interval <- factor(df_dhw1$NDWI_Interval, levels = levels(dhw1), ordered = TRUE)
df_dhw2$NDWI_Interval <- factor(df_dhw2$NDWI_Interval, levels = levels(dhw2), ordered = TRUE)
df_dhw3$NDWI_Interval <- factor(df_dhw3$NDWI_Interval, levels = levels(dhw3), ordered = TRUE)
df_dhw4$NDWI_Interval <- factor(df_dhw4$NDWI_Interval, levels = levels(dhw4), ordered = TRUE)

# Plotting the various NDVI differences (ΔNDVI)
hw1 <- ggplot(df_dhw1, aes(x = NDWI_Interval, y = Count, fill = NDWI_Interval)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_viridis_d(option = "D") + 
  geom_text(aes(label = Count), vjust = -0.3, size = 3) +
  labs(title = "NDWI differences: 05.10–06.09", x = "ΔNDWI interval", y = "Amount of pixels") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5))

hw2 <- ggplot(df_dhw2, aes(x = NDWI_Interval, y = Count, fill = NDWI_Interval)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_viridis_d(option = "D") + 
  geom_text(aes(label = Count), vjust = -0.3, size = 3) +
  labs(title = "NDWI differences: 06.25–07.25", x = "ΔNDWI interval", y = "Amount of pixels") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5))

hw3 <- ggplot(df_dhw3, aes(x = NDWI_Interval, y = Count, fill = NDWI_Interval)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_viridis_d(option = "D") + 
  geom_text(aes(label = Count), vjust = -0.3, size = 3) +
  labs(title = "NDWI differences: 08.09–09.09", x = "ΔNDWI interval", y = "Amount of pixels") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5))

hw4 <- ggplot(df_dhw4, aes(x = NDWI_Interval, y = Count, fill = NDWI_Interval)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_viridis_d(option = "D") + 
  geom_text(aes(label = Count), vjust = -0.3, size = 3) +
  labs(title = "NDWI differences: 09.25–10.25", x = "ΔNDWI interval", y = "Amount of pixels") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5))

hw1 + hw2 + hw3 + hw4

#_______________________________________________________________________________

# Values of the single years for the statistical analisis 

val_NDVI_051022_060922 <- values(NDVI_051022_060922) |> 
  na.omit()
val_NDVI_062522_072522 <- values(NDVI_062522_072522) |> 
  na.omit()
val_NDVI_080922_090922 <- values(NDVI_080922_090922) |> 
  na.omit()
val_NDVI_092522_102522 <- values(NDVI_092522_102522) |>   
  na.omit()

val_NDVI_051023_060923 <- values(NDVI_051023_060923) |> 
  na.omit()
val_NDVI_062523_072523 <- values(NDVI_062523_072523) |> 
  na.omit()
val_NDVI_080923_090923 <- values(NDVI_080923_090923) |> 
  na.omit()
val_NDVI_092523_102523 <- values(NDVI_092523_102523) |>   
  na.omit()


# Using the Wilcoxon rank-sum test (Mann-Whitney) with paired = FALSE 
# because the two NDVI images being compared come from the same period 
# in different years but may not have perfectly matching pixel positions 
# or identical sample sizes. This non-parametric test does not assume 
# normality and is robust to non-Gaussian distributions and outliers, 
# making it suitable for comparing overall differences in NDVI 
# distributions between the two periods.

wilcox.test(val_NDVI_051022_060922, val_NDVI_051023_060923, paired = FALSE)
#Wilcoxon rank sum test with continuity correction
#data:  val_NDVI_051022_060922 and val_NDVI_051023_060923
#W = 1.6496e+13, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(val_NDVI_062522_072522, val_NDVI_062523_072523, paired = FALSE)
#Wilcoxon rank sum test with continuity correction
#data:  val_NDVI_062522_072522 and val_NDVI_062523_072523
#W = 1.703e+13, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(val_NDVI_080922_090922, val_NDVI_080923_090923, paired = FALSE)
#Wilcoxon rank sum test with continuity correction
#data:  val_NDVI_080922_090922 and val_NDVI_080923_090923
#W = 1.6302e+13, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(val_NDVI_092522_102522, val_NDVI_092523_102523, paired = FALSE)
#Wilcoxon rank sum test with continuity correction
#data:  val_NDVI_092522_102522 and val_NDVI_092523_102523
#W = 1.4717e+13, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0


# All Wilcoxon rank-sum tests indicate statistically significant differences 
# in the distribution of NDVI values between 2022 and 2023 for each period analyzed. 
# This suggests that vegetation activity varied between the two years, 
# likely due to climatic conditions or environmental disturbances.
# 
# The Wilcoxon test does not indicate the direction of the change, 
# so to better understand the magnitude and trend of NDVI differences, 


# Extraction of the single values foe each year for NDWI

val_NDWI_051022_060922 <- values(NDWI_051022_060922) |> 
  na.omit()
val_NDWI_062522_072522 <- values(NDWI_062522_072522) |> 
  na.omit()
val_NDWI_080922_090922 <- values(NDWI_080922_090922) |> 
  na.omit()
val_NDWI_092522_102522 <- values(NDWI_092522_102522) |>   
  na.omit()

val_NDWI_051023_060923 <- values(NDWI_051023_060923) |> 
  na.omit()
val_NDWI_062523_072523_resampled <- values(NDWI_062523_072523_resampled) |> 
  na.omit()
val_NDWI_080923_090923 <- values(NDWI_080923_090923) |> 
  na.omit()
val_NDWI_092523_102523 <- values(NDWI_092523_102523) |>   
  na.omit()


# Wilcoxon rank-sum test to compare NDWI values between years.
# paired = FALSE because pixel positions may not correspond exactly
# (i.e., we are comparing independent samples, not matched observations).


wilcox.test(val_NDWI_051022_060922, val_NDWI_051023_060923, paired = FALSE)
#Wilcoxon rank sum test with continuity correction
#data:  val_NDWI_051022_060922 and val_NDWI_051023_060923
#W = 1.5385e+13, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(val_NDWI_062522_072522, val_NDWI_062523_072523_resampled, paired = FALSE)
#Wilcoxon rank sum test with continuity correction
#data:  val_NDWI_062522_072522 and val_NDWI_062523_072523_resampled
#W = 1.4901e+13, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(val_NDWI_080922_090922, val_NDWI_080923_090923, paired = FALSE)
#Wilcoxon rank sum test with continuity correction
#data:  val_NDWI_080922_090922 and val_NDWI_080923_090923
#W = 1.58e+13, p-value = 1.184e-05
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(val_NDWI_092522_102522, val_NDWI_092523_102523, paired = FALSE)
#Wilcoxon rank sum test with continuity correction
#data:  val_NDWI_092522_102522 and val_NDWI_092523_102523
#W = 1.8872e+13, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0


# Summary of Wilcoxon rank-sum test results (NDVI and NDWI comparisons between 2022 and 2023):
#
# All NDVI comparisons between the same seaso# Spatial Ecology Project: NDVI & Drought Impact - Province of Sondrio
# Author: Tommaso Magarotto

# Goal: Analyze the impact of drought on vegetation using NDVI (Sentinel-2) 
# and climate data, aggregated by season

# The time period we will focus on is from 2022 to 2024
# During this period, Italy experienced several extreme events, including droughts and floods. 
# The goal is to assess the impact of these extreme events on mountainous ecosystems.

#_______________________________________________________________________________

# Uncomment and run the following line if you haven't installed the packages yet:
# install.packages(c("sf", "geodata", "viridis", "imageRy", "tidyr", "ggplot2", "patchwork", "raster", "terra"))


library(sf)         # For handling spatial vector data (e.g., polygons, shapefiles)
library(geodata)    # For downloading administrative boundaries (e.g., GADM dataset)
library(viridis)    # Color palettes optimized for visibility and accessibility
library(imageRy)    # Used for working with raster/image data, in this case for working with the classification of true color images
library(tidyr)      # For data wrangling and reshaping
library(ggplot2)    # For creating elegant data visualizations
library(patchwork)  # For combining multiple ggplot2 plots
library(raster)     # For working with raster data (e.g., satellite images)
library(terra)      # Newer alternative to 'raster' for raster and vector data

clv <- viridis(100)  # Create a continuous viridis color palette with 100 values

#_______________________________________________________________________________

# === Download the administrative boundaries ===
# Required libraries: sf and geodata

# Download the administrative boundaries of Italy - level 3 (i.e., municipalities)
# 'level = 3' gives you the most detailed administrative unit (municipalities)
# 'path = tempdir()' stores the data in a temporary directory
italy_admin3 <- gadm(country = "ITA", level = 3, path = tempdir())

#I dati scaricati non sono direttamente in formato sf (spatial feature)
# Downloaded data are not directly ready for spatial analysis so
# Convert the downloaded object to a simple feature (sf) object
italy_admin3 <- st_as_sf(italy_admin3)

# Define the municipalities of interest (in the Province of Sondrio)
# These are selected because they are key mountain municipalities
target_municipalities <- c("Livigno", "Valdidentro", "Valdisotto", "Bormio",
                           "Valfurva", "Grosio", "Sondalo", "Grosotto")

# Filter the dataset to keep only the selected municipalities
# 'NAME_3' is the GADM field for municipality names
selected_municipalities <- italy_admin3[italy_admin3$NAME_3 %in% target_municipalities, ]

# Ensure that geometries are valid (sometimes features may have topology issues)
# This is a good practice before doing spatial operations like bounding boxes or overlays
selected_municipalities <- st_make_valid(selected_municipalities)

# Create a bounding box that fully contains the selected municipalities
# st_bbox() generates a rectangle (bbox) that minimally wraps all selected areas 
bbox <- st_bbox(selected_municipalities)

# st_as_sfc() converts the bounding box to a spatial feature (polygon)
bbox_polygon <- st_as_sfc(bbox)

# Wrap the geometry into an sf object and assign it the same CRS (coordinate system) 
bbox_polygon <- st_sf(geometry = bbox_polygon, crs = st_crs(selected_municipalities))

# Plot the bounding box (in red) and the selected municipalities (in transparent blue)
# This helps visualize how the municipalities are spatially distributed inside the bbox
# st_geometry() gives back the polygon  without the extra columns, useful for drawing and analyzing  spatial components
plot(st_geometry(bbox_polygon), border = "red", lwd = 2, 
     main = "Bounding Box + Selected Municipalities (GADM)")
plot(st_geometry(selected_municipalities), add = TRUE, 
     col = rgb(0, 0, 1, 0.3), border = "blue")

# Export or print the bounding box in WKT (Well-Known Text) format
# Useful for logging, sharing geometry in text form, or using in GIS software
st_as_text(st_geometry(bbox_polygon))

# "POLYGON ((10.03806 46.26057, 10.63152 46.26057, 10.63152 46.63806, 10.03806 46.63806, 10.03806 46.26057))"

#_______________________________________________________________________________

#  Image format:
# Summary of the downloaded images:
# TIFF (32-bit float)
# layer: True Color, NDVI, False Color (urban)
# single bands B02, B03, B04, B05, B06 B08, B08A B11, B12
# Image resolution: HIGH 2500 x 2308 px
# Coordinate system: UTM 32N (EPSG:32632)
# Projected resolution: 18 m/px
# Data mask

# Plant seasonal phases considerations
################################################################################

# Plant seasonal phases:
# Avoiding the dormancy period because NDVI don't see the photosynthetic activity since it's too low or even absent

# Vegetative awakening (Spring): May 10 – June 9
# During this phase, the first leaves appear, with flowering and the beginning of growth. 
# The NDVI (Normalized Difference Vegetation Index) should show low intensity but followed to a rapid growth.
#
# Maximum activity (Early Summer): June 25 – July 24
# This is the period of maximum photosynthesis, with full leaf coverage. NDVI reaches its peak.
#
# Summer stress (Late Summer): August 9 – September 9
# During this phase, plants may experience water or heat stress. NDVI stabilizes or shows a slight decrease.
#
# Early senescence (Pre-Autumn): September 25 – October 25
# Leaves begin to yellow, and photosynthetic activity decreases. NDVI shows a noticeable drop.
#
# The selected time intervals for each phase correspond to approximately 30 days, representing the critical periods of 
# plant growth, stress, and senescence. These will be used to analyze the impact of drought on vegetation over the years. 
# This approach helps monitor the seasonal evolution of NDVI and identify anomalies in Italy between 2022 and 2023.

################################################################################

#_______________________________________________________________________________

# === Load the images in Tiff on R and visualization ===
# Required libraries: raster and terra

setwd("C:/Users/Tommy/Documents/altavaltellian")
getwd()
#"C:/Users/Tommy/Documents/altavaltellian"

list.files()

# List of all the files in the chosen dirrectory
tif_files <- list.files(pattern = "\\.tiff$", full.names = TRUE)

# Grouping the file based on the layer that we want to analayse
# Searches for a specific string of text inside of the vector and gives back a logical vector that gives which elements arecontined in that string
tif_true_color <- tif_files[grepl("True_color", tif_files)]
tif_NDVI <- tif_files[grepl("NDVI", tif_files)]
tif_false_color <- tif_files[grepl("False_color", tif_files)]

# Creation of the raster applying terra::rast to all the files
# lapply(..., terra::rast): applies the function terra::rast() to every element
rasters_true_color <- lapply(tif_true_color, terra::rast)
rasters_NDVI <- lapply(tif_NDVI, terra::rast)
rasters_false_color <- lapply(tif_false_color, terra::rast)

# Example to see if the plot goes well
# Linear stretching of the pixel values for a better plotting. It is a visual normalization. 
rasters_true_color[[1]]
plotRGB(rasters_true_color[[1]], r = 1, g = 2, b = 3, stretch = "lin")

# After checking the available .tif files and attempting to plot them,
# it became clear that some images are corrupted or incomplete.
# Despite multiple download attempts, some files could not be retrieved correctly.
# Therefore, we need to reconstruct the missing composite layers manually.
# This step is essential in order to later build the complete set of final images.
rasters_files <- lapply(tif_files, terra::rast)

# 3-----------------------------------------------------------------------------
rasters_false_color[[3]] # Gives us information on the raster
#source: 2022-08-09-00_00_2022-09-09-23_59_Sentinel-2_L2A_False_color.tiff

names(rasters_files[[26]])
names(rasters_files[[27]])
names(rasters_files[[30]])

b4_2289_2299 <- rasters_files[[26]][[1]] # red
b3_2289_2299 <- rasters_files[[27]][[1]] # green
b8_2289_2299 <- rasters_files[[30]][[1]] # (NIR) (8)

# Extract the first layer of each band to create a single-date RGB image
nir_2289_2299 <- b8_2289_2299[[1]]
red_2289_2299 <- b4_2289_2299[[1]]
green_2289_2299 <- b3_2289_2299[[1]]

# Stack of the bands
false_color_2289_2299 <- c(nir_2289_2299, red_2289_2299, green_2289_2299)

# Plot false color maxcell=inf is needed to force the use of all the pixels from the file 
plotRGB(false_color_2289_2299, r = 1, g = 2, b = 3, scale = 10000, stretch = "lin", maxcell = Inf)

####
# The image appears with an unusual blue tint instead of the expected white.
# This is likely due to differences in how reflectance values are handled in R
# compared to how they are visualized in the original Sentinel-2 platform.
####

# 3-----------------------------------------------------------------------------
rasters_true_color[[3]]
# source: 2022-08-09-00_00_2022-09-09-23_59_Sentinel-2_L2A_True_color.tiff
names(rasters_files[[25]])
names(rasters_files[[26]])
names(rasters_files[[27]])

# Loading rasters corresponding to the bands of true color
b2_2289_2299 <- rasters_files[[25]][[1]]  # Blue
b3_2289_2299 <- rasters_files[[26]][[1]]  # Green
b4_2289_2299 <- rasters_files[[27]][[1]]  # Red

# Extract the first layer of each band to create a single-date RGB image
red_2289_2299 <- b4_2289_2299[[1]]
green_2289_2299 <- b3_2289_2299[[1]]
blue_2289_2299 <- b2_2289_2299[[1]]

# Stack of the RGB bands
true_color_2289_2299 <- c(red_2289_2299, green_2289_2299, blue_2289_2299)

# Plot RGB
plotRGB(true_color_2289_2299, r = 1, g = 2, b = 3, scale=10000, stretch = "hist")

# Plot false color maxcell=inf is needed to force the use of all the pixels from the file 
plotRGB(true_color_2289_2299, r = 1, g = 2, b = 3, scale = 10000, stretch = "hist", maxcell = Inf)

# 6-----------------------------------------------------------------------------
rasters_true_color[[6]]
# source: 2023-06-25-00_00_2023-07-25-23_59_Sentinel-2_L2A_True_color.tiff 
names(rasters_files[[61]])
names(rasters_files[[62]])
names(rasters_files[[63]])

# Loading rasters corresponding to the bands of true color
b2_23625_23725 <- rasters_files[[61]][[1]]  # Blue
b3_23625_23725 <- rasters_files[[62]][[1]]  # Green
b4_23625_23725 <- rasters_files[[63]][[1]]  # Red

# Extract the first layer of each band to create a single-date RGB image
red_23625_23725 <- b4_23625_23725[[1]]
green_23625_23725 <- b3_23625_23725[[1]]
blue_23625_23725 <- b2_23625_23725[[1]]

# Stack
true_color_23625_23725 <- c(red_23625_23725, green_23625_23725, blue_23625_23725)

# Plot RGB
plotRGB(true_color_23625_23725, r = 1, g = 2, b = 3, scale=10000, stretch = "hist")

dev.off()

#_______________________________________________________________________________

# === Visualization of the composite layers to give an idea of the area of study ===

# True Color Final
par(mfrow = c(2, 4), mar = c(1, 2, 3, 1), oma = c(0, 3, 3, 2))  # Margins and layout

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

# False color final

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

# NDVI already composite final 

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

#_______________________________________________________________________________

# Now it is time to understand the different impact of the two years
# We want to see the differences in the photosynthetic activity
# So it is needed, the classification of the true color map in a way that lets us 
# have a preliminary search on the condition of the forests.

# Required libraries: imageRy

par(mfrow = c(1,2))
plotRGB(rasters_true_color[[1]], r = 1, g = 2, b = 3, stretch = "lin")
Bands_TC_051022_060922_class <- im.classify(rasters_true_color[[1]], num_clusters = 3)

Freq_TC_051022_060922_class <- freq(Bands_TC_051022_060922_class)
Freq_TC_051022_060922_class
Tot_TC_051022_060922_class <- ncell(Bands_TC_051022_060922_class)
P_TC_051022_060922_class = Freq_TC_051022_060922_class[3]*100/Tot_TC_051022_060922_class #the third column is the count for each pixel
P_TC_051022_060922_class
#1 41.68983 These are mountains' peak
#2 28.56901 These are denser forests
#3 29.74116 These are high-altitude pastures and villages that still have a percentage of cement

par(mfrow = c(1,2))
plotRGB(rasters_true_color[[2]], r = 1, g = 2, b = 3, stretch = "lin")
Bands_TC_062522_072522_class <- im.classify(rasters_true_color[[2]], num_clusters = 3)

Freq_TC_062522_072522_class <- freq(Bands_TC_062522_072522_class)
Freq_TC_062522_072522_class
Tot_TC_062522_072522_class <- ncell(Bands_TC_062522_072522_class)
P_TC_062522_072522_class <- Freq_TC_062522_072522_class[3] * 100 / Tot_TC_062522_072522_class
P_TC_062522_072522_class
#1 24.29951 a bit of cloud coverage and stone
#2 41.14040 denser forests
#3 34.56009 mountain peaks


par(mfrow = c(1,2))
plotRGB(true_color_2289_2299, r = 1, g = 2, b = 3, scale = 10000, stretch = "hist", maxcell = Inf)
Bands_TC_080922_090922_class <- im.classify(true_color_2289_2299, num_clusters = 5)

Freq_TC_080922_090922_class <- freq(Bands_TC_080922_090922_class)
Freq_TC_080922_090922_class
Tot_TC_080922_090922_class <- ncell(Bands_TC_080922_090922_class)
P_TC_080922_090922_class <- Freq_TC_080922_090922_class[3] * 100 / Tot_TC_080922_090922_class
P_TC_080922_090922_class
#1 36.7770191 denser vegetation
#2  4.6405546 cloud coverage on the mountain top
#3  0.3673657 X
#4 23.1041075 stone and villages
#5 35.1109532 mountain top

par(mfrow = c(1,2))
plotRGB(rasters_true_color[[4]], r = 1, g = 2, b = 3, stretch = "lin")
Bands_TC_092522_102522_class <- im.classify(rasters_true_color[[4]], num_clusters = 3)

Freq_TC_092522_102522_class <- freq(Bands_TC_092522_102522_class)
Freq_TC_092522_102522_class
Tot_TC_092522_102522_class <- ncell(Bands_TC_092522_102522_class)
P_TC_092522_102522_class <- Freq_TC_092522_102522_class[3] * 100 / Tot_TC_092522_102522_class
P_TC_092522_102522_class
#1 22.27191 high altitude snow
#2 38.59825 denser forests
#3 39.12984 stone and a bit of high altitude pastures

par(mfrow = c(1,2))
plotRGB(rasters_true_color[[5]], r = 1, g = 2, b = 3, stretch = "lin")
Bands_TC_051023_060923_class <- im.classify(rasters_true_color[[5]], num_clusters = 5)

Freq_TC_051023_060923_class <- freq(Bands_TC_051023_060923_class)
Freq_TC_051023_060923_class
Tot_TC_051023_060923_class <- ncell(Bands_TC_051023_060923_class)
P_TC_051023_060923_class <- Freq_TC_051023_060923_class[3] * 100 / Tot_TC_051023_060923_class
P_TC_051023_060923_class
#1 4.911768 it's the outer most margin
#2 14.960087 border of the forest and stone
#3 19.869116 denser vegetation
#4 43.439324 cloud coverage and mountain top
#5 16.819705 less dense vegetation

par(mfrow = c(1,2))
plotRGB(true_color_23625_23725, r = 1, g = 2, b = 3, scale = 10000, stretch = "hist", maxcell = Inf)
Bands_TC_062523_072523_class <- im.classify(true_color_23625_23725, num_clusters = 5)

Freq_TC_062523_072523_class <- freq(Bands_TC_062523_072523_class)
Freq_TC_062523_072523_class
Tot_TC_062523_072523_class <- ncell(Bands_TC_062523_072523_class)
P_TC_062523_072523_class <- Freq_TC_062523_072523_class[3] * 100 / Tot_TC_062523_072523_class
P_TC_062523_072523_class
#1 34.067210 villages and stone
#2  3.285251 bit of cloud coverage
#3 50.429948 denser vegetation
#4  9.132322 mountain top and glacier
#5  3.085269 mountain top and glacier

par(mfrow = c(1,2))
plotRGB(rasters_true_color[[7]], r = 1, g = 2, b = 3, stretch = "lin")
Bands_TC_080923_090923_class <- im.classify(rasters_true_color[[7]], num_clusters = 3)

Freq_TC_080923_090923_class <- freq(Bands_TC_080923_090923_class)
Freq_TC_080923_090923_class
Tot_TC_080923_090923_class <- ncell(Bands_TC_080923_090923_class)
P_TC_080923_090923_class <- Freq_TC_080923_090923_class[3] * 100 / Tot_TC_080923_090923_class
P_TC_080923_090923_class
#1 30.79466 denser vegetation
#2 40.64298 mountain tops and villages
#3 28.56236 glaciers and a bit more cloud coverage

par(mfrow = c(1,2))
plotRGB(rasters_true_color[[8]], r = 1, g = 2, b = 3, stretch = "lin")
Bands_TC_092523_102523_class <- im.classify(rasters_true_color[[8]], num_clusters = 3)

Freq_TC_092523_102523_class <- freq(Bands_TC_092523_102523_class)
Freq_TC_092523_102523_class
Tot_TC_092523_102523_class <- ncell(Bands_TC_092523_102523_class)
P_TC_092523_102523_class <- Freq_TC_092523_102523_class[3] * 100 / Tot_TC_092523_102523_class
P_TC_092523_102523_class
#1 31.53042 denser vegetation
#2 20.30043 glacier and rocks
#3 48.16915 stone and dry pastures

#--------------------------------------------------------------------------------

# Visualization of the percentage
# Required libraries: ggplot2, patchwork and tidyr

class <- c("Dense vegetation","Baren terrain")

# Percentage of denser vegetation for each period and for the barren terrain
dens_veg <- c(28.56901, 
              41.14040, 
              36.77702, 
              38.59825, 
              35.68882, 
              50.42995, 
              30.79466, 
              31.53042)

barren_terrain <- 100 - dens_veg

# Names for the different periods
periods <- c("Vegetative awakening",
             "Maximum activity",
             "Summer stress",
             "Early senescence")

# Creation of the dataframes
df23 <- data.frame(class,
                   "Vegetative awakening" = c(dens_veg[5], barren_terrain[5]),
                   "Maximum activity" = c(dens_veg[6], barren_terrain[6]),
                   "Summer stress" = c(dens_veg[7], barren_terrain[7]),
                   "Early senescence" = c(dens_veg[8], barren_terrain[8]))

df22 <- data.frame(class,
                   "Vegetative awakening" = c(dens_veg[1], barren_terrain[1]),
                   "Maximum activity" = c(dens_veg[2], barren_terrain[2]),
                   "Summer stress" = c(dens_veg[3], barren_terrain[3]),
                   "Early senescence" = c(dens_veg[4], barren_terrain[4]))

# Transformation of the dataframe into the long format to make it work on ggplot
df_long22 <- pivot_longer(df22,
                          cols = -class,
                          names_to = "periods",
                          values_to = "percentage")

df_long23 <- pivot_longer(df23,
                          cols = -class,
                          names_to = "periods",
                          values_to = "percentage")


g1 <- ggplot(df_long22, aes(x = periods, y = percentage, fill = class)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(option = "D") +
  scale_x_discrete(limits = c("Vegetative.awakening",
                              "Maximum.activity",
                              "Summer.stress",
                              "Early.senescence")) + ylim(c(0, 100)) +
  labs(title = "Vegetation classification 2022", x = "Periods", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7),
        plot.title = element_text(hjust = 0.5))
g1

g2 <- ggplot(df_long23, aes(x = periods, y = percentage, fill = class)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(option = "D") +
  scale_x_discrete(limits = c("Vegetative.awakening",
                              "Maximum.activity",
                              "Summer.stress",
                              "Early.senescence")) + ylim(c(0, 100)) +
  labs(title = "Vegetation classification 2023", x = "Periods", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7),
        plot.title = element_text(hjust = 0.5))
g2

# Unification of the two plots
g1 + g2

#_______________________________________________________________________________

# === NDVI and NDWI - Analysis Setup ===
# The following section prepares and processes Sentinel-2 imagery data to calculate 
# vegetation and water stress indices (NDVI and NDWI) across different seasonal periods 
#
# Corresponding bands to Red (B04), Near Infrared (B08), and SWIR (B11) bands 
# are filtered and loaded as raster layers, and then grouped by periods.
#
# NDVI is computed using Red and NIR bands, while NDWI is computed using NIR and SWIR bands.
# Some rasters must be reprojected to a common CRS to allow for correct mathematical 
# operations. Finally, difference maps are created to assess temporal variation in vegetation 
# and water stress between the two years, and results are visualized in a comparative layout.

# 05102*_06092* → from may 10th to ju e 9th
# 06252*_07252* → from 255h of june to 25th of july
# 08092*_09092* → from 9th of august to the 9th of september
# 09252*_10252* → from the 25th of September to the 25th of October

# Required libraries: raster and terra

print(tif_files)

tif_B04 <- tif_files[grepl("B04", tif_files)]
tif_B08 <- tif_files[grepl("B08", tif_files)]
tif_B11 <- tif_files[grepl("B11", tif_files)]

rasters_tif_B04 <- lapply(tif_B04, terra::rast)
rasters_tif_B08 <- lapply(tif_B08, terra::rast)
rasters_tif_B11 <- lapply(tif_B11, terra::rast)

# NDVI (Normalized Difference Vegetation Index):
#   - Red (Band 4) and NIR (Band 8)
#   - Formula: NDVI = (NIR - Red) / (NIR + Red)
#   - Used to assess vegetation health.
#   - NDVI already be present in the files, but it can be recalculated if needed.

# Red band (B04)
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

# NDVI = (NIR - Red) / (NIR + Red)

# From previous errors, some files needed to be reprojected on the CRS
# Mainly the NIR in degrees reprojected on the RED band in meters
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

par(mfrow = c(2, 4), mar = c(1, 2, 3, 1), oma = c(0, 3, 3, 2))

plot(NDVI_051022_060922[[1]], col = clv, main = "NDVI 05.10–06.09 2022")
plot(NDVI_062522_072522[[1]], col = clv, main = "NDVI 06.25–07.25 2022")
plot(NDVI_080922_090922[[1]], col = clv, main = "NDVI 08.09–09.09 2022")
plot(NDVI_092522_102522[[1]], col = clv, main = "NDVI 09.25–10.25 2022")

plot(NDVI_051023_060923[[1]], col = clv, main = "NDVI 05.10–06.09 2023")
plot(NDVI_062523_072523[[1]], col = clv, main = "NDVI 06.25–07.25 2023")
plot(NDVI_080923_090923[[1]], col = clv, main = "NDVI 08.09–09.09 2023")
plot(NDVI_092523_102523[[1]], col = clv, main = "NDVI 09.25–10.25 2023")

dev.off()

# NDWI (Normalized Difference Water Index):
#   - NIR (Band 8) and SWIR1 (Band 11)
#   - Formula: NDWI = (NIR - SWIR1) / (NIR + SWIR1)
#   - Used to distinguish between water bodies and stressed vegetation (e.g., drier areas).

# NDWI 2022

# From previous errors, some files needed to be reprojected on the CRS
nir_projected <- terra::project(nirband08_062522_072522, redband04_062522_072522)

NDWI_051022_060922 <- (nirband08_051022_060922 - swirband11_051022_060922) / (nirband08_051022_060922 + swirband11_051022_060922)
NDWI_062522_072522 <- (nirband08_062522_072522 - swirband11_062522_072522) / (nirband08_062522_072522 + swirband11_062522_072522)
NDWI_080922_090922 <- (nirband08_080922_090922 - swirband11_080922_090922) / (nirband08_080922_090922 + swirband11_080922_090922)
NDWI_092522_102522 <- (nirband08_092522_102522 - swirband11_092522_102522) / (nirband08_092522_102522 + swirband11_092522_102522)

# NDWI 2023
NDWI_051023_060923 <- (nirband08_051023_060923 - swirband11_051023_060923) / (nirband08_051023_060923 + swirband11_051023_060923)
NDWI_062523_072523 <- (nirband08_062523_072523 - swirband11_062523_072523) / (nirband08_062523_072523 + swirband11_062523_072523)
NDWI_080923_090923 <- (nirband08_080923_090923 - swirband11_080923_090923) / (nirband08_080923_090923 + swirband11_080923_090923)
NDWI_092523_102523 <- (nirband08_092523_102523 - swirband11_092523_102523) / (nirband08_092523_102523 + swirband11_092523_102523)

par(mfrow = c(2, 4), mar = c(1, 2, 3, 1), oma = c(0, 3, 3, 2))

plot(NDWI_051022_060922[[1]], col = clv, main = "NDWI 05.10–06.09 2022")
plot(NDWI_062522_072522[[1]], col = clv, main = "NDWI 06.25–07.25 2022")
plot(NDWI_080922_090922[[1]], col = clv, main = "NDWI 08.09–09.09 2022")
plot(NDWI_092522_102522[[1]], col = clv, main = "NDWI 09.25–10.25 2022")

plot(NDWI_051023_060923[[1]], col = clv, main = "NDWI 05.10–06.09 2023")
plot(NDWI_062523_072523[[1]], col = clv, main = "NDWI 06.25–07.25 2023")
plot(NDWI_080923_090923[[1]], col = clv, main = "NDWI 08.09–09.09 2023")
plot(NDWI_092523_102523[[1]], col = clv, main = "NDWI 09.25–10.25 2023")

#_______________________________________________________________________________

#
# The results of NDVI and NDWI are used to analyze vegetation health, water stress, and drought effects.

# === NDVI ===
difNDVI_05 <- NDVI_051023_060923 - NDVI_051022_060922
difNDVI_06 <- NDVI_062523_072523 - NDVI_062522_072522
difNDVI_08 <- NDVI_080923_090923 - NDVI_080922_090922
difNDVI_09 <- NDVI_092523_102523 - NDVI_092522_102522

par(mfrow = c(2, 2))
plot(difNDVI_05[[1]], col = clv, main = "NDVI Diff: 05.10–06.09")
plot(difNDVI_06[[1]], col = clv, main = "NDVI Diff: 06.25–07.25")
plot(difNDVI_08[[1]], col = clv, main = "NDVI Diff: 08.09–09.09")
plot(difNDVI_09[[1]], col = clv, main = "NDVI Diff: 09.25–10.25")

dev.off()

# === NDWI ===

#----------------------------------------------------
# difNDWI_06 <- NDWI_062523_072523 - NDWI_062522_072522
# Error: [-] extents do not match

# Projection 2023 on CRS from 2022
NDWI_062523_072523_proj <- terra::project(NDWI_062523_072523, NDWI_062522_072522)
# Resample for the extent and resolution match
NDWI_062523_072523_resampled <- terra::resample(NDWI_062523_072523_proj, NDWI_062522_072522)
#----------------------------------------------------

difNDWI_05 <- NDWI_051023_060923 - NDWI_051022_060922
difNDWI_06 <- NDWI_062523_072523_resampled - NDWI_062522_072522
difNDWI_08 <- NDWI_080923_090923 - NDWI_080922_090922
difNDWI_09 <- NDWI_092523_102523 - NDWI_092522_102522

par(mfrow = c(2, 2))
plot(difNDWI_05[[1]], col = clv, main = "NDWI Diff: 05.10–06.09")
plot(difNDWI_06[[1]], col = clv, main = "NDWI Diff: 06.25–07.25")
plot(difNDWI_08[[1]], col = clv, main = "NDWI Diff: 08.09–09.09")
plot(difNDWI_09[[1]], col = clv, main = "NDWI Diff: 09.25–10.25")

dev.off()

#_______________________________________________________________________________

# NDVI and NDWI values range from +1.0 to -1.0. 

# Areas of barren rock, sand, or snow usually
# show very low NDVI values (for example, 0.1 or less). Sparse vegetation, such as 
# shrubs and grasslands or senescing crops, may result in moderate NDVI values 
# (approximately 0.2 to 0.5). High NDVI values (approximately 0.6 to 0.9) 
# correspond to dense vegetation such as that found in temperate and tropical 
# forests or crops at their peak growth stage. 

# Calculation of NDVI difference (ΔNDVI) between periods 
# to quantify changes in vegetation activity or health over time,
# comparing similar phenological periods across 2 different years.

# Interpretation of ΔNDVI values:
#   ≈ 0   --> Vegetation remains stable
#   < 0   --> Decrease in vegetation activity (possible stress or degradation)
#   > 0   --> Increase in vegetation activity (growth, recovery)
#   Values near -1 or -2 indicate drastic decline (e.g., total vegetation loss)
#   Values near +1 or +2 indicate a strong increase (e.g., new growth or recovery)

val_difNDVI_05 <- values(difNDVI_05) |> 
  na.omit()
val_difNDVI_06 <- values(difNDVI_06) |> 
  na.omit()
val_difNDVI_08 <- values(difNDVI_08) |> 
  na.omit()
val_difNDVI_09 <- values(difNDVI_09) |> 
  na.omit()

summary(val_difNDVI_05)
summary(val_difNDVI_06)
summary(val_difNDVI_08)
summary(val_difNDVI_09)

####
# Function to categorize the NDVI differences based on the limits
categorize_ndvi_diff <- function(x) {
  cut(x, breaks = seq(-2, 2, length.out = 6), right = TRUE)
}
####

dh1 <- categorize_ndvi_diff(val_difNDVI_05)
dh2 <- categorize_ndvi_diff(val_difNDVI_06)
dh3 <- categorize_ndvi_diff(val_difNDVI_08)
dh4 <- categorize_ndvi_diff(val_difNDVI_09)

# Construction of the dataframe
df_dh1 <- as.data.frame(table(dh1))
colnames(df_dh1) <- c("NDVI_Interval", "Count")

df_dh2 <- as.data.frame(table(dh2))
colnames(df_dh2) <- c("NDVI_Interval", "Count")

df_dh3 <- as.data.frame(table(dh3))
colnames(df_dh3) <- c("NDVI_Interval", "Count")

df_dh4 <- as.data.frame(table(dh4))
colnames(df_dh4) <- c("NDVI_Interval", "Count")

# Order for vegetative interval
df_dh1$NDVI_Interval <- factor(df_dh1$NDVI_Interval, levels = levels(dh1), ordered = TRUE)
df_dh2$NDVI_Interval <- factor(df_dh2$NDVI_Interval, levels = levels(dh2), ordered = TRUE)
df_dh3$NDVI_Interval <- factor(df_dh3$NDVI_Interval, levels = levels(dh3), ordered = TRUE)
df_dh4$NDVI_Interval <- factor(df_dh4$NDVI_Interval, levels = levels(dh4), ordered = TRUE)

# Plotting the various NDVI differences (ΔNDVI)

h1 <- ggplot(df_dh1,  aes(x = NDVI_Interval, y = Count, fill = NDVI_Interval)) +
  geom_bar(stat = "identity",  position = "dodge") + 
  scale_fill_viridis_d(option = "D") + 
  geom_text(aes(label = Count), vjust = -0.3, size = 3) +
  labs(title = "NDVI differences: 05.10–06.09", x = "ΔNDVI interval", y = "Amount of pixels") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5))
h1


h2 <- ggplot(df_dh2, aes(x = NDVI_Interval, y = Count, fill = NDVI_Interval)) +
  geom_bar(stat = "identity",  position = "dodge") + 
  scale_fill_viridis_d(option = "D") + 
  geom_text(aes(label = Count), vjust = -0.3, size = 3) +
  labs(title = "NDVI differences: 06.25–07.25", x = "ΔNDVI interval", y = "Amount of pixels") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5))
h2

h3 <- ggplot(df_dh3, aes(x = NDVI_Interval, y = Count, fill = NDVI_Interval)) +
  geom_bar(stat = "identity",  position = "dodge") + 
  scale_fill_viridis_d(option = "D") + 
  geom_text(aes(label = Count), vjust = -0.3, size = 3) +
  labs(title = "NDVI differences: 08.09–09.09", x = "ΔNDVI interval", y = "Amount of pixels") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5))
h3


h4 <- ggplot(df_dh4, aes(x = NDVI_Interval, y = Count, fill = NDVI_Interval)) +
  geom_bar(stat = "identity",  position = "dodge") + 
  scale_fill_viridis_d(option = "D") + 
  geom_text(aes(label = Count), vjust = -0.3, size = 3) +
  labs(title = "NDVI differences: 09.25–10.25", x = "ΔNDVI interval", y = "Amount of pixels") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5))
h4


h1+h2+h3+h4

#_______________________________________________________________________________


# NDWI (Normalized Difference Water Index) is used to monitor changes in water content 
# of vegetation or detect the presence of surface water. It typically ranges between -1 and +1.

# Interpretation of NDWI values:
#   < 0   --> Typically indicates land surfaces such as soil, dry vegetation, or built-up areas
#   ≈ 0   --> Transitional zones; may correspond to moist soil or sparse vegetation
#   > 0   --> Indicates the presence of water or high moisture content in vegetation
#   Values closer to +1 suggest strong water signals (e.g., open water bodies)
#   Values closer to -1 indicate very dry areas or impervious surfaces

# Calculation of NDWI difference (ΔNDWI) between time periods 
# allows for the detection of hydrological changes, such as:
#   - Reduction in surface water (e.g., due to drought or drainage)
#   - Increase in moisture (e.g., after rainfall or irrigation)
#   - Monitoring vegetation water stress or wetland dynamics

# Interpretation of ΔNDWI values:
#   ≈ 0   --> No significant change in moisture or water presence
#   < 0   --> Decrease in water content (drying trend)
#   > 0   --> Increase in water content (wetting trend)
#   Values near -1 indicate strong water loss (e.g., wetland drying)
#   Values near +1 indicate strong water gain (e.g., flooding or recharge)

val_difNDWI_05 <- values(difNDWI_05) |> 
  na.omit()
val_difNDWI_06 <- values(difNDWI_06) |> 
  na.omit()
val_difNDWI_08 <- values(difNDWI_08) |> 
  na.omit()
val_difNDWI_09 <- values(difNDWI_09) |> 
  na.omit()

summary(val_difNDWI_05)
summary(val_difNDWI_06)
summary(val_difNDWI_08)
summary(val_difNDWI_09)

dhw1 <- categorize_ndvi_diff(val_difNDWI_05)
dhw2 <- categorize_ndvi_diff(val_difNDWI_06)
dhw3 <- categorize_ndvi_diff(val_difNDWI_08)
dhw4 <- categorize_ndvi_diff(val_difNDWI_09)

# Construction of the dataframe
df_dhw1 <- as.data.frame(table(dhw1))
colnames(df_dhw1) <- c("NDWI_Interval", "Count")

df_dhw2 <- as.data.frame(table(dhw2))
colnames(df_dhw2) <- c("NDWI_Interval", "Count")

df_dhw3 <- as.data.frame(table(dhw3))
colnames(df_dhw3) <- c("NDWI_Interval", "Count")

df_dhw4 <- as.data.frame(table(dhw4))
colnames(df_dhw4) <- c("NDWI_Interval", "Count")

# Order for vegetative interval
df_dhw1$NDWI_Interval <- factor(df_dhw1$NDWI_Interval, levels = levels(dhw1), ordered = TRUE)
df_dhw2$NDWI_Interval <- factor(df_dhw2$NDWI_Interval, levels = levels(dhw2), ordered = TRUE)
df_dhw3$NDWI_Interval <- factor(df_dhw3$NDWI_Interval, levels = levels(dhw3), ordered = TRUE)
df_dhw4$NDWI_Interval <- factor(df_dhw4$NDWI_Interval, levels = levels(dhw4), ordered = TRUE)

# Plotting the various NDVI differences (ΔNDVI)
hw1 <- ggplot(df_dhw1, aes(x = NDWI_Interval, y = Count, fill = NDWI_Interval)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_viridis_d(option = "D") + 
  geom_text(aes(label = Count), vjust = -0.3, size = 3) +
  labs(title = "NDWI differences: 05.10–06.09", x = "ΔNDWI interval", y = "Amount of pixels") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5))

hw2 <- ggplot(df_dhw2, aes(x = NDWI_Interval, y = Count, fill = NDWI_Interval)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_viridis_d(option = "D") + 
  geom_text(aes(label = Count), vjust = -0.3, size = 3) +
  labs(title = "NDWI differences: 06.25–07.25", x = "ΔNDWI interval", y = "Amount of pixels") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5))

hw3 <- ggplot(df_dhw3, aes(x = NDWI_Interval, y = Count, fill = NDWI_Interval)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_viridis_d(option = "D") + 
  geom_text(aes(label = Count), vjust = -0.3, size = 3) +
  labs(title = "NDWI differences: 08.09–09.09", x = "ΔNDWI interval", y = "Amount of pixels") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5))

hw4 <- ggplot(df_dhw4, aes(x = NDWI_Interval, y = Count, fill = NDWI_Interval)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_viridis_d(option = "D") + 
  geom_text(aes(label = Count), vjust = -0.3, size = 3) +
  labs(title = "NDWI differences: 09.25–10.25", x = "ΔNDWI interval", y = "Amount of pixels") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5))

hw1 + hw2 + hw3 + hw4

#_______________________________________________________________________________

# Values of the single years for the statistical analysis 

val_NDVI_051022_060922 <- values(NDVI_051022_060922) |> 
  na.omit()
val_NDVI_062522_072522 <- values(NDVI_062522_072522) |> 
  na.omit()
val_NDVI_080922_090922 <- values(NDVI_080922_090922) |> 
  na.omit()
val_NDVI_092522_102522 <- values(NDVI_092522_102522) |>   
  na.omit()

val_NDVI_051023_060923 <- values(NDVI_051023_060923) |> 
  na.omit()
val_NDVI_062523_072523 <- values(NDVI_062523_072523) |> 
  na.omit()
val_NDVI_080923_090923 <- values(NDVI_080923_090923) |> 
  na.omit()
val_NDVI_092523_102523 <- values(NDVI_092523_102523) |>   
  na.omit()


# Using the Wilcoxon rank-sum test (Mann-Whitney) with paired = FALSE 
# because the two NDVI images being compared come from the same period 
# in different years but may not have perfectly matching pixel positions 
# or identical sample sizes. This non-parametric test does not assume 
# normality and is robust to non-Gaussian distributions and outliers, 
# making it suitable for comparing overall differences in NDVI 
# distributions between the two periods.

wilcox.test(val_NDVI_051022_060922, val_NDVI_051023_060923, paired = FALSE)
#Wilcoxon rank sum test with continuity correction
#data:  val_NDVI_051022_060922 and val_NDVI_051023_060923
#W = 1.6496e+13, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(val_NDVI_062522_072522, val_NDVI_062523_072523, paired = FALSE)
#Wilcoxon rank sum test with continuity correction
#data:  val_NDVI_062522_072522 and val_NDVI_062523_072523
#W = 1.703e+13, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(val_NDVI_080922_090922, val_NDVI_080923_090923, paired = FALSE)
#Wilcoxon rank sum test with continuity correction
#data:  val_NDVI_080922_090922 and val_NDVI_080923_090923
#W = 1.6302e+13, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(val_NDVI_092522_102522, val_NDVI_092523_102523, paired = FALSE)
#Wilcoxon rank sum test with continuity correction
#data:  val_NDVI_092522_102522 and val_NDVI_092523_102523
#W = 1.4717e+13, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0


# All Wilcoxon rank-sum tests indicate statistically significant differences 
# in the distribution of NDVI values between 2022 and 2023 for each period analyzed. 
# This suggests that vegetation activity varied between the two years, 
# likely due to climatic conditions or environmental disturbances.
# 
# The Wilcoxon test does not indicate the direction of the change, 
# so to better understand the magnitude and trend of NDVI differences, 


# Extraction of the single values foe each year for NDWI

val_NDWI_051022_060922 <- values(NDWI_051022_060922) |> 
  na.omit()
val_NDWI_062522_072522 <- values(NDWI_062522_072522) |> 
  na.omit()
val_NDWI_080922_090922 <- values(NDWI_080922_090922) |> 
  na.omit()
val_NDWI_092522_102522 <- values(NDWI_092522_102522) |>   
  na.omit()

val_NDWI_051023_060923 <- values(NDWI_051023_060923) |> 
  na.omit()
val_NDWI_062523_072523_resampled <- values(NDWI_062523_072523_resampled) |> 
  na.omit()
val_NDWI_080923_090923 <- values(NDWI_080923_090923) |> 
  na.omit()
val_NDWI_092523_102523 <- values(NDWI_092523_102523) |>   
  na.omit()


# Wilcoxon rank-sum test to compare NDWI values between years.
# paired = FALSE because pixel positions may not correspond exactly
# (i.e., we are comparing independent samples, not matched observations).


wilcox.test(val_NDWI_051022_060922, val_NDWI_051023_060923, paired = FALSE)
#Wilcoxon rank sum test with continuity correction
#data:  val_NDWI_051022_060922 and val_NDWI_051023_060923
#W = 1.5385e+13, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(val_NDWI_062522_072522, val_NDWI_062523_072523_resampled, paired = FALSE)
#Wilcoxon rank sum test with continuity correction
#data:  val_NDWI_062522_072522 and val_NDWI_062523_072523_resampled
#W = 1.4901e+13, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(val_NDWI_080922_090922, val_NDWI_080923_090923, paired = FALSE)
#Wilcoxon rank sum test with continuity correction
#data:  val_NDWI_080922_090922 and val_NDWI_080923_090923
#W = 1.58e+13, p-value = 1.184e-05
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(val_NDWI_092522_102522, val_NDWI_092523_102523, paired = FALSE)
#Wilcoxon rank sum test with continuity correction
#data:  val_NDWI_092522_102522 and val_NDWI_092523_102523
#W = 1.8872e+13, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0


# Summary of Wilcoxon rank-sum test results (NDVI and NDWI comparisons between 2022 and 2023):
#
# All NDVI comparisons between the same seasonal periods in 2022 and 2023 show statistically significant 
# differences (p-value < 2.2e-16) suggest a substantial vegetation activity shift across all periods.
#
# Similarly, all NDWI comparisons show statistically significant differences as well. Three out of four 
# periods have p-values < 2.2e-16, while the remaining period (August–September) still shows significance 
# (p = 1.184e-05), indicating consistent changes in vegetation water content.
#
# These results support the hypothesis that environmental factors, such as drought or climate anomalies, had 
# a measurable impact on vegetation conditions in the study area between 2022 and 2023.nal periods in 2022 and 2023 show statistically significant 
# differences (p-value < 2.2e-16) suggest a substantial vegetation activity shift across all periods.
#
# Similarly, all NDWI comparisons show statistically significant differences as well. Three out of four 
# periods have p-values < 2.2e-16, while the remaining period (August–September) still shows significance 
# (p = 1.184e-05), indicating consistent changes in vegetation water content.
#
# These results support the hypothesis that environmental factors, such as drought or climate anomalies, had 
# a measurable impact on vegetation conditions in the study area between 2022 and 2023.

