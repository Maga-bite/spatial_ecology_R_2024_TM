# Spatial Ecology Project: NDVI & Drought Impact - Province of Sondrio
# Author: Tommaso Magarotto
# Goal: Analyze the impact of drought on vegetation using NDVI (Sentinel-2) and climate data, aggregated by season over the last 5 years

#working to get the poliygon of the province

getwd()
library(sf) #Support for simple feature access
#a standardized way to encode and analyze spatial vector data


#https://www.istat.it/notizia/confini-delle-unita-amministrative-a-fini-statistici-al-1-gennaio-2018-2/
# Load of the ISTAT data to focus on the province 
province <- st_read("C:/Users/Tommy/Documents/Limiti01012025/ProvCM01012025")

# Filtering the Province under study
sondrio <- province[province$DEN_UTS == "Sondrio", ]

# checking the Polygon
plot(st_geometry(sondrio), main = "Sondrio Province")

#since this province has complex administrative boundries it is better for 
#the sake of the project to simplify the constrains of the analisis to a simple 
#rctancular shape that engulf all the province, in this way we can give an idea 
#of the succesion of the stress also not related to the specific area 




################################################################################

#TENTATIVO DI OTTENERE UN FUILE DA INSERIRE SU COPERNICUS MA E TROPPO COMPLESSO
# Esporta il poligono in formato GeoJSON o WKT per EO Browser
#st_write(sondrio, "sondrio.geojson", driver = "GeoJSON")  # per Sentinel EO Browser

# This code saves the polygon representing the province of Sondrio in a GeoJSON file format. 
# The 'st_write' function from the 'sf' package is used to write spatial data to the specified file.
# The polygon is selected from the 'province' dataset, which contains all Italian provinces, 
# by filtering for the province of Sondrio using the 'DEN_UTS' column.
# The output file "sondrio.geojson" can then be used in GIS tools, including Sentinel Hub EO Browser, 
# for downloading and analyzing Sentinel-2 satellite imagery over the selected area.


# Estrai solo la geometria della provincia di Sondrio
#geometry_sondrio <- sondrio$geometry

# Convert the geometry to WKT (Well-Known Text) format
#wkt_string <- st_as_text(geometry_sondrio)

# Print the WKT string to use in Copernicus EO Browser
#print(wkt_string)

 #this is too complex as it has also the 3d in it
# Elimina la terza dimensione Z
#sondrio_2d <- st_zm(sondrio)

# Semplifica se necessario (facoltativo, solo se è troppo dettagliato)
#sondrio_simple <- st_simplify(sondrio_2d, dTolerance = 0.01)

# 5. Estrai la geometria e converti in WKT (Well-Known Text)
wkt_string <- st_as_text(sondrio_simple$geometry[[1]])

# 6. Stampa il WKT in console (puoi copiarlo da qui)
#cat(wkt_string)



# 3. Prendi la bounding box (rettangolo che contiene tutto)
#bbox <- st_bbox("sondrio.geojson")

# 4. Convertila in poligono
#bbox_poly <- st_as_sfc(bbox)

# 5. Converti in stringa WKT
#wkt_string <- st_as_text(bbox_poly)

# 6. Visualizza il WKT da copiare e incollare su EO Browser
#cat(wkt_string)




#install.packages("dplyr")


#library(sf)
#library(dplyr)

# Carica i confini
#italy_prov <- st_read("https://raw.githubusercontent.com/openpolis/geojson-italy/master/geojson/province/limits_IT_provinces.geojson")

# Filtra Sondrio
#sondrio <- italy_prov %>% filter(provincia == "Sondrio")

# Ottieni la bounding box (rettangolo che racchiude tutto)
#bbox <- st_bbox(sondrio)  # Restituisce xmin, ymin, xmax, ymax

# Convertila in poligono
#bbox_poly <- st_as_sfc(bbox)

# Converti in stringa WKT
#wkt_string <- st_as_text(bbox_poly)

#cat(wkt_string)

################################################################################





install.packages("geodata")
library(geodata)
library(sf)

# Scarica i confini amministrativi dell'Italia (livello 2 = province)
italy_admin2 <- gadm(country = "ITA", level = 2, path = tempdir())

# Filtra per la provincia di Sondrio
sondrio <- italy_admin2[italy_admin2$NAME_2 == "Sondrio", ]

bbox <- st_bbox(sondrio)
bbox_poly <- st_as_sfc(bbox)
plot(bbox_poly)

st_as_text(bbox_poly)













library(sf)

# 1. Carica il layer ISTAT
province_istat <- st_read("C:/Users/Tommy/Documents/Limiti01012025/ProvCM01012025")
sondrio_istat <- province_istat[province_istat$DEN_UTS == "Sondrio", ]

# 2. Scarica il layer da geodata
library(geodata)
italy_admin2 <- gadm(country = "ITA", level = 2, path = tempdir())
sondrio_gadm <- italy_admin2[italy_admin2$NAME_2 == "Sondrio", ]

# 3. Controlla e armonizza CRS (sistemi di riferimento)
st_crs(sondrio_istat)
st_crs(sondrio_gadm)

# Se diversi, trasformali nello stesso CRS (es: quello del primo)
sondrio_gadm <- st_transform(sondrio_gadm, crs = st_crs(sondrio_istat))

# 4. Sovrapposizione con plot base
plot(st_geometry(sondrio_istat), col = NA, border = "blue", lwd = 2, main = "Confronto ISTAT vs GADM")
plot(st_geometry(sondrio_gadm), col = NA, border = "red", add = TRUE, lwd = 2)
legend("bottomleft", legend = c("ISTAT", "GADM"), col = c("blue", "red"), lty = 1, lwd = 2)








# Librerie necessarie
library(sf)
library(terra)
library(geodata)
library(ggplot2)

# 1. Carica dati ISTAT
province_istat <- st_read("C:/Users/Tommy/Documents/Limiti01012025/ProvCM01012025")
sondrio_istat <- province_istat[province_istat$DEN_UTS == "Sondrio", ]

# 2. Scarica e filtra GADM (usa terra::gadm)
italy_admin2 <- gadm(country = "ITA", level = 2, path = tempdir())
sondrio_gadm <- italy_admin2[italy_admin2$NAME_2 == "Sondrio", ]

# 3. Converte da SpatVector a sf
sondrio_gadm <- st_as_sf(sondrio_gadm)

# 4. Armonizza il sistema di coordinate
sondrio_gadm <- st_transform(sondrio_gadm, crs = st_crs(sondrio_istat))

# 5. Aggiungi colonna per distinguere le fonti
sondrio_istat$source <- "ISTAT"
sondrio_gadm$source <- "GADM"

# 6. Unisci i due dataset
combined <- rbind(sondrio_istat, sondrio_gadm)

# 7. Plot con ggplot2
ggplot(combined) +
  geom_sf(aes(color = source), fill = NA, linewidth = 1.2) +
  scale_color_manual(values = c("ISTAT" = "blue", "GADM" = "red")) +
  ggtitle("Sovrapposizione dei confini della provincia di Sondrio") +
  theme_minimal()












library(sf)
library(terra)
library(geodata)
library(ggplot2)
library(dplyr)

# 1. Carica dati ISTAT
province_istat <- st_read("C:/Users/Tommy/Documents/Limiti01012025/ProvCM01012025")
sondrio_istat <- province_istat[province_istat$DEN_UTS == "Sondrio", ]

# 2. Scarica e filtra GADM
italy_admin2 <- gadm(country = "ITA", level = 2, path = tempdir())
sondrio_gadm <- italy_admin2[italy_admin2$NAME_2 == "Sondrio", ]

# 3. Converti SpatVector in sf
sondrio_gadm <- st_as_sf(sondrio_gadm)

# 4. Armonizza CRS
sondrio_gadm <- st_transform(sondrio_gadm, crs = st_crs(sondrio_istat))

# 5. Crea nuovi oggetti solo con geometria e colonna "source"
sondrio_istat_sf <- sondrio_istat %>% 
  st_geometry() %>% 
  st_sf(source = "ISTAT", geometry = .)

sondrio_gadm_sf <- sondrio_gadm %>% 
  st_geometry() %>% 
  st_sf(source = "GADM", geometry = .)

# 6. Unisci i due layer
combined <- rbind(sondrio_istat_sf, sondrio_gadm_sf)

# 7. Plot
ggplot(combined) +
  geom_sf(aes(color = source), fill = NA, linewidth = 1.2) +
  scale_color_manual(values = c("ISTAT" = "blue", "GADM" = "red")) +
  ggtitle("Sovrapposizione dei confini della provincia di Sondrio") +
  theme_minimal()













library(sf)
library(terra)
library(geodata)
library(ggplot2)
library(dplyr)

# 1. Carica dati ISTAT
province_istat <- st_read("C:/Users/Tommy/Documents/Limiti01012025/ProvCM01012025")
sondrio_istat <- province_istat[province_istat$DEN_UTS == "Sondrio", ]

# 2. Scarica e filtra GADM
italy_admin2 <- gadm(country = "ITA", level = 2, path = tempdir())
sondrio_gadm <- italy_admin2[italy_admin2$NAME_2 == "Sondrio", ]

# 3. Converti SpatVector in sf
sondrio_gadm <- st_as_sf(sondrio_gadm)

# 4. Armonizza CRS
sondrio_gadm <- st_transform(sondrio_gadm, crs = st_crs(sondrio_istat))

# 5. Controlla e correggi geometrie
sondrio_istat <- st_make_valid(sondrio_istat)
sondrio_gadm  <- st_make_valid(sondrio_gadm)

# 6. Crea oggetti solo con geometria e colonna 'source'
sondrio_istat_sf <- st_sf(source = "ISTAT", geometry = st_geometry(sondrio_istat))
sondrio_gadm_sf  <- st_sf(source = "GADM",  geometry = st_geometry(sondrio_gadm))

# 7. Unisci
combined <- rbind(sondrio_istat_sf, sondrio_gadm_sf)

# 8. Plot
ggplot(combined) +
  geom_sf(aes(color = source), fill = NA, linewidth = 1.2) +
  scale_color_manual(values = c("ISTAT" = "blue", "GADM" = "red")) +
  ggtitle("Sovrapposizione dei confini della provincia di Sondrio") +
  theme_minimal()
