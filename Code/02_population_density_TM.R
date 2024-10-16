#How to calculate the density of individuls of population

#Installing the spatsstat package
install.packages("spatstat")

#recalling the package in order to use it
library(spatstat)

#name of the dataset
bei
plot(bei)
#change the data rappresentation
plot(bei, pch=19)
plot(bei, pch=19, cex=0.5)

#bei is a vector set, the other is nei.extra and it is composed by pixels = "raster"
#it is a plane divadided in quadrant, bei.extra it is composd by images
bei.extra

plot(bei.extra)
#if we want to extract one of the tu raster images, we want to extract only elev
#extracting the data
elevation <- bei.extra$elev
#to link something toghether we should use "$", in this case we are linking the two images
> plot(elevation)

#in this case elev is the fisrt image and grad is the second
#explaining to R that we want just elev
elevation <- bei.extra[[1]]
plot(elevation)
#like this we have the same graph

#we have point scattered and we want those point to be putted on a image that shows the density of those



# Code related to population ecology

# A package is needed for point pattern analysis
# install.packages("spatstat")
library(spatstat)

# let's use the bei data:
# data description:
# https://CRAN.R-project.org/package=spatstat

bei

# plotting the data
plot(bei)

# changing dimension - cex
plot(bei, cex=.2)

# changing the symbol - pch
plot(bei, cex=.2, pch=19)

# additional datasets
bei.extra
plot(bei.extra)

# let's use only part of the dataset: elev
plot(bei.extra$elev)
elevation <- bei.extra$elev
plot(elevation)

# second method to select elements
elevation2 <- bei.extra[[1]]
plot(elevation2)

# passing from points to a countinuous surface
densitymap <- density(bei)
plot(densitymap)
points(bei, cex=.2)

########## DAY 2

# Plotting the maps one beside the other
par(mfrow=c(1,2))
plot(elevation2)
plot(densitymap)

# Exercise: make a multiframe with maps one on top of the other
par(mfrow=c(2,1))
plot(elevation2)
plot(densitymap)

# one frined to clear graphs
dev.off()
plot(elevation2)

# Changin colors to maps
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
cl <- colorRampPalette(c("black", "blue", "red"))(3)
plot(densitymap, col=cl)

cl <- colorRampPalette(c("black", "blue", "red"))(100)
plot(densitymap, col=cl)

# Exercise: change the color ramp palette using different colors
cln <- colorRampPalette(c("purple1", "orchid2", "palegreen3", "paleturquoise"))(100)
plot(densitymap, col=cln)

# Exrcise: build a multiframe and plot the densitymap with two different color ramp palettes one beside the other

par(mfrow=c(1,2))

cln <- colorRampPalette(c("purple1", "orchid2", "palegreen3", "paleturquoise"))(100)
plot(densitymap, col=cln)

clg <- colorRampPalette(c("green4", "green3", "green2", "green1", "green"))(100)
plot(densitymap, col=clg)

dev.off()
#a function for density map starting from individual points
#it is a function inside spatstat
densitymap <- density(bei)
#we putted it inside the object densitymap
densitymap
plot(densitymap)
#is there a possibility to show the map and the original points?
#doing it by commmon procedure
plot(bei)
#the previous graph is deleated so...
plot(densitymap)
points(bei)
points(bei, col="green")
#we have estimated the density of points 
#next lesson is going to also put elevation


