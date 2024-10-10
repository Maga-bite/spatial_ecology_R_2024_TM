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


