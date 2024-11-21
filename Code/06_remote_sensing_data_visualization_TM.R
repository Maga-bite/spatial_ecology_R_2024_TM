#code for managing and analazing remote sensing data

install.packages("devtools")
library("devtools")

#it is needed the installation of Rtools from a different installation package 
# install.packages "Rtools"

install_github("ducciorocchini/imageRy") #function from devtools for the use of the package imageRy


library(imageRy)
im.list()
library(terra)
# in case you have not terra

# install.packages("terra")
im.list() 
#this list all the data inside the package

b2 <- im.import("sentinel.dolomites.b2.tif") #importante to put the quotes
cl <- colorRampPalette(c("black", "grey", "light grey")) (100)
plot(b2, col=cl)
#presentation on gi hub on how the active and passive sensors work
# incident radiant flux = the energy that comes on top of the object that later is reflected as reflectant flux
#plant are absorbing the red trough photosyntesis,sothe reflectant mesures the % of radiant light in comparison with the reflected light 


#excerise: import b3 and plot it with the previous palette
b3 <- im.import("sentinel.dolomites.b3.tif")
plot(b3, col=cl)


#import the red band and see what happens
b4 <- im.import("sentinel.dolomites.b4.tif")
plot(b4, col=cl)

# by the plot of bad number 8 we are going to plot more into the infrared
# we can devide the infrare int o near (NIIR) and thermal infrared, thermal is related more to temperature related sensors, which they mesure the reflecting energy
#the middle part is short wave

#importing the near band
b8 <- im.import("sentinel.dolomites.b8.tif")
plot(b8, col=cl)

#near infrared is ore related to vegetation and we can plot all the 4 images togheter in a multiframe
#multiframe
par(mfrow=c(2,2))
plot(b2, col=cl)
plot(b3, col=cl)
plot(b4, col=cl)
plot(b8, col=cl)

# let's make a stack on the same image, we can consider them illments of the same array
sentstack<-c(b2,b3,b4,b8)
plot(sentstack, col=cl)
sentstack

dev.off()
#ploting one layer
plot(sentstack[[1]], col=cl)


plot(sentstack[[4]], col=cl)
#let's make a multiframe with different colorpalette
#when u plot one band u see that the original palette is going from blue to yellow 

#this was to try out
clb <- colorRampPalette(c("navyblue", "yellow", "red")) (100)
plot(b3, col=clb)

#this is an exercise
# here is realted to the color from each band
par(mfrow=c(2,2))

clb <- colorRampPalette(c("dark blue", "blue", "light blue")) (100)
plot(b2, col=clb)

clg <- colorRampPalette(c("dark green", "green", "light green")) (100)
plot(b3, col=clg)

clr <- colorRampPalette(c("dark red", "red", "pink")) (100)
plot(b4, col=clr)

cln <- colorRampPalette(c("brown", "orange", "yellow")) (100)
plot(b8, col=cln)

#always using infrared gives out a lot of informtion
#combining them in a scheme RGB in a natural color image
#RGB plotting
dev.off()
im.plotRGB(sentstack, r=3, g=2, b=1)
#now let's do the false color

# RGB space
# sentstack: 
# band2 blue element 1, sentstack[[1]] 
# band3 green element 2, sentstack[[2]]
# band4 red element 3, sentstack[[3]]
# band8 nir element 4, sentstack[[4]]
im.plotRGB(sentstack, r=3, g=2, b=1)
im.plotRGB(sentstack, r=4, g=3, b=2)
im.plotRGB(sentstack, r=3, g=4, b=2)
im.plotRGB(sentstack, r=3, g=2, b=4)

#the band rappresent different light spectrum and are understendable on the site: Sentinel-2 bands
#https://gisgeography.com/sentinel-2-bands-combinations/
