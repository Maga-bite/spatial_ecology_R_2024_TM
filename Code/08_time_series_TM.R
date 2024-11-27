# today we are going to talk about time series
# playing with data from sentinel
# copernicus program we can search for data, for example, we can track the amount of NOx in the atmosphere

library(terra)
library(imageRy)

# Listing all the available files
im.list()

#en means European nitrogen mainly the dioxide
#import the first one
EN01 <- im.import("EN_01.png")
#this is 3 layers of RGB with non-colorblind friendly package

#march is en 13
EN13 <- im.import("EN_13.png")
#big decrease during the stop of human activity

#let's see how wach file is composed
EN01

difEN = EN01[[1]] - EN13[[1]]
difEN
plot(difEN)
#this is taking the dataset and making a difference between the two

#example 2 ice melting
#we can import several files
gr <- im.import("greenland")
#this imports the 4 datasets "greenland.2000.tif" "greenland.2005.tif" "greenland.2010.tif" "greenland.2015.tif"
#we can plot one dataset
plot(gr[[1]])
plot(gr[[2]])
plot(gr[[3]])
plot(gr[[4]])
#in the last case is the file regarding 2015

# excersise plot in a multi-frame the first and last elements
par (mfrow=c(1,2))
plot(gr[[1]])
plot(gr[[4]])

#previously we made a different
dev.off()
difgr = gr[[1]] - gr[[4]]
plot(difgr)
#these areas are the ones in which the temperatures are  decreasing

#we can do the same thing we did with different types of bands with different images, all the pixels that turn different colors  in respect to what element we put there
im.plotRGB(gr, r=1, g=2, b=4)
# We put in the different colors b r and g the different years and we can see hoe (how) the RGB puts it
#every red pixel had a higher value in 2000, and every green had a higher value in 2005
#the blue area has higher temperatures

#there is another plt to show
#showing the different frequencies 
im.ridgeline(gr, scale=2, option="A")
# the professor is developing this plot
