#how to import data on R, since we always used data from package
#one site u can use is earth observatory
#downloadable image
#we need to tell R that we are setting the workin directory in a determine working folder
#windows user: we need to use / not \
setwd("C:/Users/Tommy/Desktop/Downloads")
getwd()

#at this point we can import the data and we are not using imageRy but directly terra
getwd()
#this function is to understand which working directory we are using

scotland<-rast("scotland_outerhebrides_oli_20240918_lrg.jpg")
#the import is done by rast and remembrer to put the extention
plotRGB(scotland, r=1, g=2, b=3)

#on the git hub can be use the markdown that has a lor of data in Code/09_external_data_available.md

#exercise
stelvio19 <- rast("mtblanc_oli_2019237_lrg.jpg")
plotRGB(stelvio19, r=2, g=3, b=1)
