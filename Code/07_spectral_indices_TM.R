#spectral images
# vegetation indices

library(imageRy)
library(terra)

im.list()
#we are going to import the image matogrosso_ast_2006209_lrg.jpg
#the image has already the image composed by all the band and not the single bands

#ast means aster that it's anaother satellite with generally a wider resolution of pixel
m2006 <- im.import("matogrosso_ast_2006209_lrg.jpg")
m2006

#we can look in the earth observatory ro see if it's still on that site
#this image rapresent the deforestation of mato grosso
#band number 1 is the NIR
#band number 2 is the red band
#band number 3 is the green band
im.plotRGB(m2006, r=1, g=2, b=3)

#we can also plot single bands
plot(m2006[[2]])
plot(m2006[[1]])
# 1 is th near infrared that carries a lot of info on the type of vegetation 

im.plotRGB(m2006, r=1, g=2, b=3)
#in 1992 thi was complitely covered in forest and now is completely ruined by human action
#we can enanche the info by putting the NIR on top of the blue we will have a clear definition of soil
im.plotRGB(m2006, r=3, g=2, b=1)
#in this way we enanched the bare soil contrast
#water in this image is coloured cause it's basically dirty/polluted

im.plotRGB(m2006, r=2, g=1, b=3)

#now import the image from 1992
#this image is from landsat
m1992 <- im.import("matogrosso_l5_1992219_lrg.jpg")
im.plotRGB(m1992, r=1, g=2, b=3)

#now a multi-frame comparing the situation
par(mfrow=c(1,2))
im.plotRGB(m1992, r=1, g=2, b=3)
im.plotRGB(m2006, r=1, g=2, b=3)


dev.off()
# exercise make a multi-frame with 3 rows per 2 columns 
par(mfrow=c(3,2))
im.plotRGB(m1992, r=1, g=2, b=3)
im.plotRGB(m2006, r=1, g=2, b=3)
im.plotRGB(m1992, r=2, g=2, b=1)
im.plotRGB(m2006, r=2, g=2, b=1)
im.plotRGB(m1992, r=3, g=1, b=2)
im.plotRGB(m2006, r=3, g=1, b=2)

#the important part is the NIR band and the other are just visible colors

#problem of the vegetation index
#basically a sane spectral signature of healthy vegetation absorbs all the blue and red
#we could have slightly different signatures from other species but it is more or less the same
#we could find different species of course based on the spectral signature

#in the red a healthy plant reflects X and we can make a DVI (different vegetation difference) (=NIR-Red=100-10=90)
#a not so healthy plant photosynthesis is compromised and the blue and the red reflectants are going to be higher and the chloroplast are not responding well
# WE WILL ALSO HAVE A DECREASE IN THE NIR AND THE GREEN
#DVI = NIR-Red = 80-20 =60
#suffering plants will have a decrease in DVI

#let's calculate it
#we are subtracting the Red layer to the NIR
dvi1992 = m1992[[1]] - m1992[[2]]
#let's plot the data with a palette

dev.off()
cl <- colorRampPalette(c("darkblue", "yellow", "red", "black")) (100)
plot(dvi1992, col=cl)

#let's see it in 2006
dvi2006 = m2006[[1]] - m2006[[2]]
plot(dvi2006, col=cl)


par(mfrow=c(1,2))
plot(dvi1992, col=cl)
plot(dvi2006, col=cl)

#NDVI
#let's normalize the DVI
#placing all the DVI on a scale from 1 to 0
# useful to compare different DVI from different scales
#in general everyone is using NDVI
ndvi1992 = dvi1992 / (m1992[[1]] + m1992[[2]])
ndvi2006 = dvi2006 / (m2006[[1]] + m2006[[2]])
dev.off()

par(mfrow=c(1,2))
plot(ndvi1992, col=cl)
plot(ndvi2006, col=cl)
