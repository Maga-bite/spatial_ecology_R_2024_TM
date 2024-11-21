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

#now a multiframe comparing the situation
par(mfrow=c(1,2))
im.plotRGB(m1992, r=1, g=2, b=3)
im.plotRGB(m2006, r=1, g=2, b=3)
