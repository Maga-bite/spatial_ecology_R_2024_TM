
#code for managing and analazing remote sensing data

install.packages("devtools")
library("devtools")

#it is needed the installation of Rtools from a different installation package 
# install.packages "Rtools"

install_github("ducciorocchini/imageRy") #function from devtools for the use of the package imageRy


library(imageRy)
im.list()
