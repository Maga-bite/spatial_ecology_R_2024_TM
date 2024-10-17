#as we did for spatstat we are installing new packages

install.packages("sdm")
library(sdm)
#this is species distribution model
#using covariants (will be epxplained in species distribution model later)


install.packages("terra")
library(terra)

#last lecture we talked about population density
#today we will talk about distribution
#we are learning how to do a subset of file in a datafile
#with the package we also installed a file dataset
#inside di sdm folder there is a external folder with a lot of data

#last lecture we talked about population density
#today we will talk about distribution

#this finds the name of R system files and it checks whereare the files
file <- system.file("external/species.shp", package="sdm")
#we started the folder and now we will need quotes to establish the path
#extention shp is related to the data related to species (sph= shape file), it is a file originally from a company and it has been established as standard
#now we need to establish which package we want to use 
#in general / means it is what we want to search for by specifying what is inside


file
#here we can see the path of the file
#differently from before we dont have the file ready for R, so this procedure we are using a file and making it usable to R
#we are going to save this data
#this datas rappresents the diatribution of a frong in space
#all the points we have a vector that posintions them
#last time we had a raster

#we are using vect to translate the file in manner which R may use
#R cannot use directly .shp, we are transalting through spatvector
rana <- vect(file)
#before recalling vect you need to recall the package terra
library(terra)
rana
#for each point we have different data attached
#the extent in terms of coordiates
#we have coordinates also in the coord. ref. 
#we have occurance that is being mesuared, in different point we add a 0 or a 1 depending we found in the place the rana
rana$Occurrence
#this gives out every point
#this points we have 3 information X Y and occurance
#let's plot
plot(rana)

#let's select only the presences in the point registered with 1
#let's select the point from rana where occurance equals 1, in sequel = is ==
#in sql all statments end with ";"
#selection froma a data set = rana, the we will have to state the conditioni in [] with a double =
pres<-rana[rana$Occurrence==1]
plot(pres)

#we can open a multiframe we all the point and the other with only the presence
#exercise
par(mfrow=c(1,2))
plot(rana, main = "ciao")
plot(pres)
dev.off()
#with  "main = "ciao"" we can add titles to the graph
