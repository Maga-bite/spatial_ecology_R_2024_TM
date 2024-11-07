# relation among species in time
#code to estimate the temporal overlap between species
#exploring the prey-predator relationship through time

install.packages("overlap")
library(overlap)
# ?overlap gives us information
?overlap

#dataset called kerinci
data(kerinci)
head(kerinci)
# in this way we can see the 6 first rows of the data set
#we will need to convert the data of time into hours
summary(kerinci)
#will give a summary 

#we need to create a new thingh to put time in radiants
#the collumn time is part of the data
kerinci$timecirc <- kerinci$Time*2*pi
#we can create a new collumn or assign it to a new object (we created a new collumn)
head(kerinci)

#we are just going to look at the tiger data and we are creating a new object to put in all th tiger data
tiger <- kerinci[kerinci$Sps=="tiger",]
# "," closes the query in the last script 

#densityplot is for example a line over an istogram that ondicates the density of a particular data

tigertime <- tiger$timecirc
tigertime

densityPlot(tigertime)

#let's do the same for another specie and then overlap it
#excercise select the data for the macaque and assign to a new object

macaque <- kerinci[kerinci$Sps=="macaque",]
macaquetime <- macaque$timecirc
macaquetime





#----SQL
#everything but macaque
summary (macaque)
nomacaque <- kerinci[kerinci$Sps!="macaque",]
summary(nomacaque)

densityPlot(macaquetime)

overlapPlot(tigertime, macaquetime)
