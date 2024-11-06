# This code is related to multivariate analysis in R for monitoring communities
# to see the relationship in space among different species

install.packages("vegan")
library(vegan)

data(dune)
dune

#if u type that u will have all the data sets with all the various plots
#there is a function, head, of this data set and it will shows only the first 6 lines
head(dune)

#another manner is the view function that allows you to see all the tables
#View(dune)

#inthe package vegan the simplest function is the detrended correspondence analysis "decorana"
#this function reduce the dimention and this is exactly what is done for the PCA
decorana(dune)
multivar <-decorana(dune)
multivar
#passing from the original pack to just a few axis
#as an example the leght means the amount of range rappresented from the first axis in all dimention
#the most amount of axis we can use is 3

#axis lengh
dca1 = 3.7004
dca2 = 3.1166 
dca3 = 1.30055
dca4 = 1.47888

total = dca1 + dca2 + dca3 + dca4 
total
#to make the % we need to multiplate the dcax100

#proportio of the axis
prop1 = dca1/total
prop2 = dca2/total
prop3 = dca3/total
prop4 = dca4/total

#percentages
perc1 = prop1*100
perc2 = prop2*100
perc3 = prop3*100
perc4 = prop4*100

perc1
perc2 
perc3 
perc4

#I want to know the lenght of the first two axis
#whole amount of variability (%)
perc1 + perc2

#like this i'm losing 29 % of the data 
#the first two axes explain 71 %
plot(multivar)

#in the down left part we are looking at a community of species living in grasslands
#the idea is to pass from big plot of data to simpler graph in which we can understand the species as a community
#we can asess some pattern from the data set, for example the flower and the butterfly
