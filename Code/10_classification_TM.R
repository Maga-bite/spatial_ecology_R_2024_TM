# Classifying satellite images and estimate the amount of change using imageRy
# classification type: unsupervised classification
# we will learn ho to get image and importing it in R

library(terra)
library(imageRy)

#solar orbiter
im.list()

sun<-im.import("Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg")
sun

#there are 3 classes of area high medium and low energy let's classify these in 3 classes
sunc<-im.classify(sun, num_cluster=3)
# we need to specify the number of classes/cluster

#depending on the poitn (randomly) in which it starts to classify that would be the class
#there is a way to reapet the process but it's complicated and we are not going to do it

#mtogrosso example
#using the matogrosso area we are going to do this
m1992<-im.import("matogrosso_l5_1992219_lrg.jpg")
m2006<-im.import("matogrosso_ast_2006209_lrg.jpg")

#let's start with classification
m1992c<-im.classify(m1992, num_clusters =2)
#class1= forest related areas while class 2= human related area and river
m2006c<-im.classify(m2006, num_clusters = 2)
#in this case from the reference that i had from previous image i can understand that the classes have not changed in this case
#class1= forest related areas while class 2= human related area and river

#now we want to calculate the frequency of pixels (the amount of pixels from each class)
#frequnecies
f1992<-freq(m1992c)
f1992
#some pixels sometimes are categorized as the other class since they are in the middle

#now let's calculate the percentage
#how to get the total
tot1992 <- ncell(m1992c)
p1992 = f1992*100/tot1992
#class1 = 83% forest 
#class2 = 17% human


f2006<-freq(m2006c)
f2006
tot2006 <- ncell(m2006c)
p2006 = f2006*100/tot2006
#class1 = 45% forest 
#class2 = 55% human

#now we can build  common table all togheter
#building a dataframe
#we need first to prepare the collumns
#first collumn class and assign the first
class <- c("Forest","Human")
y1992 <- c(83, 17)
#this was the percebtage of 1992 let's do 2006
y2006 <- c(45,55)

#let'sclamp togheter the collumn
tabout<-data.frame(class, y1992, y2006)

#now let's make a final graph
#now let's recall the ggplot package we are going to use
library("ggplot2")

#final graph
#the sintax is kinda strange
#we should state the dataser in use,the estetics(are related to what are u putting in XY and the final colour)
#the aes need to be stated
#we need to stand which graph we want to use
ggplot(tabout,aes(x=class,y=y1992,color=class)) + geom_bar(stat="identity", fill="white")
#these are real percentages for the 1992
#the same for 2006
ggplot(tabout,aes(x=class,y=y2006,color=class)) + geom_bar(stat="identity", fill="white")

#we need to merge this in a single graph thanks to a graph named patchwork and by summing it doeas this
install.packages("patchwork")
library("patchwork")

p1 <- ggplot(tabout, aes(x=class, y=y1992, color=class)) + geom_bar(stat="identity", fill="white")
p2 <- ggplot(tabout, aes(x=class, y=y2006, color=class)) + geom_bar(stat="identity", fill="white")
p1 + p2
#there is abiass in these graps the Y is not up to 100 and it's a problem
#let's change the limits of range

p1 <- ggplot(tabout, aes(x=class, y=y1992, color=class)) + geom_bar(stat="identity", fill="white") + ylim(c(0,100))
p2 <- ggplot(tabout, aes(x=class, y=y2006, color=class)) + geom_bar(stat="identity", fill="white") + ylim(c(0,100))
p1 + p2
#for example we can have one plot over the other
p1/p2

#now we can understand how pixel change in different timeframe
