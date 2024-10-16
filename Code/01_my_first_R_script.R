> 2 + 3 
[1] 5
> sonia <- 2 + 3
> sonia
[1] 5
> lea <- 3 + 4
> lea
[1] 7
> sonia + lea
[1] 12
> sonia ^ lea
[1] 78125
> (2+3)^(3+4)
[1] 78125
> "adding comments"
[1] "adding comments"
> # R can be used as acalculator

tommaso <- c(10, 20, 30, 50, 70) #this is an array #c is concatenate crumping all data
tommaso
erica <- c(1,3,5,7,9) 
erica

plot(tommaso, erica) #inside () they are arguments

#we want to do a plot where we put tommaso and erica datas in relation
#let's put the data in points
plot(tommaso, erica)
plot(tommaso, erica, pch=19) #pch changes the point charachter
plot(tommaso, erica, pch=19, col="deepskyblue") #change the color
plot(tommaso, erica, pch=19, col="deepskyblue", cex=2) #change point dimension
plot(tommaso, erica, pch=19, col="deepskyblue", cex=0.5)
plot(tommaso, erica, pch=19, col="deepskyblue", cex=2)

plot(tommaso, erica, pch=19, col="deepskyblue", cex=2, xlab="Cannabis sativa", ylab="dolphin")
plot(tommaso, erica, pch=19, col="deepskyblue", cex=2, xlab="Cannabis sativa", ylab="dolphins")
