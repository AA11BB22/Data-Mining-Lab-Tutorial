food <- read.csv("protein.csv")

str(food)

grpMeat <- kmeans(food[,c("WhiteMeat","RedMeat")],centers=3,nstart=10)

plot(food$WhiteMeat,food$RedMeat,col=grpMeat$cluster,pch=19,cex=2)
points(grpMeat$centers,col='blue',pch=3,cex=2, lwd=4)

o <- order(grpMeat$cluster)
data.frame(food$Country[o],grpMeat$cluster[o])

par(mar=c(5,5,2,2))
plot(food$RedMeat, food$WhiteMeat, type="n", xlim=c(3,19), xlab="Red Meat", ylab="White Meat")
text(x=food$Red, y=food$White, labels=food$Country,col=grpMeat$cluster+1)