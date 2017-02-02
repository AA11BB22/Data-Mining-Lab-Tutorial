set.seed(1234)

# Data Generation
x<-rnorm(12,mean=rep(1:3,each=4),sd=0.2)
y<-rnorm(12,mean=rep(c(1,2,1),each=4),sd=0.2)

plot(x,y,col="blue",pch=19,cex=2)
text(x,y,labels=as.character(1:12),col="cyan")

df<-data.frame(x,y)

# K-Means Clustering
k <- 3

k.cluster<-kmeans(df,centers=k)
k.cluster$cluster
k.cluster$centers

plot(x,y,col=k.cluster$cluster,pch=19,cex=2)
points(k.cluster$centers,col=1:k,pch=3,cex=3,lwd=3)
text(x,y,labels=as.character(1:12),col="cyan")

# Hierarchical Clustering
distxy<-dist(df)

clusters<-hclust(distxy)
plot(clusters)