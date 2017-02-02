df <- read.csv("data_Lab5.csv")

str(df)
dim(df)
any(is.na(df))
duplicated(df)

df.washers <- df[,2:7]
dist(df.washers,method="euclidean")
dist(df.washers,method="manhattan")

kmeansFit <- kmeans(df.washers,4)
attributes(kmeansFit)
kmeansFit$centers
kmeansFit$cluster

# Number of Clusters Selection Plot.
wssplot <- function(data, nc=15, seed=1234) {
    wss <- (nrow(data)-1)*sum(apply(data,2,var))
    
    for (i in 2:nc) {
        set.seed(seed)
        wss[i] <- sum(kmeans(data, centers=i)$withinss)
    }
    
    plot(1:nc, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
}

wssplot(df.washers, 5)


# Alternative
library(cluster)
clusplot(df.washers,
         kmeansFit$cluster,
         main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,labels=2, lines=0)