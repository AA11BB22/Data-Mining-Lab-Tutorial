library(caret)

data(iris)
summary(iris[,1:4])

# Scale, divides each column by its standard deviation.
preprocessParams <- preProcess(iris[,1:4], method = c("scale"))
transformed <- predict(preprocessParams, iris[,1:4])
summary(transformed)


# Center, subtracts each column by its mean.
preprocessParams <- preProcess(iris[,1:4], method = c("center"))
transformed <- predict(preprocessParams, iris[,1:4])
summary(transformed)


# Standardize, Center + Scale
preprocessParams <- preProcess(iris[,1:4], method = c("center", "scale"))
transformed <- predict(preprocessParams, iris[,1:4])
summary(transformed)


# Normalize, (data - min)/(max - min)
preprocessParams <- preProcess(iris[,1:4], method = c("range"))
transformed <- predict(preprocessParams, iris[,1:4])
summary(transformed)


library(mlbench)
data(PimaIndiansDiabetes)
summary(PimaIndiansDiabetes[,7:8])


# BoxCox, reduces skewness (data must be positive)
preprocessParams <- preProcess(PimaIndiansDiabetes[,7:8], method = c("BoxCox"))
transformed <- predict(preprocessParams, PimaIndiansDiabetes[,7:8])
summary(transformed)


# YeoJohnson, reduces skewness
preprocessParams <- preProcess(PimaIndiansDiabetes[,7:8], method = c("YeoJohnson"))
transformed <- predict(preprocessParams, PimaIndiansDiabetes[,7:8])
summary(transformed)


# PCA
preprocessParams <- preProcess(iris, method = c("center", "scale", "pca"))
transformed <- predict(preprocessParams, iris)
summary(transformed)


# ICA
library(fastICA)
preprocessParams <- preProcess(PimaIndiansDiabetes[,1:8], method = c("center", "scale", "ica"), n.comp=5)
transformed <- predict(preprocessParams, PimaIndiansDiabetes[,1:8])
summary(transformed)