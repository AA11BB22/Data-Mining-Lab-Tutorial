# Packages.
library(ISLR)
library(caTools)
library(ggplot2)
library(neuralnet)

# Data.
data(College)
df <- College

# Explore.
dim(df)
View(head(df))
str(df)
colSums(is.na(df))

boxplot(df)
summary(df)

qplot(x = as.factor(Private), data = df) + 
    geom_bar(aes(fill = as.factor(Private))) +
    xlab("Class Label") +
    guides(fill = guide_legend(title = "Private"))

# Scaled Data.
scaled.data <- data.frame(scale(df[, !names(df) %in% c("Private")]))
Private <- as.numeric(df$Private)-1
unique(Private) # Should only be 0 and 1.

df <- cbind(scaled.data, Private)

# Sampling.
set.seed(0)
split <- sample.split(df$Private, SplitRatio = 0.7)
train <- subset(df, split)
test <- subset(df, !split)

# Formula for NN.
names <- names(df) != "Private"
names <- names(df[0, names])

f <- paste(names, collapse = "+")
f <- paste("Private~", f)
f <- as.formula(f)

# NN.
nn <- neuralnet(f, data = train, hidden = c(10, 10, 10), linear.output = FALSE)
predict <- compute(nn, test[, !names(df) %in% c("Private")])
head(predict$net.result)

results <- round(predict$net.result, digits = 0)

# Confusion Table
table(test$Private, results)

# NN Plot.
plot(nn)