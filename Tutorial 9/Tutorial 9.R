library(ISLR)
library(tree)
library(ggplot2)

High <- with(Carseats, ifelse(Sales <= 8, "No", "Yes"))

# Append "High" to Carseats.
Carseats <- data.frame(Carseats, High)

# Answer.
table(Carseats$High)
qplot(Carseats$High) + geom_bar(aes(fill = Carseats$High))

# Decision Tree, predict whether a observation for High, manually exclude the "Sales" variable.
tree.carseats <- tree::tree(High ~ . -Sales, Carseats)
summary(tree.carseats)

# Plot the tree.
plot(tree.carseats)
text(tree.carseats, pretty = 0, cex = 0.6)

# Display features used / unused.
variables.used <- summary(tree.carseats)$used
variables.unused <- names(Carseats)[which(!(names(Carseats) %in% variables.used))]

# Answer, 0.09 (36 out of 400)

# Splitting to test and training.
# 1. Fixed the seed for reproducibility.
set.seed(2)

# 200 observations as training.
train <- sample(1:nrow(Carseats), 200)

# Other 200 observations as test.
Carseats.test <- Carseats[-train,]

# Class for each observation in the test set. (For testing model purpose)
High.test <- High[-train]

# Decision Tree using training.
tree.carseats <- tree(High ~ . -Sales,Carseats, subset = train)

# Test the model.
tree.pred <- predict(tree.carseats, Carseats.test, type = "class")

# Confusion Table
table(tree.pred, High.test)

# Pruning
cv.carseats <- cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)
cv.carseats

# Index of tree with minimum error
min.idx <- which.min(cv.carseats$dev)
min.idx

# Number of leaves in that tree
cv.carseats$size[min.idx]

# Number of misclassifications (this is a count)
cv.carseats$dev[min.idx]


par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")


par(mfrow = c(1, 1))
prune.carseats <- prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0, cex = 0.6)

# Variables used
summary(prune.carseats)$used


# Variables that are used in one model but not the other (Compare current and previous)
c(setdiff(summary(prune.carseats)$used, summary(tree.carseats)$used),
  setdiff(summary(tree.carseats)$used, summary(prune.carseats)$used))

tree.pred <- predict(tree.carseats, Carseats.test, type="class")
tree.confusion.pred <- table(tree.pred, High.test)

prune.pred <- predict(prune.carseats, Carseats.test, type="class")
prune.confusion.pred <- table(prune.pred, High.test)

# Error
1 - sum(diag(tree.confusion.pred)) / sum(tree.confusion.pred)
1 - sum(diag(prune.confusion.pred)) / sum(prune.confusion.pred)