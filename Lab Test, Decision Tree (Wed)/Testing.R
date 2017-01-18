library(ISLR)
library(tree)
library(scatterplot3d)
library(lattice)
library(ggplot2)

df <- read.csv("data.csv")

str(df)
unique(df$X)

# Drop X since it's all NAs.
df <- df[, !names(df) %in% "X"]

# Drop data that are not means, since mean is the "average"
# Better than se, worst ... for being the representative.
df <- df[, 1:12]

# All columns has no NAs.
colSums(is.na(df))

# Distribution of labels / classes.
table(df$diagnosis)

# Exploration.
df$pcolor[df$diagnosis == "M"] = "magenta"
df$pcolor[df$diagnosis == "B"] = "blue"

# Finding which feature gives the best split.
with(df, xyplot(area_mean ~ diagnosis, group = diagnosis, alpha = 0.2, pch = 18)) # Bad
with(df, xyplot(radius_mean ~ diagnosis, group = diagnosis, alpha = 0.2, pch = 18)) # Bad
with(df, xyplot(texture_mean ~ diagnosis, group = diagnosis, alpha = 0.2, pch = 18)) # Very bad
with(df, xyplot(perimeter_mean ~ diagnosis, group = diagnosis, alpha = 0.2, pch = 18)) # Bad
with(df, xyplot(smoothness_mean ~ diagnosis, group = diagnosis, alpha = 0.2, pch = 18)) # Bad
with(df, xyplot(compactness_mean ~ diagnosis, group = diagnosis, alpha = 0.2, pch = 18)) # Bad
with(df, xyplot(concavity_mean ~ diagnosis, group = diagnosis, alpha = 0.2, pch = 18)) # Bad
with(df, xyplot(concave.points_mean ~ diagnosis, group = diagnosis, alpha = 0.2, pch = 18)) # Decent
with(df, xyplot(symmetry_mean ~ diagnosis, group = diagnosis, alpha = 0.2, pch = 18)) # Very Bad
with(df, xyplot(fractal_dimension_mean ~ diagnosis, group = diagnosis, alpha = 0.2, pch = 18)) # Very Bad

df <- df[, !names(df) %in% c("texture_mean", "symmetry_mean", "fractal_dimension_mean")]

# Try 2 features.
qplot(radius_mean, perimeter_mean, colour = diagnosis, data = df, alpha = 0.1) # Decent
qplot(radius_mean, area_mean, colour = diagnosis, data = df, alpha = 0.1) # Decent
qplot(perimeter_mean, area_mean, colour = diagnosis, data = df, alpha = 0.1) # Decent
qplot(radius_mean, smoothness_mean, colour = diagnosis, data = df, alpha = 0.1) # Decent
qplot(compactness_mean, smoothness_mean, colour = diagnosis, data = df, alpha = 0.1) # Bad
qplot(compactness_mean, concavity_mean, colour = diagnosis, data = df, alpha = 0.1) # Some really off points for M
qplot(smoothness_mean, concavity_mean, colour = diagnosis, data = df, alpha = 0.1) # Once again, off points for M

df = df[, !names(df) %in% "concavity_mean"]

qplot(smoothness_mean, compactness_mean, colour = diagnosis, data = df, alpha = 0.1) # Very Bad
qplot(radius_mean, compactness_mean, colour = diagnosis, data = df, alpha = 0.1) # Decent
qplot(radius_mean, smoothness_mean, colour = diagnosis, data = df, alpha = 0.1) # Decent


# Try 3 features.
with(df, scatterplot3d(radius_mean,
                       compactness_mean,
                       smoothness_mean,
                       color = alpha(pcolor, 0.2),
                       pch = 19
                       )
     )

with(df, scatterplot3d(radius_mean,
                       area_mean,
                       concave.points_mean,
                       color = alpha(pcolor, 0.2),
                       pch = 19
                       )
) # Looks pretty pure.


# Main features, radius_mean, area_mean, concave.points_mean
set.seed(0)

df <- read.csv("data.csv")
df <- df[, 1:ncol(df)-1]

train <- sample(1:nrow(df), 380)
df.test <- df[-train, ]

# Tree auto
tree.all <- tree(diagnosis~.-diagnosis-id, data=df, subset=train)
plot(tree.all)
text(tree.all, pretty=0, cex=0.6)

# Test the tree
tree.all.pred <- predict(tree.all, df.test, type="class")

# Confusion Table
table(tree.all.pred, df.test[, "diagnosis"])


# Prune
prune.tree <- prune.misclass(tree.all, best=7)
plot(prune.tree)
text(prune.tree, pretty=0, cex=0.6)

prune.tree.pred <- predict(prune.tree, df.test, type="class")

# Confusion Table
table(prune.tree.pred, df.test[, "diagnosis"])


# Custom Tree (Mean only)
df <- df[, 1:12]
tree.mean <- tree(diagnosis~.+concave.points_mean
                  -diagnosis-texture_mean-symmetry_mean
                  -fractal_dimension_mean-id-concavity_mean, 
                  data=df, subset=train)
plot(tree.mean)
text(tree.mean, pretty=0, cex=0.6)

# Test the tree
tree.mean.pred <- predict(tree.mean, df.test, type="class")

# Confusion Table
table(tree.mean.pred, df.test[, "diagnosis"])