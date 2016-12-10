# Exercise
if (!file.exists("dirty_iris.csv")) {
    url <- "https://raw.github.com/edwindj/datacleaning/master/data/dirty_iris.csv"
    download.file(url, destfile = "dirty_iris.csv", mode='wb')
}
data <- read.csv("dirty_iris.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Percentage of complete rows?
count <- 0
for (i in 1:nrow(data)) {
    if (any(is.na(data[i, ]))) count <- count + 1
}
complete.rows <- 100 - count / nrow(data) * 100
paste("Complete:", as.character(complete.rows), "%")
remove(count)
remove(i)

# Does the data contain special values? Replace them with NAs
data[apply(data[, ], 2, function(x) {is.infinite(x)})] <- NA

# Edit rules
library(editrules)
E <- editfile("IrisRules.txt")
VE <- violatedEdits(E, data) 
summary(VE)
plot(VE)

# Prints the rows which have violated the rules
LE <- localizeErrors(E, data, method = "mip") 
rows.in.violation <- data[apply(LE$adapt, 1, function(x) {if (sum(x) > 0) TRUE else FALSE}), ]

# What percentage of the data has no errors?
paste("No error:",as.character((nrow(data) - nrow(rows.in.violation)) / nrow(data) * 100), "%")

# Find out which observations have too long petals using the result of violatedEdits.
rules <- readLines("IrisRules.txt")
E <- editset(rules[grep("Sepal.Length > Petal.Length", rules, fixed = TRUE)])
LE <- localizeErrors(E, data, method = "mip")
data[apply(LE$adapt, 1, function(x) {if (sum(x) > 0) TRUE else FALSE}), ]

# Find outliers in sepal length using boxplot and boxplot.stats.
boxplot(x = data$Petal.Length)
outlier.values <- boxplot.stats(x = data$Petal.Length)$out
data[which(data$Petal.Length %in% outlier.values), ]

#     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
# 28          73.0        29.0           63          NA  virginica
# 35           6.6         2.9           23         1.3 versicolor
# 125         49.0        30.0           14         2.0     setosa

# 28, the measurements in all columns is may not be in cm or wrong floating point
# 35, the measurement for Petal.Length may be missing floating point
# 125, same reason as 28 except Petal.Width

drop <- c("Species")
data[28, !(names(data) %in% drop)] <- data[28, !(names(data) %in% drop)]/10
data[35, "Petal.Length"] <- data[35, "Petal.Length"]/10
drop <- c(drop, "Petal.Width")
data[125, !(names(data) %in% drop)] <- data[125, !(names(data) %in% drop)]/10

remove(drop)

# View the new result
boxplot(x = data$Petal.Length)


# Correcting
library(deducorrect)
E <- editmatrix("Petal.Width > 0")
correctWithRules(E, data)

LE <- localizeErrors(E, data)
data[apply(LE$adapt, 1, function(x) {if (sum(x) > 0) TRUE else FALSE}), "Petal.Width"] <- NA

remove(E)
remove(LE)
remove(rules)

# Imputing
library(VIM)
data1 <- kNN(data)$Petal.Width
data2 <- hotdeck(data, ord_var = "Species")$Petal.Width
data3 <- hotdeck(data, ord_var = c("Species", "Sepal.Length"))$Petal.Width

par(mfrow = c(1, 3))
hist(data1, ylim = c(0, 35), main = "KNN", xlab = "Petal Width")
hist(data2, ylim = c(0, 35), main = "Hotdeck, sorted by Species", xlab = "Petal Width")
hist(data3, ylim = c(0, 35), main = "Hotdeck, sorted by Species then Sepal.Length", xlab = "Petal Width")
