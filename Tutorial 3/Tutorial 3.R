# READ THE person.txt using read.csv as the object called person
person <- read.csv( file = "files/unnamed.txt" , header = FALSE , col.names = c("age","height") )
person
persons_complete <- na.omit(person)
persons_complete$height <- gsub("[^0-9]", "", as.character(persons_complete$height))
persons_complete$height <- as.numeric(persons_complete$height)


# Special values
is.finite(c(1, Inf, NaN, NA))
is.special <- function(x){ if (is.numeric(x)) !is.finite(x) else is.na(x) }
sapply(person, is.special)


# Editrules
library(editrules)
people <- read.csv("files/people.txt") 
people
(E <- editset(c("age >=0", "age <= 150")))
violatedEdits(E, people)
E <- editfile("files/edits.txt")
ve <- violatedEdits(E, people) 
summary(ve)
plot(ve)

id <- c(2, 5) 
people[id, ]
le <- localizeErrors(E, people[id, ], method = "mip") 
le$adapt
people[2, "status"] <- "single" 
people[5, "height"] <- 7 
people[5, "agegroup"] <- "adult"
summary(violatedEdits(E, people[id, ]))


# Deducorrect
library(deducorrect)
e <- editmatrix("x + y == z")
d <- data.frame(x = 100, y = 101, z = 200) 
cor <- correctRounding(e, d) 
cor$corrected 
cor$corrections 


# Hmisc
library(Hmisc)
x <- 1:5 # create a vector...
x[2] <- NA # ...with an empty value
x <- impute(x, mean)
x
is.imputed(x)


# Linear Regression for missing values
data(iris)
iris$Sepal.Length[1:10] <- NA
model <- lm(Sepal.Length ~ Sepal.Width + Petal.Width, data = iris)
I <- is.na(iris$Sepal.Length)
iris$Sepal.Length[I] <- predict(model, newdata = iris[I, ])


# kNN
library(VIM)
data(iris)
n <- nrow(iris)
# provide some empty values (10 in each column, randomly)
for (i in 1:ncol(iris)) {
    iris[sample(1:n, 10, replace = FALSE), i] <- NA
}
iris
iris2 <- kNN(iris)
any(is.na(iris2))