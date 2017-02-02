library(e1071)

train <- data.frame(class=c("spam","ham","ham","ham"),
                    viagra=c("yes","no","no","yes"))

classifier <- naiveBayes

test <- data.frame(viagra=c("yes"))
test$viagra <- factor(test$viagra, levels=c("no","yes"))

prediction <- predict(classifier, test, type="raw")

train <- data.frame(type=c("spam","ham","ham","ham"), 
                    viagra=c("yes","no","no","yes"),
                    meet=c("yes","yes","yes", "no"))

classifier <- naiveBayes(type ~ viagra + meet, train)

test <- data.frame(viagra=c("yes"), meet=c("yes"))
test$viagra <- factor(test$viagra, levels=c("no","yes"))
test$meet <- factor(test$meet, levels=c("no","yes"))

prediction <- predict(classifier, test, type="raw")