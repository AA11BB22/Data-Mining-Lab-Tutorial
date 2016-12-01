# Load data into memory
source("http://www.openintro.org/stat/data/present.R")
source("http://www.openintro.org/stat/data/arbuthnot.R")

# 1. What years are included in this data set?
#    What are the dimensions of the data frame and 
#    what are the variable or column names?

unique(present$year)
dim(present)
colnames(present)


# 2. How do these counts compare to Arbuthnot’s? Are they on a similar scale?

library(gridExtra)
library(ggplot2)
plot1 <- ggplot() + 
        geom_line(data = present, aes(x = year, y = boys, color = "Boy")) +
        geom_line(data = present, aes(x = year, y = girls, color = "Girl")) +
        ylab("") +
        xlab("Year") +
        aes(title = "Present")
        

plot2 <- ggplot() + 
    geom_line(data = arbuthnot, aes(x = year, y = boys, color = "Boy")) +
    geom_line(data = arbuthnot, aes(x = year, y = girls, color = "Girl")) +
    ylab("Frequency") +
    xlab("Year") +
    aes(title = "Arbuthnot") +
    guides(color = FALSE)

grid.arrange(plot2, plot1, ncol = 2)

# The number of births in 'present' are a lot higher than 'arbuthnot', however
# both plots show that the number of boys is always higher than the girls.


# 3. Make a plot that displays the boy-to-girl ratio for every year in the data set.
#    What do you see? Does Arbuthnot’s observation about boys being born in greater proportion than girls hold up in the U.S.?
#    Include the plot in your response.

present$boyToGirlRatio <- present$boys/present$girls
arbuthnot$boyToGirlRatio <- arbuthnot$boys/arbuthnot$girls

par(mfrow = c(1, 2))
plot(x = arbuthnot$year, y = arbuthnot$boyToGirlRatio, type = "l", ylim = c(1, 1.2), ylab = "", xlab = "")
title("Arbuthnot")
plot(x = present$year, y = present$boyToGirlRatio, type = "l", ylim = c(1, 1.2), ylab = "", xlab = "")
title("Present")

# Arbuthnot's observation holds, however in Arbuthnot's dataset, the signal flunctuates.
# In Present's dataset, the signal can be seen decreasing linearly.


# 4. In what year did we see the most total number of births in the U.S.?

getIndexOfMaxBirth <- function() {
    
    a <- 1; max <- present[a, "boys"] + present[a, "girls"]
    
    for (i in 2:nrow(present)) {
        if (present[i, "boys"] + present[i, "girls"] > max) {
            max <- present[i, "boys"] + present[i, "girls"]
            a <- i
        }
    }
    return(a)
}

maxIndex <- getIndexOfMaxBirth()
presentMaxX <- present[maxIndex, "year"]
presentMaxY <- present[maxIndex, "boys"] + present[maxIndex, "girls"]

par(mfrow = c(1, 1))
plot(x = present$year, y = (present$boys + present$girls), type = "l", xlab = "Year", ylab = "Total Births")
points(x = presentMaxX, y = presentMaxY, pch = 19, col = "red")
abline(v = presentMaxX, col = "blue", lty = "dashed")
abline(h = presentMaxY, col = "blue", lty = "dashed")