load("titanic.raw.rdata")

str(titanic.raw)
head(titanic.raw)

# install.packages("arules")
require(arules)
rules.all <- apriori(titanic.raw)
rules.all
inspect(rules.all)


rules<-apriori(titanic.raw, control=list(verbose=F), # Don't show the steps
               parameter=list(minlen=2,supp=0.005,conf=0.8), # Restriction, 2 items, min sup 0.5%, min conf 80%
               appearance = list(rhs=c("Survived=No",
                                       "Survived=Yes"),
                                 default="lhs"))

quality(rules)<-round(quality(rules),digits=3) # Reduced the decimal points to 3 digits
rules.sorted <- sort(rules,by="lift") # Sort by lift
inspect(rules.sorted)

# Find redundant rules.
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

# Remove redundant rules.
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)


# install.packages("arulesViz")
require(arulesViz)
plot(rules.pruned)
plot(rules.pruned, method="grouped")
plot(rules.pruned, method="graph")

