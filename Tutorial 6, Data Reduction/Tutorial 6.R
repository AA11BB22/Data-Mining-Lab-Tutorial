library(caret)

# Load Data
data(crimtab)
df <- crimtab

# Check Data
print(dim(df))
str(df)
summary(df)

# Near Zero Variance of Data
nzv <- nearZeroVar(df, saveMetrics = TRUE)
df_nzv <- df[, !nzv$nzv]

# For Evaluation
df_eval <- cbind(as.data.frame(sapply(df_nzv, as.numeric)), cluster=df[,"147.32"])

EvaluateAUC <- function(dfEvaluate) {
    require(xgboost)
    require(Metrics)
    CVs <- 5
    cvDivider <- floor(nrow(dfEvaluate) / (CVs+1))
    indexCount <- 1
    outcomeName <- c('cluster')
    predictors <- names(dfEvaluate)[!names(dfEvaluate) %in% outcomeName]
    lsErr <- c()
    lsAUC <- c()
    for (cv in seq(1:CVs)) {
        print(paste('cv',cv))
        dataTestIndex <- c((cv * cvDivider):(cv * cvDivider + cvDivider))
        dataTest <- dfEvaluate[dataTestIndex,]
        dataTrain <- dfEvaluate[-dataTestIndex,]
        
        bst <- xgboost(data = as.matrix(dataTrain[,predictors]),
                       label = dataTrain[,outcomeName],
                       max.depth=6, eta = 1, verbose=0,
                       nround=5, nthread=4, 
                       objective = "reg:linear")
        
        predictions <- predict(bst, as.matrix(dataTest[,predictors]), outputmargin=TRUE)
        err <- rmse(dataTest[,outcomeName], predictions)
        auc <- auc(dataTest[,outcomeName],predictions)
        
        lsErr <- c(lsErr, err)
        lsAUC <- c(lsAUC, auc)
        gc()
    }
    print(paste('Mean Error:',mean(lsErr)))
    print(paste('Mean AUC:',mean(lsAUC)))
}

EvaluateAUC(df_eval)

# PCA
df_scaled <- scale(df_nzv)
pca <- prcomp(df_scaled)

pca_comp <- predict(pca, newdata=df_scaled)[, 1:2]
df_eval2 <- cbind(as.data.frame(pca_comp), cluster=df[,"147.32"])

EvaluateAUC(df_eval2)