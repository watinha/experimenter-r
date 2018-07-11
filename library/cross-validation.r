library(caret)

printConfusion <- function (h, y) {
    confusion <- table(h, y)
    tp <- confusion[2,2]
    fp <- confusion[2,1]
    fn <- confusion[1,2]
    precision <- tp/(tp+fp)
    recall <- tp/(tp+fn)
    fMeasure <- 2 * (precision * recall) / (precision + recall)
    print(confusion)
    print(paste('Precision', precision))
    print(paste('Recall', recall))
    print(paste('F-measure',fMeasure))
}

crossValidation <- function (dataset, test_dataset, features, outputclass, ml_function, nfolds=5) {
    print('Training results')
    X <- dataset[,features]
    y <- dataset[,outputclass]
    h <- ml_function(X, y, X)
    printConfusion(h, y)

    print('Test results')
    X_test <- test_dataset[,features]
    y_test <- test_dataset[,outputclass]
    h <- ml_function(X, y, X_test)
    printConfusion(h, y_test)

    print('Cross-validation')
    folds <- createFolds(dataset[,outputclass], k=nfolds)
    results <- matrix(nrow=(1+nfolds), ncol=4)
    results[1,] <- c('true negative', 'true positive', 'FN', 'FP')
    precision <- 0
    recall <- 0
    for (i in 1:nfolds) {
        f <- dataset[folds[[i]],]
        train <- dataset[-folds[[i]],]
        X <- train[,features]
        y <- train[,outputclass]
        X_cv <- f[,features]
        y_cv <- f[,outputclass]
        h <- ml_function(X, y, X_cv)
        confusion <- table(h, y_cv)
        results[1+i, 1] <- confusion[1,1]
        results[1+i, 2] <- confusion[2,2]
        results[1+i, 3] <- confusion[1,2]
        results[1+i, 4] <- confusion[2,1]
        precision <- precision + (confusion[2,2]/(confusion[2,2] + confusion[2,1]))
        recall <- recall + (confusion[2,2]/(confusion[2,2] + confusion[1,2]))
    }
    precision <- precision/nfolds
    recall <- recall/nfolds
    print(results)
    print(paste('Precision', precision))
    print(paste('Recall', recall))
    print(paste('F-measure', (2 * precision * recall / (precision + recall))))
}
