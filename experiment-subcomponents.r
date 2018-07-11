library(C50)
library(RWeka)
library(caret)
source('library/cross-validation.r')

s_dataset <- read.arff('changed/TRAINING-SUBCOMPONENTS.arff')
s_test_dataset <- read.arff('changed/TEST-SUBCOMPONENTS.arff')

s_columns <- names(s_dataset)
s_features <- s_columns[1:(length(s_columns)-1)]

ml_function <- function (X, y, X_cv) {
    model <- C5.0(X, y)
    return (predict(model, X_cv))
}

crossValidation(s_dataset, s_test_dataset, s_features, 'subelement', ml_function, 10)
