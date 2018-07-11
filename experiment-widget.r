library(C50)
library(RWeka)
library(caret)
source('library/cross-validation.r')

w_dataset <- read.arff('changed/TRAINING-WIDGET.arff')
w_test_dataset <- read.arff('changed/TEST-WIDGET.arff')

w_columns <- names(w_dataset)
w_features <- w_columns[1:(length(w_columns)-1)]

ml_function <- function (X, y, X_cv) {
    model <- C5.0(X, y)
    return (predict(model, X_cv))
}

crossValidation(w_dataset, w_test_dataset, w_features, 'widget', ml_function, 10)
