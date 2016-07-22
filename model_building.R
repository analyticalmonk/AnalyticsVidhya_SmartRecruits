library(xgboost)
library(Matrix)
library(caret)
library(ROCR)

set.seed(1234)

train <- read.csv(file = "Train_pjb2QcD.csv", stringsAsFactors = F)
test <- read.csv(file = "Test_wyCirpO.csv", stringsAsFactors = F)

test.id <- test$ID
train.id <- train$ID
train.y <- train$Business_Sourced

## FEATURE ENGINEERING
train <- train[,c(13, 17:23)]
test <- test[,c(13, 17:22)]

train$Manager_Grade[!complete.cases(train)] <- median(train$Manager_Grade, na.rm = T)
train$Manager_Num_Application[!complete.cases(train)] <- 2.00
train$Manager_Num_Coded[!complete.cases(train)] <- mean(train$Manager_Num_Coded, na.rm = T)
train$Manager_Business[!complete.cases(train)] <- mean(train$Manager_Business, na.rm = T)
train$Manager_Num_Products[!complete.cases(train)] <- mean(train$Manager_Num_Products, 
                                                             na.rm = T)
train$Manager_Business2[!complete.cases(train)] <- median(train$Manager_Business2, 
                                                          na.rm = T)
train$Manager_Num_Products2[!complete.cases(train)] <- median(train$Manager_Num_Products2, 
                                                          na.rm = T)

test$Manager_Grade[!complete.cases(test)] <- median(test$Manager_Grade, na.rm = T)
test$Manager_Num_Application[!complete.cases(test)] <- 2.00
test$Manager_Num_Coded[!complete.cases(test)] <- mean(test$Manager_Num_Coded, na.rm = T)
test$Manager_Business[!complete.cases(test)] <- mean(test$Manager_Business, na.rm = T)
test$Manager_Num_Products[!complete.cases(test)] <- mean(test$Manager_Num_Products, 
                                                           na.rm = T)
test$Manager_Business2[!complete.cases(test)] <- median(test$Manager_Business2, 
                                                          na.rm = T)
test$Manager_Num_Products2[!complete.cases(test)] <- median(test$Manager_Num_Products2, 
                                                              na.rm = T)

######################

## BUILD MODEL WITH STRATIFIED K-FOLD CV
folds <- createFolds(as.factor(train$Business_Sourced), k = 5)
fold_auc <- c()

for (fold in folds) {
    x_train <- train[-fold, ]
    x_train.y <- train$Business_Sourced[-fold]
    x_test <- train[fold, ]
    x_test.y <- train$Business_Sourced[fold]
    
    x_train <- sparse.model.matrix(Business_Sourced ~ .-1, data= x_train) 
    x_test <- sparse.model.matrix(Business_Sourced ~ .-1, data = x_test)
    
    d_train <- xgb.DMatrix(data = x_train, label = x_train.y)
    d_test <- xgb.DMatrix(data = x_test, label = x_test.y)
    watchlist <- list(train=d_train, test=d_test)
    
    param <- list( objective    = "binary:logistic",
                   booster      = "gbtree",
                   eval_metric  = "auc",
                   eta          = 0.02,
                   max_depth    = 5
                   )
    
    clf <- xgb.train(   params              = param, 
                        data                = d_train, 
                        nrounds             = 150, 
                        verbose             = 2,
                        watchlist           = watchlist,
                        maximize            = FALSE
                        )
    
    fold_pred <- predict(clf, x_test)
    pred <- prediction(fold_pred, x_test.y)
    auc <- performance(pred, measure = "auc")
    fold_auc <- c(fold_auc, auc@y.values[[1]])
}


print(fold_auc)
########################################

train <- sparse.model.matrix(Business_sourced ~ ., data = train)
dtrain <- xgb.DMatrix(data=train, label=train.y)
watchlist <- list(train=dtrain)
# 
param <- list( objective    = "binary:logistic",
               booster      = "gbtree",
               eval_metric  = "auc",
               eta          = 0.02,
               max_depth    = 5
)

clf <- xgb.train(   params              = param, 
                    data                = d_train, 
                    nrounds             = 150, 
                    verbose             = 2,
                    watchlist           = watchlist,
                    maximize            = FALSE
)

test$target <- -1
test <- sparse.model.matrix(target ~ ., data = test)

preds <- predict(clf, test)
submission <- data.frame(ID=test.id, Business_Sourced=preds)
cat("saving the submission file\n")
write.csv(submission, "Submissions/submission1.csv", row.names = F)

