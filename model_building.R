library(xgboost)
library(Matrix)
library(caret)
library(ROCR)
library(tidyr)

set.seed(1234)

train <- read.csv(file = "Train_pjb2QcD.csv", stringsAsFactors = F)
test <- read.csv(file = "Test_wyCirpO.csv", stringsAsFactors = F)

test.id <- test$ID
train.id <- train$ID
train.y <- train$Business_Sourced

#########################################################################################
## FEATURE ENGINEERING
## -------------------
## -------------------
train <- train[,-c(1, 4)]
test <- test[,-c(1, 4)]

train$Business_Sourced <- NULL

# Separating Application_Receipt_Date into date, month, year
# ----------------------------------------------------------
train <- separate(data = train, col = Application_Receipt_Date, 
         into = c("Receipt_month", "Receipt_date", "Receipt_year"))
train$Receipt_date <- as.numeric(train$Receipt_date)
train$Receipt_month <- as.numeric(train$Receipt_month)
train$Receipt_year <- as.numeric(train$Receipt_year)

test <- separate(data = test, col = Application_Receipt_Date, 
                  into = c("Receipt_month", "Receipt_date", "Receipt_year"))
test$Receipt_date <- as.numeric(test$Receipt_date)
test$Receipt_month <- as.numeric(test$Receipt_month)
test$Receipt_year <- as.numeric(test$Receipt_year)

# Separating Applicant_BirthDate into date, month, year
# ----------------------------------------------------------
train <- separate(data = train, col = Applicant_BirthDate, 
                  into = c("Applicant_Birth_month", "Applicant_Birth_date", "Applicant_Birth_year"))
# train$Applicant_Birth_date <- as.numeric(train$Applicant_Birth_date)
# train$Applicant_Birth_date[is.na(train$Applicant_Birth_date)] <- 
#     as.numeric(names(which.max((table(train$Applicant_Birth_date)))))
# 
# train$Applicant_Birth_month <- as.numeric(train$Applicant_Birth_month)
# train$Applicant_Birth_month[is.na(train$Applicant_Birth_month)] <- 
#     as.numeric(names(which.max((table(train$Applicant_Birth_month)))))
train$Applicant_Birth_date <- NULL
train$Applicant_Birth_month <- NULL
train$Applicant_Birth_year <- as.numeric(train$Applicant_Birth_year)
train$Applicant_Birth_year[is.na(train$Applicant_Birth_year)] <- 
    as.numeric(names(which.max((table(train$Applicant_Birth_year)))))

test <- separate(data = test, col = Applicant_BirthDate, 
                 into = c("Applicant_Birth_month", "Applicant_Birth_date", "Applicant_Birth_year"))
# test$Applicant_Birth_date <- as.numeric(test$Applicant_Birth_date)
# test$Applicant_Birth_date[is.na(test$Applicant_Birth_date)] <- 
#     as.numeric(names(which.max((table(test$Applicant_Birth_date)))))
# 
# test$Applicant_Birth_month <- as.numeric(test$Applicant_Birth_month)
# test$Applicant_Birth_month[is.na(test$Applicant_Birth_month)] <- 
#     as.numeric(names(which.max((table(test$Applicant_Birth_month)))))
test$Applicant_Birth_date <- NULL
test$Applicant_Birth_month <- NULL
test$Applicant_Birth_year <- as.numeric(test$Applicant_Birth_year)
test$Applicant_Birth_year[is.na(test$Applicant_Birth_year)] <- 
    as.numeric(names(which.max((table(test$Applicant_Birth_year)))))

# Create Applicant_age variable
# ---------------------
train$Applicant_Age <- (2015 - train$Applicant_Birth_year)
train$Applicant_Birth_year <- NULL
test$Applicant_Age <- (2015 - test$Applicant_Birth_year)
test$Applicant_Birth_year <- NULL

# Separating Manager_DOJ into date, month, year
# ----------------------------------------------------------
train <- separate(data = train, col = Manager_DOJ, 
                  into = c("Manager_Join_month", "Manager_Join_date", "Manager_Join_year"))
train$Manager_Join_date <- as.numeric(train$Manager_Join_date)
train$Manager_Join_date[is.na(train$Manager_Join_date)] <- 
    median(train$Manager_Join_date, na.rm = T)

train$Manager_Join_month <- as.numeric(train$Manager_Join_month)
train$Manager_Join_month[is.na(train$Manager_Join_month)] <- 
    median(train$Manager_Join_month, na.rm = T)

# train$Manager_Join_date <- NULL
# train$Manager_Join_month <- NULL
train$Manager_Join_year <- as.numeric(train$Manager_Join_year)
train$Manager_Join_year[is.na(train$Manager_Join_year)] <- 
    median(train$Manager_Join_year, na.rm = T)

test <- separate(data = test, col = Manager_DOJ, 
                 into = c("Manager_Join_month", "Manager_Join_date", "Manager_Join_year"))
test$Manager_Join_date <- as.numeric(test$Manager_Join_date)
test$Manager_Join_date[is.na(test$Manager_Join_date)] <- 
    median(test$Manager_Join_date, na.rm = T)

test$Manager_Join_month <- as.numeric(test$Manager_Join_month)
test$Manager_Join_month[is.na(test$Manager_Join_month)] <- 
    median(test$Manager_Join_month, na.rm = T)

# test$Manager_Join_date <- NULL
# test$Manager_Join_month <- NULL
test$Manager_Join_year <- as.numeric(test$Manager_Join_year)
test$Manager_Join_year[is.na(test$Manager_Join_year)] <- 
    median(test$Manager_Join_year, na.rm = T)

# Separating Manager_DoB into date, month, year
# ----------------------------------------------------------
train <- separate(data = train, col = Manager_DoB, 
                  into = c("Manager_Birth_month", "Manager_Birth_date", "Manager_Birth_year"))
# train$Manager_Birth_date <- as.numeric(train$Manager_Birth_date)
# train$Manager_Birth_date[is.na(train$Manager_Birth_date)] <- 
#     median(train$Manager_Birth_date, na.rm = T)
# 
# train$Manager_Birth_month <- as.numeric(train$Manager_Birth_month)
# train$Manager_Birth_month[is.na(train$Manager_Birth_month)] <- 
#     median(train$Manager_Birth_month, na.rm = T)

train$Manager_Birth_date <- NULL
train$Manager_Birth_month <- NULL
train$Manager_Birth_year <- as.numeric(train$Manager_Birth_year)
train$Manager_Birth_year[is.na(train$Manager_Birth_year)] <- 
    median(train$Manager_Birth_year, na.rm = T)

test <- separate(data = test, col = Manager_DoB, 
                 into = c("Manager_Birth_month", "Manager_Birth_date", "Manager_Birth_year"))
# test$Manager_Birth_date <- as.numeric(test$Manager_Birth_date)
# test$Manager_Birth_date[is.na(test$Manager_Birth_date)] <- 
#     median(test$Manager_Birth_date, na.rm = T)
# 
# test$Manager_Birth_month <- as.numeric(test$Manager_Birth_month)
# test$Manager_Birth_month[is.na(test$Manager_Birth_month)] <- 
#     median(test$Manager_Birth_month, na.rm = T)

test$Manager_Birth_date <- NULL
test$Manager_Birth_month <- NULL
test$Manager_Birth_year <- as.numeric(test$Manager_Birth_year)
test$Manager_Birth_year[is.na(test$Manager_Birth_year)] <- 
    median(test$Manager_Birth_year, na.rm = T)

# Create Manager_Age variable
# ---------------------
train$Manager_Age <- (2015 - train$Manager_Birth_year)
train$Manager_Birth_year <- NULL
test$Manager_Age <- (2015 - test$Manager_Birth_year)
test$Manager_Birth_year <- NULL

# Encoding Applicant_Gender
# ----------------------------------------------------------
train$Applicant_Gender[train$Applicant_Gender == "F"] <- 1
train$Applicant_Gender[train$Applicant_Gender == "M"] <- 2
train$Applicant_Gender[train$Applicant_Gender == ""] <- 3
train$Applicant_Gender <- as.numeric(train$Applicant_Gender)

test$Applicant_Gender[test$Applicant_Gender == "F"] <- 1
test$Applicant_Gender[test$Applicant_Gender == "M"] <- 2
test$Applicant_Gender[test$Applicant_Gender == ""] <- 3
test$Applicant_Gender <- as.numeric(test$Applicant_Gender)

# train$Female <- as.numeric(train$Applicant_Gender != "M")
# train$Male <- as.numeric(!train$Female)
# test$Female <- as.numeric(test$Applicant_Gender != "M")
# test$Male <- as.numeric(!test$Female)
# 
# train$Applicant_Gender <- NULL
# test$Applicant_Gender <- NULL

# Encoding Applicant_Marital_Status
# ----------------------------------------------------------
train$Applicant_Marital_Status[train$Applicant_Marital_Status == "M"] <- 1
train$Applicant_Marital_Status[train$Applicant_Marital_Status == "S"] <- 2
train$Applicant_Marital_Status[train$Applicant_Marital_Status == "D"] <- 3
train$Applicant_Marital_Status[train$Applicant_Marital_Status == "W" |
                                   train$Applicant_Marital_Status == "D" |
                                   train$Applicant_Marital_Status == ""] <- 3
train$Applicant_Marital_Status <- as.numeric(train$Applicant_Marital_Status)

test$Applicant_Marital_Status[test$Applicant_Marital_Status == "M"] <- 1
test$Applicant_Marital_Status[test$Applicant_Marital_Status == "S"] <- 2
test$Applicant_Marital_Status[test$Applicant_Marital_Status == "D"] <- 3
test$Applicant_Marital_Status[test$Applicant_Marital_Status == "W" |
                                   test$Applicant_Marital_Status == "D" |
                                   test$Applicant_Marital_Status == ""] <- 3
test$Applicant_Marital_Status <- as.numeric(test$Applicant_Marital_Status)

# Encoding Applicant_Occupation
# ----------------------------------------------------------
train$Applicant_Occupation[train$Applicant_Occupation == "Salaried"] <- 1
train$Applicant_Occupation[train$Applicant_Occupation == "Business"] <- 2
train$Applicant_Occupation[train$Applicant_Occupation == "Others" |
                               train$Applicant_Occupation == ""] <- 3
train$Applicant_Occupation[train$Applicant_Occupation == "Self Employed" |
                               train$Applicant_Occupation == "Student"] <- 4
train$Applicant_Occupation <- as.numeric(train$Applicant_Occupation)

test$Applicant_Occupation[test$Applicant_Occupation == "Salaried"] <- 1
test$Applicant_Occupation[test$Applicant_Occupation == "Business"] <- 2
test$Applicant_Occupation[test$Applicant_Occupation == "Others" |
                               test$Applicant_Occupation == ""] <- 3
test$Applicant_Occupation[test$Applicant_Occupation == "Self Employed" |
                               test$Applicant_Occupation == "Student"] <- 4
test$Applicant_Occupation <- as.numeric(test$Applicant_Occupation)

# Encoding Applicant_Qualification
# ----------------------------------------------------------
train$Applicant_Qualification[train$Applicant_Qualification == "Class XII"] <- 1
train$Applicant_Qualification[train$Applicant_Qualification == "Graduate"] <- 2
train$Applicant_Qualification[train$Applicant_Qualification == "Class X"] <- 3
train$Applicant_Qualification[train$Applicant_Qualification != 1 &
                                  train$Applicant_Qualification != 2 &
                                  train$Applicant_Qualification != 3] <- 4
train$Applicant_Qualification <- as.numeric(train$Applicant_Qualification)

test$Applicant_Qualification[test$Applicant_Qualification == "Class XII"] <- 1
test$Applicant_Qualification[test$Applicant_Qualification == "Graduate"] <- 2
test$Applicant_Qualification[test$Applicant_Qualification == "Class X"] <- 3
test$Applicant_Qualification[test$Applicant_Qualification != 1 &
                                  test$Applicant_Qualification != 2 &
                                  test$Applicant_Qualification != 3] <- 4
test$Applicant_Qualification <- as.numeric(test$Applicant_Qualification)

# Encoding Manager_Joining_Designation
# ----------------------------------------------------------
temp_joining_des <- train$Manager_Joining_Designation
train$Manager_Joining_Designation <- 6
train$Manager_Joining_Designation[temp_joining_des == "Level 1"] <- 1
train$Manager_Joining_Designation[temp_joining_des == "Level 2"] <- 2
train$Manager_Joining_Designation[temp_joining_des == "Level 3"] <- 3
train$Manager_Joining_Designation[temp_joining_des == "Level 4"] <- 4
train$Manager_Joining_Designation[temp_joining_des == "Level 5" |
                                      temp_joining_des == "Level 6" |
                                      temp_joining_des == "Level 7"] <- 5
rm(temp_joining_des)

temp_joining_des <- test$Manager_Joining_Designation
test$Manager_Joining_Designation <- 6
test$Manager_Joining_Designation[temp_joining_des == "Level 1"] <- 1
test$Manager_Joining_Designation[temp_joining_des == "Level 2"] <- 2
test$Manager_Joining_Designation[temp_joining_des == "Level 3"] <- 3
test$Manager_Joining_Designation[temp_joining_des == "Level 4"] <- 4
test$Manager_Joining_Designation[temp_joining_des == "Level 5" |
                                      temp_joining_des == "Level 6" |
                                      temp_joining_des == "Level 7"] <- 5
rm(temp_joining_des)

# Encoding Manager_Current_Designation
# ----------------------------------------------------------
temp_current_des <- train$Manager_Current_Designation
train$Manager_Current_Designation <- 6
train$Manager_Current_Designation[temp_current_des == "Level 1"] <- 1
train$Manager_Current_Designation[temp_current_des == "Level 2"] <- 2
train$Manager_Current_Designation[temp_current_des == "Level 3"] <- 3
train$Manager_Current_Designation[temp_current_des == "Level 4"] <- 4
train$Manager_Current_Designation[temp_current_des == "Level 5" |
                                      temp_current_des == "Level 6" |
                                      temp_current_des == "Level 7"] <- 5
rm(temp_current_des)

temp_current_des <- test$Manager_Current_Designation
test$Manager_Current_Designation <- 6
test$Manager_Current_Designation[temp_current_des == "Level 1"] <- 1
test$Manager_Current_Designation[temp_current_des == "Level 2"] <- 2
test$Manager_Current_Designation[temp_current_des == "Level 3"] <- 3
test$Manager_Current_Designation[temp_current_des == "Level 4"] <- 4
test$Manager_Current_Designation[temp_current_des == "Level 5" |
                                      temp_current_des == "Level 6" |
                                      temp_current_des == "Level 7"] <- 5
rm(temp_current_des)

# Encoding Manager_Status
# ----------------------------------------------------------
train$Manager_Status[train$Manager_Status == "Confirmation"] <- 1
train$Manager_Status[train$Manager_Status == "Probation"] <- 2
train$Manager_Status[train$Manager_Status == ""] <- 3
train$Manager_Status <- as.numeric(train$Manager_Status)

test$Manager_Status[test$Manager_Status == "Confirmation"] <- 1
test$Manager_Status[test$Manager_Status == "Probation"] <- 2
test$Manager_Status[test$Manager_Status == ""] <- 3
test$Manager_Status <- as.numeric(test$Manager_Status)

# Encoding Manager_Gender
# ----------------------------------------------------------
train$Manager_Gender[train$Manager_Gender == "F"] <- 1
train$Manager_Gender[train$Manager_Gender == "M"] <- 2
train$Manager_Gender[train$Manager_Gender == ""] <- 3
train$Manager_Gender <- as.numeric(train$Manager_Gender)

test$Manager_Gender[test$Manager_Gender == "F"] <- 1
test$Manager_Gender[test$Manager_Gender == "M"] <- 2
test$Manager_Gender[test$Manager_Gender == ""] <- 3
test$Manager_Gender <- as.numeric(test$Manager_Gender)

# Imputing the numeric variables
# ----------------------------------------------------------
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

# Create variables for business from Category A advisor
# -----------------------------------------------------
# train$Manager_Business1 <- (train$Manager_Business - train$Manager_Business2)
# train$Manager_Num_Products1 <- (train$Manager_Num_Products2 - train$Manager_Num_Products2)
# 
# test$Manager_Business1 <- (test$Manager_Business - test$Manager_Business2)
# test$Manager_Num_Products1 <- (test$Manager_Num_Products2 - test$Manager_Num_Products2)

# ----------------------------------------------
# train$Manager_Gender <- NULL
# test$Manager_Gender <- NULL
# train$Applicant_Marital_Status <- NULL
# test$Applicant_Marital_Status <- NULL

# ----------------------------------------------
# train$Applicant_City_PIN[is.na(train$Applicant_City_PIN)] <- 
#     mean(train$Applicant_City_PIN, na.rm = T)
# test$Applicant_City_PIN[is.na(test$Applicant_City_PIN)] <- 
#     mean(test$Applicant_City_PIN, na.rm = T)

# Add back the Target variable, Business_Sourced
# ----------------------------------------------
train$Business_Sourced <- train.y

###########################################################################################

## BUILD MODEL WITH STRATIFIED K-FOLD CV
folds <- createFolds(as.factor(train$Business_Sourced), k = 5)
fold_auc <- c()

for (fold in folds) {
    x_train <- train[-fold, ]
    x_train.y <- train$Business_Sourced[-fold]
    x_test <- train[fold, ]
    x_test.y <- train$Business_Sourced[fold]
    
    print("Split info")
    print(table(x_train$Business_Sourced)/nrow(x_train))
    print(table(x_test$Business_Sourced)/nrow(x_test))
    
    x_train <- sparse.model.matrix(Business_Sourced ~ ., data= x_train) 
    x_test <- sparse.model.matrix(Business_Sourced ~ ., data = x_test)
    
    d_train <- xgb.DMatrix(data = x_train, label = x_train.y)
    d_test <- xgb.DMatrix(data = x_test, label = x_test.y)
    watchlist <- list(train=d_train, test=d_test)
    
    # Changed depth from 2 to 4
    param <- list( objective    = "binary:logistic",
                   booster      = "gbtree",
                   eval_metric  = "auc",
                   eta          = 0.05,
                   max_depth    = 3
                   )
    
    clf <- xgb.train(   params              = param, 
                        data                = d_train, 
                        nrounds             = 125, 
                        verbose             = 2,
                        watchlist           = watchlist
                        )
    
    fold_pred <- predict(clf, x_test)
    pred <- prediction(fold_pred, x_test.y)
    auc <- performance(pred, measure = "auc")
    fold_auc <- c(fold_auc, auc@y.values[[1]])
}

print(fold_auc)
########################################

train <- sparse.model.matrix(Business_Sourced ~ ., data = train)
dtrain <- xgb.DMatrix(data=train, label=train.y)
watchlist <- list(train=dtrain)
# 
param <- list( objective    = "binary:logistic",
               booster      = "gbtree",
               eval_metric  = "auc",
               eta          = 0.05,
               max_depth    = 4
)

clf <- xgb.train(   params              = param, 
                    data                = d_train, 
                    nrounds             = 125, 
                    verbose             = 2,
                    watchlist           = watchlist,
                    maximize            = FALSE
)

test$target <- -1
test <- sparse.model.matrix(target ~ ., data = test)

preds <- predict(clf, test)
submission <- data.frame(ID=test.id, Business_Sourced=preds)
cat("saving the submission file\n")
write.csv(submission, "Submissions/submission12.csv", row.names = F)

