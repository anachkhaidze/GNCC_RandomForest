library(psych)
library(plyr)
library(car)
library(ggplot2)
library(modeest)
library(lmSupport)
library(psych)
library(Hmisc)
library(MASS)
library(perturb)
library(stargazer)
library(olsrr)
library(caret)
library(ResourceSelection)
library(pROC)
library(aod)
library(e1071)
library(randomForest)

## do a LOGIT REGRESSION with megabytes used as a predictor
#
#
# Use "smartphone" as outcome
training.data.raw <- read.csv('data_usage_train_imbalanced.csv',header=T,na.strings=c(""))
training.data.raw$smartphone <- factor(training.data.raw$smartphone) # so variable will be treated as a categorical variable.


#missing values
# sapply(training.data.raw,function(x) sum(is.na(x)))
# sapply(training.data.raw, function(x) length(unique(x)))

#visualize missing
library(Amelia)
missmap(training.data.raw, main = "Missing values vs observed")

#use subset of data
data <- subset(training.data.raw,select=c(1,2,3))

#categorical variables
is.factor(data$smartphone)
contrasts(data$smartphone)

#Model fitting
# Random sample indexes
train_index <- sample(1:nrow(data), 0.9 * nrow(data))
test_index <- setdiff(1:nrow(data), train_index)

# Build X_train, y_train, X_test, y_test
X_train <- data[train_index, -15]
y_train <- data[train_index, "smartphone"]

X_test <- data[test_index, -15]
y_test <- data[test_index, "smartphone"]

# Create a Random Forest model with default parameters
model1 <- randomForest(smartphone ~ ., data = X_train, importance = TRUE)
model1

scatterplot(data$num_days, data$smartphone)

# Predicting on train set
predTrain <- predict(model1, X_train, type = "class")
# Checking classification accuracy
table(predTrain, X_train$smartphone)

# Predicting on Validation set
predValid <- predict(model1, X_test, type = "class")
# Checking classification accuracy
mean(predValid == X_test$smartphone)                    
table(predValid,X_test$smartphone)

# # Using For loop to identify the right mtry for model
# a=c()
# i=5
# for (i in 3:8) {
#   model3 <- randomForest(smartphone ~ ., data = X_train, ntree = 500, mtry = i, importance = TRUE)
#   predValid <- predict(model3, X_test, type = "class")
#   a[i-2] = mean(predValid == X_test$smartphone)
# }
# 
# a
# 
# plot(3:8,a)

#Compare with decision trees
# Compare with Decision Tree

install.packages("rpart")
install.packages("caret")
install.packages("e1071")

library(rpart)
library(caret)
library(e1071)

# We will compare model 1 of Random Forest with Decision Tree model

model_dt = train(smartphone ~ ., data = X_train, method = "rpart")
model_dt_1 = predict(model_dt, data = X_train)
table(model_dt_1, X_train$smartphone)

mean(model_dt_1 == X_train$smartphone)

# Running on Validation Set
model_dt_vs = predict(model_dt, newdata = X_test)
table(model_dt_vs, X_test$smartphone)

mean(model_dt_vs == X_test$smartphone)

