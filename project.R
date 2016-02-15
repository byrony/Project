# clear the data. Use the variables that aren't null and NA in test set.

dir <- "D:/Coursera/8. Practical Machine Learning/Project"
setwd(dir)
test <- read.csv("pml-testing.csv", header = TRUE, na.strings = c("NA", "#DIV/0!"))
train <- read.csv("pml-training.csv", header = TRUE, na.strings = c("NA", "#DIV/0!"))

# if the count of NAs in a column equals to the number of rows, it must be entirely NA
# here drop the columns in which NAs are greater than 60% # of rows
train <- train[ ,colSums(is.na(train)) <= nrow(train)*0.6]
test <- test[ ,colSums(is.na(test)) != nrow(test)]

# drop the columns near zero variance
col.NZV <- nearZeroVar(train, saveMetrics = TRUE)


drop.columns <- c("X", "problem_id", names(col.NZV))

test <- test[ , !colnames(test) %in% drop.columns]
classe <- train$classe
train <- train[, colnames(train) %in% names(test)]
train <- data.frame(train, classe)


## fit the decision tree model.
library(caret)
set.seed(123)
# default lift=TRUE, use [[1]] in extract resampled index
inTrain <-createDataPartition(train$user_name, p=0.6, list = FALSE) 
training <- train[inTrain, ]
validation <- train[-inTrain, ]

#tree <- train(classe ~., data = training, method = "rpart") # why use caret is slow? seems there is error.
tree <- rpart(classe ~., data = training, method = "class")
rattle::fancyRpartPlot(tree)
tree.predict <- predict(tree, newdata = validation, type = "class")
table(validation$classe, tree.predict)
confusionMatrix(validation$classe, tree.predict)

## random forest
library(randomForest)
set.seed(1234)
rf <- randomForest(classe ~., data = training, ntree = 50)
rf.predict <- predict(rf, newdata = validation)
table(validation$classe, rf.predict)
confusionMatrix(validation$classe, rf.predict)

## boosting
library(gbm)
set.seed(12345)
# fitControl <- trainControl(method = "repeatedcv",
#                            number = 5,
#                            repeats = 1)
# boost <- train(classe ~., data = training, method = "gbm", verbose = FALSE, trControl = fitControl)
boost <- gbm(classe ~., data = training, distribution = "multinomial", n.trees = 100, 
             shrinkage = 0.2, interaction.depth = 3,verbose = FALSE)
# notice that boost.predict is a array
boost.predict <- predict(boost, newdata = validation, n.trees = 100, type = "response")
boost.predict[1:6, ,]
boost.predIndex <- apply(boost.predict, MARGIN = 1, which.max) # find out the index of maximal prob. each row
boost.predClass <- colnames(boost.predict)[boost.predIndex] # convert index into class A-E
levels(boost.predClass) <- levels(validation$classe) # make sure they have the same level
confusionMatrix(validation$classe, boost.predClass)

# predict test dataset
levels(test$cvtd_timestamp) <- levels(train$cvtd_timestamp)
levels(test$new_window) <- levels(train$new_window)
test.predicted <- predict(rf, newdata = test, type = "class")
test.predicted
