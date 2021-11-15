#install.packages("naivebayes")
library(naivebayes)


data <- read.csv("ch.csv")
library(dplyr)
library(ggplot2)
#install.packages('caret', dependencies = TRUE)
#install with dependencies = TRUE is important for the calculation of sensitivity and specficity
library(caret)
library(corrplot)
library(tidyverse)
library(repr)
library(caTools)
library(pROC)
library(rpart)
library(rpart.plot)
library(ggpubr)


data <- transform(
  data, 
  Attrition_Flag = as.factor(Attrition_Flag),
  Gender = as.factor(Gender),
  Education_Level = as.factor(Education_Level),
  Marital_Status = as.factor(Marital_Status),
  Income_Category = as.factor(Income_Category),
  Card_Category = as.factor(Card_Category))

data <- subset(data, select=-c(X,CLIENTNUM))


smp_size <- floor(0.75 * nrow(data))

## set the seed to make your partition reproducible
set.seed(12345)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

model <- naive_bayes(Attrition_Flag ~ ., data = train, usekernel = T) 
plot <- plot(model) 

p <- predict(model, train, type = 'prob')
head(cbind(p, train))

p1 <- predict(model, train)
(tab1 <- table(p1, train$Attrition_Flag))

print(accuracyrftest <- (6372+478)/nrow(train))
print(sensitivityrftest <- 478/(478+21))
print(precisionrftest <- 478/(478+724))
print(specificityrftest <- 6372/(6372 + 724))

p2 <- predict(model, newdata = test)
print(p2table <- table(p2, test$Attrition_Flag))
print(accuracynbtest <- (2097+176)/nrow(test))
print(sensitivitynbtest <- 176/(176+10))
print(precisionnbtest <- 176/(176+249))
print(specificitynbtest <- 2097/(2097 + 249))


par(mai=c(.9,.8,.2,.2))
plot(roc(test$Attrition_Flag, as.numeric(p2)), print.auc=TRUE,
     col="black", lwd=1, main="ROC curve", xlab="Specificity: true negative rate", ylab="Sensitivity: true positive rate", xlim=c(1,0))


#SVM

#install.packages("e1071")
library(e1071)

svmfit = svm(Attrition_Flag ~ ., data = train, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)
summary(svmfit)

trainpredict <- predict(svmfit, train)

confusionMatrix(table(trainpredict, train$Attrition_Flag))


testpredict <- predict(svmfit, test)
confusionMatrix(table(testpredict, test$Attrition_Flag))


par(mai=c(.9,.8,.2,.2))
plot(roc(test$Attrition_Flag, as.numeric(testpredict)), print.auc=TRUE,
     col="black", lwd=1, main="ROC curve", xlab="Specificity: true negative rate", ylab="Sensitivity: true positive rate", xlim=c(1,0))



svmfit2 = svm(Attrition_Flag ~ ., data = train, kernel = "radial", cost = 10, scale = FALSE)
print(svmfit2)
summary(svmfit2)

trainpredict2 <- predict(svmfit2, train)

confusionMatrix(table(trainpredict2, train$Attrition_Flag))


testpredict2 <- predict(svmfit2, test)
confusionMatrix(table(testpredict2, test$Attrition_Flag))


svmfit3 = svm(Attrition_Flag ~ ., data = train, kernel = "polynomial", cost = 10, scale = FALSE)
print(svmfit3)
summary(svmfit3)

trainpredict3 <- predict(svmfit3, train)

confusionMatrix(table(trainpredict3, train$Attrition_Flag))


testpredict3 <- predict(svmfit3, test)
confusionMatrix(table(testpredict3, test$Attrition_Flag))

svmfit4 = svm(Attrition_Flag ~ ., data = train, kernel = "sigmoid", cost = 10, scale = FALSE)
print(svmfit4)
summary(svmfit4)

trainpredict4 <- predict(svmfit4, train)

confusionMatrix(table(trainpredict4, train$Attrition_Flag))


testpredict4 <- predict(svmfit4, test)
confusionMatrix(table(testpredict4, test$Attrition_Flag))
#K nearest neighbours


##the normalization function is created
#nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Run nomalization on first 4 coulumns of dataset because they are the predictors
#data <- as.data.frame(lapply(data[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)], nor))

#set.seed(12345)
#train_ind <- sample(seq_len(nrow(data)), size = smp_size)

data %>% 
  mutate_all(funs(as.numeric(as.factor(.))))
train <- data[train_ind, ]
test <- data[-train_ind, ]
traincategory <- data[train_ind, 1]
testcategory <- data[-train_ind, 1]

##load the package class
library(class)
##run knn function
pr <- knn(train,test, cl = traincategory, k=13)

