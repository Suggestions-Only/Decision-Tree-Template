install.packages("ISLR")
install.packages("e1071")
library(ISLR)
data <- ISLR::OJ
library(tidyverse)
library(rpart)
library(rpart.plot)
library(ROCR)
library(caret)
library(caTools)

set.seed(1234)
OJ_index <- sample.split(data$Purchase, SplitRatio = 0.7)
train <- data[OJ_index,]
test <- data[!OJ_index,]

sum(train$Purchase == "MM")
mean(train$PriceMM)
mean(train$DiscMM)

train%>%
  filter(Purchase == "MM",WeekofPurchase == 275)%>%
  length()


tree1 = rpart(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,data=train,
              method="class")
prp(tree1)
predTree1 = predict(tree1,type="class")
ct = table(train$Purchase,predTree1); ct

predTree2 = predict(tree1,newdata = test,type="class")
ct = table(test$Purchase,predTree2); ct

summary(test$Purchase)
summary(test)

ROCRpred = prediction(as.numeric(predTree2),as.numeric(test$Purchase))
summary(ROCRpred)
performance(ROCRpred, measure = "auc")
perf <- performance(ROCRpred, measure = "auc")
perf@y.values[[1]]

trControl = trainControl(method="cv",number=10) #10-fold cross validation
tuneGrid = expand.grid(.cp=seq(0,0.1,0.001))    # 

set.seed(100)
trainCV = train(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+PctDiscCH+PctDiscMM,data=train,
                method="rpart", trControl=trControl,tuneGrid=tuneGrid)

(trainCV$results) # first few cv results

plot(trainCV)

trainCV$bestTune

treeCV = rpart(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+PctDiscCH+PctDiscMM,data=train,
                  method="class", control=rpart.control(cp=trainCV$bestTune))
pred = predict(treeCV,newdata=test,type="class")
ROCRpred = prediction(as.numeric(pred),as.numeric(test$Purchase))
summary(ROCRpred)
performance(ROCRpred, measure = "auc")
perf <- performance(ROCRpred, measure = "auc")
perf@y.values[[1]]
