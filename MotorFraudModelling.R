library(dummies)
library(caTools)
library(ROCR)
library(caret)
library(party)
library(tree)

setwd("C://Users//Savla-Home//Documents//Manu//PGDDS//Assignment")

## Read the dataset & summarize
Data<-read.csv("Motor Fraud.csv", header = TRUE)

str(Data)
summary(Data)

## Create dummy variables for categorical variables
Data$fraud <- factor(Data$fraud)
Data$police <- factor(Data$police)
Data$witness <- factor(Data$witness)
Data$nonurban <- factor(Data$nonurban)

Data<- dummy.data.frame(Data,dummy.class="factor")
Data <- Data[,-c(2, 4, 6, 8 )]
names(Data)
attach(Data)

## Data Partitioning
split <- sample.split(Data$fraud, SplitRatio = 0.70)

trainset <- subset(Data, split == TRUE)
testset <- subset(Data, split == FALSE)

## Logistic Regression 
model_lr_tr <- glm(trainset$fraud ~ .,data = trainset, family = "binomial")
summary(model_lr_tr)

## Scoriing on test
cutoff_prob <- 0.80
predict <- predict.glm(model_lr_tr, testset, type = 'response')
table(testset$fraud, predict > cutoff_prob)

## ROC 
prob<-predict(model_lr_tr, testset, type = "response")
prob<-as.data.frame(prob)
pred<-prediction(prob[,1], testset[,"fraud0"])
perf<-performance(pred, "tpr","fpr")
plot(perf)

## AUC
auc<-performance(pred,"auc")
auc@y.values[[1]]

## Random Forest
Fit <- train(x=trainset[,2:5],y=trainset[,1],
             method = "rf",
             data=trainset,
             preProcess = c("center",'scale'),
             ntree=100)
summary(Fit)

## ROC 
prob<-predict(Fit, testset, type = "raw")
prob<-as.data.frame(prob)
pred<-prediction(prob[,1], testset[,"fraud0"])
perf<-performance(pred, "tpr","fpr")
plot(perf)

## AUC
auc<-performance(pred,"auc")
auc@y.values[[1]]
auc@y.values

## Decision Trees
ctree <- ctree(trainset$fraud ~ .,data = trainset)
summary(ctree)
print(ctree)
## ROC 
prob<-predict(ctree, testset, type = "response")
prob<-as.data.frame(prob)
pred<-prediction(prob[,1], testset[,"fraud0"])
perf<-performance(pred, "tpr","fpr")
plot(perf)

## AUC
auc<-performance(pred,"auc")
auc@y.values[[1]]


