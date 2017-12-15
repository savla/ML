install.packages("ROCR")
install.packages("caret")
install.packages("randomForest")
install.packages("FSelector")
install.packages("caTools")
install.packages("party")

library(caTools)
library(FSelector)
library(randomForest)
library(caret)
library(tree)
library(ROCR)
library(party)

setwd("C://Users//Administrator.TMP255//Desktop//PGADS_Notes_Codes")

## Read the datasset
df1<-read.csv("subscription_Prediction.csv", header = TRUE, stringsAsFactors=F)

## Data Exploration
names(df1)
dim(df1)
str(df1)
summary(df1)

## Data Clean Up - Converting categorical variables into factors & continuous into numeric
df1$C1 <- as.factor(df1$C1)
df1$C4 <- as.factor(df1$C4)
df1$C5 <- as.factor(df1$C5)
df1$C6 <- as.factor(df1$C6)
df1$C7 <- as.factor(df1$C7)
df1$C9 <- as.factor(df1$C9)
df1$C10 <- as.factor(df1$C10)
df1$C12 <- as.factor(df1$C12)
df1$C13 <- as.factor(df1$C13)
df1$target <- as.factor(df1$target)
df1$C2 <- as.numeric(df1$C2)
df1$C14 <- as.numeric(df1$C14)
str(df1)
summary(df1)

## Data Clean up - Imputing missing values - Mode for categorical variables
df1$C1[df1$C1=="?"] <- "b"
df1$C4[df1$C4=="?"] <- "u"
df1$C5[df1$C5=="?"] <- "g"
summary(df1)

## Data Clean up - Imputing missing values - Mean for numeric variables
df1$C2[is.na(df1$C2)] <- mean(df1$C2,na.rm =TRUE)
df1$C14[is.na(df1$C14)] <- mean(df1$C14,na.rm =TRUE)
summary(df1)

## Feature Selection using Informantion Gain
fs1 <- information.gain(target~., df1)
fs1
df2 <- df1[,-c(1,12,13)]

## Data Partitioning
split <- sample.split(df2$target, SplitRatio = 0.70)
trainset <- subset(df2, split == TRUE)
testset <- subset(df2, split == FALSE)

## Model building using LR
model_lr <- glm(target ~., data = trainset, family = "binomial")
summary(model_lr)

## Model building using RF
model_RF <- randomForest(formula = target ~ C2 + C3 + C4 + C5+ C6+ C7+ C8+ C9+ C10+ C11+ C14+ C15, data = trainset,ntree=500, type =classification)
print(model_RF)

## Model building using Decision Trees
model_dtree <- ctree(trainset$target ~ .,data = trainset)
summary(model_dtree)
print(model_dtree)

## Model assesments - LR
## Scoriing on test
cutoff_prob <- 0.75
predict <- predict.glm(model_lr, testset, type = 'response')
table(testset$target, predict > cutoff_prob)

## ROC 
prob<-predict(model_lr, testset, type = "response")
prob<-as.data.frame(prob)
pred<-prediction(prob[,1], testset[,"target"])
perf<-performance(pred, "tpr","fpr")
plot(perf)

## AUC
auc<-performance(pred,"auc")
auc@y.values[[1]]

## Model Assessment - Random Forest
## ROC 
prob<-predict(model_RF, testset)
prob<-as.data.frame(prob)
prob
pred<-prediction(as.numeric(prob[,1]), as.numeric(testset$target))
perf<-performance(pred, "tpr","fpr")
plot(perf)

## AUC
auc<-performance(pred,"auc")
auc@y.values[[1]]
auc@y.values

## Model Assesment - DT
## ROC 
class(model_dtree)
prob<-predict(model_dtree, testset, type = "response")
prob<-as.data.frame(prob)
pred<-prediction(as.numeric(prob[,1]), as.numeric(testset$target))
perf<-performance(pred, "tpr","fpr")
plot(perf)

## AUC
auc<-performance(pred,"auc")
auc@y.values[[1]]

## Writing the predictions
model_LR.predict <- write.csv("Results.csv")
