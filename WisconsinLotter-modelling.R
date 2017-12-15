library(corpcor)
library(car)
library(perturb)
library(MASS)
library(dummies)

setwd("C://Users//Savla-Home//Documents//Manu//PGDDS//Stats//fwdcsvfilesfortommorrow")

## Read the dataset & summarize
Data<-read.csv("WiscLottery.csv", header = TRUE)
str(Data)
attach(Data)
summary(Data)

## Step 1: Univariate Analysis of all variables: y 's and x's 
boxplot(PERPERHH, main="Persons per Household", col ="blue")
boxplot(MEDSCHYR, main="Median years of schooling", col ="blue")
boxplot(MEDHVL, main="Median home value", col ="blue")
boxplot(PRCRENT, main="Percent of housing", col ="blue")
boxplot(PRC55P, main="Percent of population that is 55", col ="blue")
boxplot(HHMEDAGE, main="Household median age", col ="blue")
boxplot(MEDINC, main="median household income", col ="blue")
boxplot(POP, main="Online lottery sales", col ="blue")
boxplot(SALES, main="Population", col ="blue")

## Step 2: Bivariate Analysis using	Correlation Coefficient, Partial Correlation Coefficient & Scatter Plots 
plot(Data[ ,2:10])
cor(Data[ ,2:10])
cor2pcor(cor(Data[ ,2:10]))

## Step 3: Basic Model with y  & all X variables: Evaluate model summary, Anova analysis, diagnostic plots, residual plots, AV Plots for getting an understanding of fit
model1 <-lm (SALES ~ ., data= Data[ ,2:10])
summary(model1)
anova(model1)
par(mfrow=c(2,2))
plot(model1)
residualPlots(model1)
avPlots(model1, id.n=2, id.cex=0.7) 

## Step 4: Transform y based on Box Cox Transformation 
gh<-boxcox(model1)
gh$x[which.max(gh$y)]

## Step 5: Build Next model with transformed Y:  Evaluate model summary, Anova analysis, diagnostic plots, residual plots, AV Plots for getting an understanding of fit
model2 <-lm (log(SALES) ~ ., data= Data[ ,2:10])
summary(model2)
anova(model2)
par(mfrow=c(2,2))
plot(model2)
residualPlots(model2)
avPlots(model2, id.n=2, id.cex=0.7) 

## Step 6: Build Next model with transformed X Values:  Evaluate model summary, Anova analysis, diagnostic plots, residual plots, AV Plots for getting an understanding of fit
Data$PERPERHH2 <- (Data$PERPERHH) ^2
Data$MEDSCHYR2 <- (Data$MEDSCHYR) ^2
Data$PRCRENT2 <- (Data$PRCRENT)^2
Data$HHMEDAGE2 <- (Data$HHMEDAGE)^2

model3 <-lm (log(SALES) ~ ., data= Data[ ,2:14])
summary(model3)
anova(model3)
par(mfrow=c(2,2))
plot(model3)
residualPlots(model3)
avPlots(model3, id.n=2, id.cex=0.7) 

Data$POP2 <- (Data$POP) ^2
model4 <-lm (log(SALES) ~ ., data= Data[ ,2:15])
summary(model4)
anova(model4)
par(mfrow=c(2,2))
plot(model4)
residualPlots(model4)
avPlots(model4, id.n=2, id.cex=0.7)

## Step 7: Check for Multicollinearity 
vif(model4)
colldiag(model4)

## Step 8: Mitigate Multicollinearity using	Approx Mean centering 
Data1 <- Data
summary(Data1)
Data1$PERPERHH <- Data1$PERPERHH - 2.706
Data1$MEDSCHYR <- Data1$MEDSCHYR-12.70
Data1$MEDHVL <- Data1$MEDHVL - 57.09
Data1$PRCRENT <- Data1$PRCRENT - 24.68
Data1$PRC55P <- Data1$PRC55P - 39.7
Data1$HHMEDAGE <- Data1$HHMEDAGE - 48.76
Data1$MEDINC <- Data1$MEDINC -45.12
Data1$POP <- Data1$POP - 9311
Data1$PERPERHH2 <- Data1$PERPERHH2 - 7.365
Data1$MEDSCHYR2 <- Data1$MEDSCHYR2 - 161.5
Data1$PRCRENT2 <- Data1$PRCRENT2 - 694.6
Data1$HHMEDAGE2 <- Data1$HHMEDAGE2 - 2394 
Data1$POP2 <- Data1$POP2 - 2.074e+08

## Step 9: Fit the model
model5 <- lm( log(SALES) ~., data = Data1[,(2:15)])
summary(model5)
anova(model5)
par(mfrow=c(2,2))
plot(model5)
residualPlots(model5)
avPlots(model5, id.n=2, id.cex=0.7) 
vif(model5)
colldiag(model5)

## Step 10: Variable Selection using StepAIC
stepAIC(model5) 

## Step 11: Fit the model with selected variables
model6 <- lm(log(SALES) ~ PERPERHH + MEDSCHYR + MEDHVL + PRC55P + 
               HHMEDAGE + POP + PERPERHH2 + POP2, data = Data1[, (2:15)])
summary(model6)
anova(model6)
par(mfrow=c(2,2))
plot(model6)
residualPlots(model6)
avPlots(model6, id.n=2, id.cex=0.7) 
vif(model6)
colldiag(model6)

## Step 12: Look at Influential variables  using Cook's distance > 1.0
influence.measures(model6)
influenceIndexPlot(model6) # Index Plots of the influence measures
influencePlot(model6) # A user friendly representation of the above

## Step 13.	Fit the model with/without influential variables 
Data2 <- Data1[-9, ]
model6 <- lm(log(SALES) ~ PERPERHH + MEDSCHYR + MEDHVL + PRC55P + 
               HHMEDAGE + POP + PERPERHH2 + POP2, data = Data2[, (2:15)])
summary(model6)
anova(model6)
par(mfrow=c(2,2))
plot(model6)
residualPlots(model6)
avPlots(model6, id.n=2, id.cex=0.7) 
vif(model6)
colldiag(model6)

Data3 <- Data1[-21, ]
model6 <- lm(log(SALES) ~ PERPERHH + MEDSCHYR + MEDHVL + PRC55P + 
               HHMEDAGE + POP + PERPERHH2 + POP2, data = Data3[, (2:15)])
summary(model6)
anova(model6)
par(mfrow=c(2,2))
plot(model6)
residualPlots(model6)
avPlots(model6, id.n=2, id.cex=0.7) 
vif(model6)
colldiag(model6)

Data4 <- Data1[-28, ]
model6 <- lm(log(SALES) ~ PERPERHH + MEDSCHYR + MEDHVL + PRC55P + 
               HHMEDAGE + POP + PERPERHH2 + POP2, data = Data4[, (2:15)])
summary(model6)
anova(model6)
par(mfrow=c(2,2))
plot(model6)
residualPlots(model6)
avPlots(model6, id.n=2, id.cex=0.7) 
vif(model6)
colldiag(model6)

## Step 14.	Final Model 
model6 <- lm(log(SALES) ~ PERPERHH + MEDSCHYR + MEDHVL + PRC55P + 
               HHMEDAGE + POP + PERPERHH2 + POP2, data = Data1[, (2:15)])
summary(model6)
anova(model6)
par(mfrow=c(2,2))
plot(model6)
residualPlots(model6)
avPlots(model6, id.n=2, id.cex=0.7) 
vif(model6)
colldiag(model6)
