library(corpcor)
library(car)
library(perturb)
library(MASS)
library(dummies)

setwd("C://Users//Savla-Home//Documents//Manu//PGDDS//Assignment")

## Read the dataset & summarize
Data<-read.csv("Term life.csv", header = TRUE)
Data <- Data[,-c(12, 13, 15, 16, 17, 18 )]

str(Data)
summary(Data)

## Create dummy variables for categorical variables
Data$GENDER <- factor(Data$GENDER)
Data$MARSTAT <- factor(Data$MARSTAT)
Data$ETHNICITY <- factor(Data$ETHNICITY)
Data$SMARSTAT <- factor(Data$SMARSTAT)
Data$SGENDER <- factor(Data$SGENDER)

Data<- dummy.data.frame(Data,dummy.class="factor")
Data <- Data[,-c(2, 6, 11, 15, 18 )]
names(Data)
attach(Data)

## Univariate Analysis of all variables: y 's and x's 
boxplot(GENDER0, col ="blue")
boxplot(AGE, col ="blue")
boxplot(MARSTAT0, col ="blue")
boxplot(MARSTAT1, col ="blue")
boxplot(EDUCATION, col ="blue")
boxplot(ETHNICITY1, col ="blue")
boxplot(ETHNICITY2, col ="blue")
boxplot(ETHNICITY3, col ="blue")
boxplot(SMARSTAT0, col ="blue")
boxplot(SMARSTAT1, col ="blue")
boxplot(SMARSTAT2, col ="blue")
boxplot(SGENDER0, col ="blue")
boxplot(SGENDER1, col ="blue")
boxplot(SAGE, col ="blue")
boxplot(SEDUCATION, col ="blue")
boxplot(NUMHH, col ="blue")
boxplot(INCOME, col ="blue")
boxplot(FACE, col ="blue")


## Bivariate Analysis using	Correlation Coefficient, Partial Correlation Coefficient & Scatter Plots 
plot(Data)
cor(Data)
cor2pcor(cor(Data))


## Model 1 - First Model with y  & all X variables: Evaluate model summary, Anova analysis, diagnostic plots, residual plots, AV Plots for getting an understanding of fit
model1 <-lm (FACE ~ ., data= Data)
summary(model1)
anova(model1)
par(mfrow=c(2,2))
plot(model1)
residualPlots(model1)
avPlots(model1, id.n=2, id.cex=0.7) 

## Model 2 - Create and evaluate a model removing SMARSTAT0, SGENDER0
Data <- Data[,-c(9,12)]
model2 <-lm (FACE ~ ., data= Data)
summary(model2)
anova(model2)
par(mfrow=c(2,2))
plot(model2)
residualPlots(model2)
avPlots(model2, id.n=2, id.cex=0.7) 

## Check for Multicollinearity 
vif(model2)
colldiag(model2)

## Model 3 - Y Transformation 
Data$FACE <- sqrt(Data$FACE)
model3 <-lm (FACE ~ ., data= Data)
summary(model3)
anova(model3)
par(mfrow=c(2,2))
plot(model3)
residualPlots(model3)
avPlots(model3, id.n=2, id.cex=0.7)

## Model 4 - X Transformations
Data$AGE <- (Data$AGE) ^2
Data$EDUCATION <- sqrt(Data$EDUCATION) 
Data$SAGE <- (Data$SAGE) ^2
Data$SEDUCATION <- sqrt(Data$SEDUCATION) 
Data$INCOME<- (Data$INCOME)^2

model4 <-lm (FACE ~ ., data= Data)
summary(model4)
anova(model4)
par(mfrow=c(2,2))
plot(model4)
residualPlots(model4)
avPlots(model3, id.n=2, id.cex=0.7)


## Variable Selection using StepAIC
stepAIC(model4) 

## Model 5 - Fit the model with selected variables
model5 <- lm(FACE ~ GENDER0 + MARSTAT0 + MARSTAT1 + EDUCATION + SMARSTAT2 + SGENDER1 + SEDUCATION + NUMHH, data = Data)
             
summary(model5)
anova(model5)
par(mfrow=c(2,2))
plot(model5)
residualPlots(model5)
avPlots(model5, id.n=2, id.cex=0.7) 

## Look at Influential variables  using Cook's distance > 1.0
influence.measures(model5)
influenceIndexPlot(model5) # Index Plots of the influence measures
influencePlot(model5) # A user friendly representation of the above

## Model 6 - Fit the model with/without influential variables 
Data1 <- Data[-c(22,452), ]
model6 <- lm(FACE ~ GENDER0 + MARSTAT0 + MARSTAT1 + EDUCATION + SMARSTAT2 + SGENDER1 + SEDUCATION + NUMHH, data = Data1)

summary(model6)
anova(model6)
par(mfrow=c(2,2))
plot(model6)
residualPlots(model6)
avPlots(model6, id.n=2, id.cex=0.7) 

## Final Model - Model 3
summary(model3)
anova(model3)
par(mfrow=c(2,2))
plot(model3)
residualPlots(model3)
avPlots(model3, id.n=2, id.cex=0.7) 

