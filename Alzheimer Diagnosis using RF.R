
#load all the required libraries

library(Hmisc)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(PerformanceAnalytics)
library(randomForest)
library(rpart)
library(rpart.plot)
library(caret)
library(pROC)

ad_data <- read.csv('')
head(ad_data)
str(ad_data)
summary(ad_data)


describe(ad_data)
ad_data <- na.omit(ad_data)

chart.Correlation(select(ad_data, Age, EDUC, SES, MMSE, CDR, eTIV, nWBV, ASF ), histogram = T, main = 'Correlation using PerformanceAnalytics')



ggplot(ad_data) +
  aes(x = CDR, y = Age, fill = Group) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_minimal()
str(ad_data)
summary(ad_data)


#\\\\\\\\----------DATA VISUALIZATION-------------

#CDR vs SES
ggplot(ad_data) +
  aes(x = CDR, y = SES) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#CDR vs eTIV
ggplot(ad_data) +
  aes(x = CDR, y = eTIV) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#CDR vs nWBV
ggplot(ad_data) +
  aes(x = CDR, y = nWBV) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#CDR va ASF
ggplot(ad_data) +
  aes(x = CDR, y = ASF) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()


#DATA PREP
#Absence of NA Values and Good Data Structure (Esp. Factors) nullify this step.


#MODELING
#Here we're gonna use Decision Trees and Random Forest to predict CDR. I could've ran a Logistic Model. But I want to try out Classification using DT and RF. 

#DECISION TREE
model_ad <- ad_data %>% select(M.F, Age, EDUC, MMSE, eTIV, nWBV, ASF, CDR) %>% mutate(CDR = as.factor(CDR))
model_ad <- na.omit(model_ad)
train_set <- round(0.8 * nrow(model_ad))
indices <- sample(1:nrow(model_ad), train_set)
train <- model_ad[indices,]
test <- model_ad[-indices,]


tree <- rpart(CDR ~ .,data = train,method="class")
plot(tree);text(tree, pretty=2)


#TREE PLOT
#plot decision tree model
prp(x = tree, type=1, extra = 102)


#CALCULATE OPTIMAL CP
#now to calc optimal value of cp
printcp(tree)
#or 
plotcp(tree)

#DECISION TREE TEST MODEL
#testing model
pred1 <- predict(tree, newdata = test, type = 'class')
pred1

confusionMatrix(pred1, test$CDR)


#CROSS VALIDATION OF DECISION TREE

# USING ORIGINAL DATASET

cv_pred <- predict(tree, newdata = model_ad, type = 'class')
confusionMatrix(cv_pred, model_ad$CDR )


# DECISION TREE ON TREE - VALIDATION DATA 2

# USING TRAIN DATA
train_pred <- predict(tree, newdata = train, type = 'class')
confusionMatrix(train_pred, train$CDR)

