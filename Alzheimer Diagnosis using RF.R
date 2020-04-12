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


ad_data <- read.csv('dataset.csv')
head(ad_data)
str(ad_data)
summary(ad_data)


describe(ad_data)
ad_data <- na.omit(ad_data)

chart.Correlation(select(ad_data, Age, EDUC, SES, MMSE, CDR, eTIV, nWBV, ASF ), histogram = T, main = 'Correlation using PerformanceAnalytics')


#\\\\\\\\----------DATA VISUALIZATION-------------

ggplot(ad_data) +
  aes(x = CDR, y = Age, fill = Group) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_minimal()
str(ad_data)
summary(ad_data)



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


confusionMatrix(pred1, test$CDR)


#CROSS VALIDATION OF DECISION TREE

# USING ORIGINAL DATASET

cv_pred <- predict(tree, newdata = model_ad, type = 'class')
confusionMatrix(cv_pred, model_ad$CDR )


# DECISION TREE ON TREE - VALIDATION DATA 2

# USING TRAIN DATA
train_pred <- predict(tree, newdata = train, type = 'class')
confusionMatrix(train_pred, train$CDR)


#RANDOM FOREST MODEL
formula1 <- CDR ~ M.F + Age + EDUC + MMSE + eTIV + nWBV + ASF + CDR
#remove na from train
new_train <- na.omit(train)
rf1 <- randomForest(formula = formula1, data = new_train, importance = T)
print(rf1)
summary(rf1)
str(new_train)
str(train)


# RANDOM FOREST ERROR RATE

plot(rf1, main = "Model Error by Number of Trees")
legend(x = "right", 
       legend = colnames(rf1$err.rate),
       fill = 1:ncol(rf1$err.rate))
varImpPlot(rf1, main = "Importance of Variables") #plot variance importance


#attributes of random forest model
attributes(rf1)
rf1$confusion


set.seed(101)
formula1 <- CDR ~ M.F + Age + EDUC + MMSE + eTIV + nWBV + ASF + CDR
rf2 <- randomForest(formula1, na.omit(train))
print(rf2)


set.seed(1010)
rf500 <- randomForest(CDR ~.,
                      data = train, ntree = 1000, do.trace = F)
rf500.pred_class <- predict(rf500, newdata = test, type = 'class')

table(rf500.pred_class, test$CDR)
rf500

importance(rf500)


#TUNING OF RANDOM FOREST
#Let's create a baseline for comparison by using the recommend defaults for each parameter and mtry=floor(sqrt(ncol(x))) or mtry=7 and ntree=500.

# Create model with default paramters
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(ad_data))
tunegrid <- expand.grid(.mtry=mtry)
rf101 <- train(CDR~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf101)
#ACCURACY IS 75%
#MTRY = 4

colnames(ad_data)

#without CDR and all ID's
df2 <- ad_data[!names(ad_data) %in% c("CDR",'Subject.ID','MRI.ID')]
str(df2)

df3 <- ad_data[,12]
df3


trf101 <- tuneRF(df2, df3, stepFactor = 0.5,mtry = 3, improve = 0.01, plot = T, ntreeTry = 500)
trf102 <- tuneRF(df2, df3, stepFactor = 0.5, improve = 0.01, plot = T, ntreetry = 500)

set.seed(seed)
bestmtry <- tuneRF(df, df3, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)


#TUNING MANUALLY

# Manual Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(ad_data))))
modellist <- list()
for (ntree in c(1000, 1500, 2000, 2500)) {
  set.seed(seed)
  fit <- train(CDR~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)

#from the results the max acc can be achieved if the no of trees = 1000 i.e 76.7%

#AUC calculation
set.seed(720)
#RandomForest Model
rf.roc <- roc(train$CDR,rf500$votes[,2])
plot(rf.roc)
rf_auc <- auc(rf.roc)
print(paste0("AUC for this RandomForest Model = ", round(rf_auc, 3)))


