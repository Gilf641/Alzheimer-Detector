
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

