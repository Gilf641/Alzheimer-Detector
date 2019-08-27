
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
