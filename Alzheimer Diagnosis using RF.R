
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


