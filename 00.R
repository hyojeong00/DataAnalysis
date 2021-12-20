## 패키지 로드
library(RPostgreSQL)
library(data.table)
library(corrplot)
library(caret)
library(rpart) 
library(rpart.plot)
library(party)
library(e1071)
library(lmtest)
library(ROSE)
library(foreach)
library(ROCR)
library(ggplot2)
library(dplyr)
library(randomForest)
library(plyr)
library(gbm)
library(car)


## 이항편차 함수 생성
binomial_deviance <- function(y_obs , yhat) {
  epsilon = 0.0001
  yhat <- ifelse(yhat < epsilon, epsilon, yhat)
  yhat <- ifelse(yhat > 1 - epsilon, 1 - epsilon, yhat)
  a = ifelse(y_obs == 0, 0, y_obs * log(y_obs/yhat))
  b = ifelse(y_obs == 1, 0, (1 - y_obs) * log((1 - y_obs)/(1 - yhat)))
  return(2 * sum(a + b))
}

