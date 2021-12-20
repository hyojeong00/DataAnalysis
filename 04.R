##함수 생성
formula_dmg <- ff_damage ~  fr_chim_area+fr_whal_area + fr_hon_area + avg_altitude + 
   efhumi_spring
train_up_30_dmg$ff_damage<-as.factor(train_up_30_dmg$ff_damage)
str(train_up_30_dmg$ff_damage)


#####################################################################################################    



## 로지스틱 회귀분석
glm_dmg <- glm(formula_dmg, data = train_up_30_dmg, family = 'binomial')
summary(glm_dmg)
glm_step_dmg <- step(glm_dmg)
#formula<-formula(glm_step_dmg)
summary(glm_step_dmg)
lrtest(glm_step_dmg) #우도비 검정
vif(glm_step_dmg) #다중공선성 진단

## 로지스틱 회귀분석 모델 평가
### confusion matrix (정분류율)
glm_prob_dmg <- predict(glm_step_dmg, newdata = test_30_dmg, type = "response")
glm_pred_dmg <- ifelse(glm_prob_dmg > 0.60, "Y", "N")
glm_pred_dmg <- as.factor(glm_pred_dmg)
str(test_30_dmg$ff_damage)
confusionMatrix(glm_pred_dmg, test_30_dmg$ff_damage,positive="Y")

##오류 다 N로 예측
### ROC curve (민감도, 특이도)
glm_roc_dmg <- roc.curve(test_30_dmg$ff_damage, glm_prob_dmg, col = 2, lwd = 2, lty = 1)$auc
paste("Area under the Curve", ":", round(glm_roc_dmg, 4))
table(test_30_dmg$ff_damage)

library(pROC)
glm_pred_dmg<-ifelse(glm_pred_dmg=="Y",1,0)
test_30_dmg$ff_damage<-ifelse(test_30_dmg$ff_damage=="Y",1,0)
auc(test_30_dmg$ff_damage,glm_pred_dmg)

### F1-score (정밀도, 재현율)
glm_pred_dmg<-as.factor(glm_pred_dmg)
test_30_dmg$ff_damage<-as.factor(test_30_dmg$ff_damage)
glm_precision_dmg <- posPredValue(glm_pred_dmg, test_30_dmg$ff_damage)
glm_recall_dmg <- sensitivity(glm_pred_dmg, test_30_dmg$ff_damage)
glm_F1_dmg <- (2 * glm_precision_dmg * glm_recall_dmg) / (glm_precision_dmg + glm_recall_dmg)
paste("F1-score", ":", round(glm_F1_dmg, 4))

### 이항편차 
y_obs_dmg <- ifelse(test_30_dmg$ff_damage == "Y", 1, 0)
yhat_glm_dmg <- ifelse(glm_pred_dmg == "Y", 1, 0)
(bin_dev_glm_dmg <- binomial_deviance(y_obs_dmg, yhat_glm_dmg))

#####################################################################################################

## 의사결정나무 분석
rpartmod_dmg <- rpart(formula_dmg, data = train_up_30_dmg)
rpart.plot(rpartmod_dmg, cex = 0.8)
printcp(rpartmod_dmg)
plotcp(rpartmod_dmg)

## 의사결정나무 분석 모델 평가
### confusion matrix (정분류율)
p.rpartmod_dmg <- prune(rpartmod_dmg, cp = rpartmod_dmg$cptable[which.min(rpartmod_dmg$cptable[, "xerror"]), "CP"])
rpart.plot(p.rpartmod_dmg, cex = 0.8)
rpart_pred_dmg <- predict(p.rpartmod_dmg, test_30_dmg, type = 'class')
table(rpart_pred_dmg)

str(test_30_dmg$ff_damage)
test_30_dmg$ff_damage<-as.factor(test_30_dmg$ff_damage)
levels(test_30_dmg$ff_damage)<-c('N','Y')
head(test_30_dmg$ff_damage)

confusionMatrix(rpart_pred_dmg, test_30_dmg$ff_damage)


### ROC curve (민감도, 특이도)
rpart_prob_dmg <- predict(p.rpartmod_dmg, test_30_dmg, type = "prob")[,2]
rpart_roc_dmg <- roc.curve(test_30_dmg$ff_damage, rpart_prob_dmg, col = 2, lwd = 2, lty = 1)$auc
paste("Area under the Curve", ":", round(rpart_roc_dmg, 4))

### F1-score (정밀도, 재현율)
rpart_precision_dmg <- posPredValue(rpart_pred_dmg, test_30_dmg$ff_damage)
rpart_recall_dmg <- sensitivity(rpart_pred_dmg, test_30_dmg$ff_damage)
rpart_F1_dmg <- (2 * rpart_precision_dmg * rpart_recall_dmg) / (rpart_precision_dmg + rpart_recall_dmg)
paste("F1-score", ":", round(rpart_F1_dmg, 4))

### 이항편차
y_obs_dmg <- ifelse(test_30_dmg$ff_damage == "Y", 1, 0)
yhat_rpart_dmg <- ifelse(rpart_pred_dmg == "Y", 1, 0)
(bin_dev_rpart_dmg <- binomial_deviance(y_obs_dmg, yhat_rpart_dmg))

#####################################################################################################

## 그레디언트 부스팅 모델
train_up_30_dmg$ff_damage <- ifelse(train_up_30_dmg$ff_damage == "Y", 1, 0)
gbm_model_dmg <- gbm(formula_dmg, data = train_up_30_dmg, distribution = "bernoulli", n.trees = 5000)
gbm_model_dmg
summary(gbm_model_dmg, cBars = 12, las = 2)

## 그레디언트 부스팅 모델 평가
# confusion matrix (정분류율)
gbm_prob_dmg <- predict(gbm_model_dmg, newdata = test_30_dmg, n.trees = 5000, type = "response")
gbm_pred_dmg <- ifelse(gbm_prob_dmg > 0.60, "Y", "N")
gbm_pred_dmg <- as.factor(gbm_pred_dmg)
confusionMatrix(gbm_pred_dmg, test_30_dmg$ff_damage)

### ROC curve (민감도, 특이도)
gbm_roc_dmg <- roc.curve(test_30_dmg$ff_damage, gbm_prob_dmg, col = 2, lwd = 2, lty = 1)$auc
paste("Area under the Curve", ":", round(gbm_roc_dmg, 4))

### F1-score (정밀도, 재현율)
gbm_precision_dmg <- posPredValue(gbm_pred_dmg, test_30_dmg$ff_damage)
gbm_recall_dmg <- sensitivity(gbm_pred_dmg, test_30_dmg$ff_damage)
gbm_F1_dmg <- (2 * gbm_precision_dmg * gbm_recall_dmg) / (gbm_precision_dmg + gbm_recall_dmg)
paste("F1-score", ":", round(gbm_F1_dmg, 4))

### 이항편차 
y_obs_dmg <- ifelse(test_30_dmg$ff_damage == "Y", 1, 0)
yhat_gbm_dmg <- ifelse(gbm_pred_dmg == "Y", 1, 0)
(bin_dev_gbm_dmg <- binomial_deviance(y_obs_dmg, yhat_gbm_dmg))
train_up_30_dmg$ff_damage <- ifelse(train_up_30_dmg$ff_damage == 1, "Y", "N")
train_up_30_dmg$ff_damage <- as.factor(train_up_30_dmg$ff_damage)

#####################################################################################################

## 최종 모델 비교 및 선택
result_model_dmg <- data.frame(method = c("rpart", "glm", "gbm"),
                               auc = c(rpart_roc_dmg, glm_roc_dmg, gbm_roc_dmg),
                               f.score = c(rpart_F1_dmg, glm_F1_dmg, gbm_F1_dmg),
                               bin_dec = c(bin_dev_rpart_dmg, bin_dev_glm_dmg, bin_dev_gbm_dmg))
print(result_model_dmg)

# 시각화
roc.curve(test_30_dmg$ff_damage, rpart_prob_dmg, col = "red", lwd = 2, lty = 1)$auc
roc.curve(test_30_dmg$ff_damage, glm_prob_dmg, add = T, col = "blue", lwd = 2, lty = 1)$auc
roc.curve(test_30_dmg$ff_damage, gbm_prob_dmg, add = T, col = "green", lwd = 2, lty = 1)$auc
abline(0,1)
legend("bottomright", inset = .1,
       legend = c("의사결정나무", "로지스틱회귀분석", "그레디언트 부스팅"),
       col = c("red", "blue", "green"), lty = 1, lwd = 2)

