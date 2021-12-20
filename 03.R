## 로지스틱 회귀분석
glm <- glm(formula, data = train_up_30, family = binomial)
summary(glm)
glm_step <- step(glm,direction="both")
summary(glm_step)
lrtest(glm_step)  #우도비 검정
vif(glm_step) #다중공선성 진단
#avg_altitude vif 4이상-> 제거
glm<-glm(ff ~ age65_ + fr_chim_area + fr_whal_area + fr_hon_area + trail_length + non_dist + bat_dist + myo_dist + 
           avg_altitude + mun_cnt +efhumi_spring + wind_spring, data=train_up_30,family=binomial)
summary(glm)
glm_step <- step(glm,direction="both")
summary(glm_step)
lrtest(glm_step)  #우도비 검정
vif(glm_step) #다중공선성 진단
#glm<-glm(ff~.,data=train_up_30,family=binomial)
#summary(glm)
#glm_step<-step(glm,direction="both")
#formula<-formula(glm_step)
#formula
#summary(glm_step)
#lrtest(glm_step)  #우도비, 두 가설이 유의미하게 차이 나는지
#vif(glm_step)


## 로지스틱 회귀분석 모델 평가
### confusion matrix (정분류율)
glm_prob <- predict(glm_step, newdata = test_30, type = "response")
glm_pred <- ifelse(glm_prob > 0.60, "Y", "N")
glm_pred <- as.factor(glm_pred)
confusionMatrix(glm_pred, test_30$ff, positive = "Y")

### ROC curve (민감도, 특이도)
glm_roc <- roc.curve(test_30$ff, glm_prob, col = 2, lwd = 2, lty = 1)$auc
paste("Area under the Curve", ":", round(glm_roc, 4))

## F1-score (정밀도, 재현율)
glm_precision <- posPredValue(glm_pred, test_30$ff)
glm_recall <- sensitivity(glm_pred, test_30$ff)
glm_F1 <- (2 * glm_precision * glm_recall) / (glm_precision + glm_recall)
paste("F1-score", ":", round(glm_F1, 4))

### 이항편차
y_obs <- ifelse(test_30$ff == "Y", 1, 0)
yhat_glm <- ifelse(glm_pred == "Y", 1, 0)
(bin_dev_glm <- binomial_deviance(y_obs, yhat_glm))

#####################################################################################################

## 의사결정나무 분석
rpartmod <- rpart(formula, data = train_up_30)
rpart.plot(rpartmod, cex = 0.8)
printcp(rpartmod)
plotcp(rpartmod)

## 의사결정나무 분석 모델 평가
### confusion matrix (정분류율)
p.rpartmod <- prune(rpartmod, cp = rpartmod$cptable[which.min(rpartmod$cptable[, "xerror"]), "CP"])
rpart.plot(p.rpartmod, cex = 0.8)
rpart_pred <- predict(p.rpartmod, test_30, type = 'class')
confusionMatrix(rpart_pred, test_30$ff, positive = "Y")

### ROC curve (민감도, 특이도)
rpart_prob <- predict(p.rpartmod, test_30, type = "prob")[,2]
rpart_roc <- roc.curve(test_30$ff, rpart_prob, col = 2, lwd = 2, lty = 1)$auc
paste("Area under the Curve", ":", round(rpart_roc, 4))

### F1-score (정밀도, 재현율)
rpart_precision <- posPredValue(rpart_pred, test_30$ff)
rpart_recall <- sensitivity(rpart_pred, test_30$ff)
rpart_F1 <- (2 * rpart_precision * rpart_recall) / (rpart_precision + rpart_recall)
paste("F1-score", ":", round(rpart_F1, 4))

### 이항편차
y_obs <- ifelse(test_30$ff == "Y", 1, 0)
yhat_rpart <- ifelse(rpart_pred == "Y", 1, 0)
(bin_dev_rpart <- binomial_deviance(y_obs, yhat_rpart))

#####################################################################################################

## 그레디언트 부스팅 모델
train_up_30$ff <- ifelse(train_up_30$ff == "Y", 1, 0)
gbm_model <- gbm(formula, data = train_up_30, distribution = "bernoulli", n.trees = 5000)
gbm_model
summary(gbm_model, cBars = 12, las = 2)

## 그레디언트 부스팅 모델 평가
# confusion matrix (정분류율)
gbm_prob <- predict(gbm_model, newdata = test_30, n.trees = 5000, type = "response")
gbm_pred <- ifelse(gbm_prob > 0.60, "Y", "N")
gbm_pred <- as.factor(gbm_pred)
confusionMatrix(gbm_pred, test_30$ff, positive="Y")

### ROC curve (민감도, 특이도)
gbm_roc <- roc.curve(test_30$ff, gbm_prob, col = 2, lwd = 2, lty = 1)$auc
paste("Area under the Curve", ":", round(gbm_roc, 4))

### F1-score (정밀도, 재현율)
gbm_precision <- posPredValue(gbm_pred, test_30$ff)
gbm_recall <- sensitivity(gbm_pred, test_30$ff)
gbm_F1 <- (2 * gbm_precision * gbm_recall) / (gbm_precision + gbm_recall)
paste("F1-score", ":", round(gbm_F1, 4))

### 이항편차
y_obs <- ifelse(test_30$ff == "Y", 1, 0)
yhat_gbm <- ifelse(gbm_pred == "Y", 1, 0)
(bin_dev_gbm <- binomial_deviance(y_obs, yhat_gbm))
train_up_30$ff <- ifelse(train_up_30$ff == 1, "Y", "N")
train_up_30$ff <- as.factor(train_up_30$ff)

#####################################################################################################

## 최종 모델 비교 및 선택
result_model <- data.frame(method = c("rpart", "glm", "gbm"),
                           auc = c(rpart_roc, glm_roc, gbm_roc),
                           f.score = c(rpart_F1, glm_F1, gbm_F1),
                           bin_dec = c(bin_dev_rpart, bin_dev_glm, bin_dev_gbm))
print(result_model)

# 시각화
roc.curve(test_30$ff, rpart_prob, col = "red", lwd = 2, lty = 1)$auc
roc.curve(test_30$ff, glm_prob, add = T, col = "blue", lwd = 2, lty = 1)$auc
roc.curve(test_30$ff, gbm_prob, add = T, col = "green", lwd = 2, lty = 1)$auc
abline(0,1)
legend("bottomright", inset = .1,
       legend = c("의사결정나무", "로지스틱회귀분석", "그레디언트 부스팅"),
       col = c("red", "blue", "green"), lty = 1, lwd = 2)

