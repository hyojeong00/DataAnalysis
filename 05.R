##산불발생위험 모델
## cutoff 결정 
(grid <- expand.grid(cut = seq(0.01, by = 0.01)))

cutoff_result <- foreach(g = 1:NROW(grid), .combine = rbind) %do% {
  
  prob <- predict(glm_step, newdata = test_30, type = "response")
  pred <- ifelse(prob > grid[g, "cut"], "Y", "N")
  pred <- as.factor(pred)
  
  matrix <- confusionMatrix(pred, test_30$ff)
  accuracy <- matrix$overall[1]
  sensitivity <- matrix$byClass[1]
  specificity <- matrix$byClass[2]
  auc <- roc.curve(test_30$ff, pred, plotit = F)
  
  return(data.table(g,
                    accuracy = round(accuracy, 4),
                    auc = auc$auc,
                    sensitivity = round(sensitivity, 4),
                    specificity = round(specificity, 4)))
}
cutoff_result <- data.frame(grid, cutoff_result)


## 최종 산불발생위험 모델 평가 (cutoff값 적용)
## confustion matrix (정분류율)
prob_result <- predict(glm_step, newdata = dataset_ffp_30, type = "response")
pred_result <- ifelse(prob_result > 0.55, "Y", "N")                         #cutoff값에 맞춰 수정
pred_result <- as.factor(pred_result)
(matrix <- confusionMatrix(pred_result, dataset_ffp_30$ff))

## ROC curve (민감도, 특이도)
(roc_result <- roc.curve(dataset_ffp_30$ff, prob_result, col = 2, lwd = 2, lty = 1)$auc)
legend("bottomright", inset = .1, cex = 1.5,
       legend = c("Area Under Curve : 0.65"),   #auc값 입력
       col = "red", lty = 1, lwd = 2)

## F1-score (정밀도, 재현율)
precision_result <- posPredValue(pred_result, dataset_ffp_30$ff)
recall_result <- sensitivity(pred_result, dataset_ffp_30$ff)
(F1_result <- (2 * precision_result * recall_result) / (precision_result + recall_result))

## 이항편차
y_obs <- ifelse(dataset_ffp_30$ff == "Y", 1, 0)
yhat_result <- ifelse(pred_result == "Y", 1, 0)
(bin.dev_result <- binomial_deviance(y_obs, yhat_result))

#####################################################################################################


##산불확산위험 모델
## cutoff 결정
(grid <- expand.grid(cut = seq(0.01, by = 0.01)))

cutoff_result_dmg <- foreach(g = 1:NROW(grid), .combine = rbind) %do% {
  
  prob <- predict(glm_step_dmg, newdata = test_30_dmg, type = "response")
  pred <- ifelse(prob > grid[g, "cut"], "Y", "N")
  pred <- as.factor(pred)
  
  matrix <- confusionMatrix(pred, test_30_dmg$ff_damage)
  accuracy <- matrix$overall[1]
  sensitivity <- matrix$byClass[1]
  specificity <- matrix$byClass[2]
  auc <- roc.curve(test_30_dmg$ff_damage, pred, plotit = F)
  
  return(data.table(g,
                    accuracy = round(accuracy, 4),
                    auc = auc$auc,
                    sensitivity = round(sensitivity, 4),
                    specificity = round(specificity, 4)))
}

cutoff_result_dmg <- data.frame(grid, cutoff_result_dmg)


## 최종 산불확산위험 모델 평가 (cutoff 값 적용)
## confustion matrix (정분류율)
prob_result_dmg <- predict(glm_step_dmg, newdata = dataset_ffp_30, type = "response")
pred_result_dmg <- ifelse(prob_result_dmg > 0.60, "Y", "N")                          #cutoff값에 맞춰 수정
pred_result_dmg <- as.factor(pred_result_dmg)
(matrix_dmg <- confusionMatrix(pred_result_dmg, dataset_ffp_30$ff_damage))

## ROC curve (민감도, 특이도)
(roc_result_dmg <- roc.curve(dataset_ffp_30$ff_damage, prob_result_dmg, col = 2, lwd = 2, lty = 1)$auc)
legend("bottomright", inset = .1, cex = 1.5,
       legend = c("Area Under Curve : 0.64"),   #auc값 입력
       col = "red", lty = 1, lwd = 2)

## F1-score (정밀도, 재현율)
precision_result_dmg <- posPredValue(pred_result_dmg, dataset_ffp_30$ff_damage)
recall_result_dmg <- sensitivity(pred_result_dmg, dataset_ffp_30$ff_damage)
(F1_result_dmg <- (2 * precision_result_dmg * recall_result_dmg) / (precision_result_dmg + recall_result_dmg))

## 이항편차
y_obs_dmg <- ifelse(dataset_ffp_30$ff_damage == "Y", 1, 0)
yhat_result_dmg <- ifelse(pred_result_dmg == "Y", 1, 0)
(bin.dev_result_dmg <- binomial_deviance(y_obs_dmg, yhat_result_dmg))

