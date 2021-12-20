## 산불감시우선지수 데이터셋 생성
# 산불발생, 산불확산 예측변수 생성
all_prob <- predict(glm_step, newdata = dataset_ffp_30, type = "response")
all_pred <- ifelse(all_prob > 0.55, "Y", "N")                                      #cutoff 적용
all_pred <- as.factor(all_pred)

all_prob_dmg <- predict(glm_step_dmg, newdata = dataset_ffp_30, type = "response")
all_pred_dmg <- ifelse(all_prob_dmg > 0.60, "Y", "N")                              #cutoff 적용
all_pred_dmg <- as.factor(all_pred_dmg)

# 데이터셋 생성
all_table <- data.table(dataset_ffp_30$gid_nm, dataset_ffp_30$ff, all_pred, all_prob, dataset_ffp_30$ff_damage, all_pred_dmg, all_prob_dmg, 
                        dataset_ffp_30$cctv_view_dist, dataset_ffp_30$cctv_close_dist, dataset_ffp_30$tower_dist, dataset_ffp_30$choso_dist, 
                        dataset_ffp_30$mun_kuk_dist, dataset_ffp_30$mun_bo_dist, dataset_ffp_30$mun_etc_dist)
names(all_table) <- c("gid_nm", "ff_real", "ff_pred", "ff_prob", "dmg_real", "dmg_pred", "dmg_prob", 
                      "cctv_view_dist", "cctv_close_dist", "tower_dist", "choso_dist", "mun_kuk_dist","mun_bo_dist", "mun_etc_dist")
all_table$gid_nm <- as.character(all_table$gid_nm)

#####################################################################################################

##산불발생 위험지수 1~10 (산불발생 확률이 높을수록 지수가 높음) -> 절대값 
all_table$ff_prob <- round(all_table$ff_prob, 4)
all_table[ff_prob <= 1.0, grade_ff_risk := 10]
all_table[ff_prob <= 0.9, grade_ff_risk := 9]
all_table[ff_prob <= 0.8, grade_ff_risk := 8]
all_table[ff_prob <= 0.7, grade_ff_risk := 7]
all_table[ff_prob <= 0.6, grade_ff_risk := 6]
all_table[ff_prob <= 0.5, grade_ff_risk := 5]
all_table[ff_prob <= 0.4, grade_ff_risk := 4]
all_table[ff_prob <= 0.3, grade_ff_risk := 3]
all_table[ff_prob <= 0.2, grade_ff_risk := 2]
all_table[ff_prob <= 0.1, grade_ff_risk := 1]

#####################################################################################################

##산불확산 위험지수 1~10 (산불확대 확률이 높을수록 지수가 높음) -> 절대값
all_table$dmg_prob <- round(all_table$dmg_prob, 4)
all_table[dmg_prob <= 1.0, grade_dmg_risk := 10]
all_table[dmg_prob <= 0.9, grade_dmg_risk := 9]
all_table[dmg_prob <= 0.8, grade_dmg_risk := 8]
all_table[dmg_prob <= 0.7, grade_dmg_risk := 7]
all_table[dmg_prob <= 0.6, grade_dmg_risk := 6]
all_table[dmg_prob <= 0.5, grade_dmg_risk := 5]
all_table[dmg_prob <= 0.4, grade_dmg_risk := 4]
all_table[dmg_prob <= 0.3, grade_dmg_risk := 3]
all_table[dmg_prob <= 0.2, grade_dmg_risk := 2]
all_table[dmg_prob <= 0.1, grade_dmg_risk := 1]

#####################################################################################################

##감시자원 취약지수 1~10 (감시자원과의 거리가 멀수록 위험지수가 높음) -> 상대값
#10분위수 생성
grade_view_percent <- quantile(all_table$cctv_view_dist, prob = seq(0.1, 1, 0.1))
#grade_close_percent <- quantile(all_table$cctv_close_dist, prob = seq(0.1, 1, 0.1))
#grade_tower_percent <- quantile(all_table$tower_dist, prob = seq(0.1, 1, 0.1))
#grade_choso_percent <- quantile(all_table$choso_dist, prob = seq(0.1, 1, 0.1))

#조망형카메라와의 거리 등급
all_table[cctv_view_dist <= grade_view_percent[10], grade_view_gamsi := 10]
all_table[cctv_view_dist <= grade_view_percent[9], grade_view_gamsi := 9]
all_table[cctv_view_dist <= grade_view_percent[8], grade_view_gamsi := 8]
all_table[cctv_view_dist <= grade_view_percent[7], grade_view_gamsi := 7]
all_table[cctv_view_dist <= grade_view_percent[6], grade_view_gamsi := 6]
all_table[cctv_view_dist <= grade_view_percent[5], grade_view_gamsi := 5]
all_table[cctv_view_dist <= grade_view_percent[4], grade_view_gamsi := 4]
all_table[cctv_view_dist <= grade_view_percent[3], grade_view_gamsi := 3]
all_table[cctv_view_dist <= grade_view_percent[2], grade_view_gamsi := 2]
all_table[cctv_view_dist <= grade_view_percent[1], grade_view_gamsi := 1]

comment="
#밀착형카메라와의 거리 등급
all_table[cctv_close_dist <= grade_close_percent[10], grade_close_gamsi := 10]
all_table[cctv_close_dist <= grade_close_percent[9], grade_close_gamsi := 9]
all_table[cctv_close_dist <= grade_close_percent[8], grade_close_gamsi := 8]
all_table[cctv_close_dist <= grade_close_percent[7], grade_close_gamsi := 7]
all_table[cctv_close_dist <= grade_close_percent[6], grade_close_gamsi := 6]
all_table[cctv_close_dist <= grade_close_percent[5], grade_close_gamsi := 5]
all_table[cctv_close_dist <= grade_close_percent[4], grade_close_gamsi := 4]
all_table[cctv_close_dist <= grade_close_percent[3], grade_close_gamsi := 3]
all_table[cctv_close_dist <= grade_close_percent[2], grade_close_gamsi := 2]
all_table[cctv_close_dist <= grade_close_percent[1], grade_close_gamsi := 1]

#감시탑과의 거리 등급
all_table[tower_dist <= grade_tower_percent[10], grade_tower_gamsi := 10]
all_table[tower_dist <= grade_tower_percent[9], grade_tower_gamsi := 9]
all_table[tower_dist <= grade_tower_percent[8], grade_tower_gamsi := 8]
all_table[tower_dist <= grade_tower_percent[7], grade_tower_gamsi := 7]
all_table[tower_dist <= grade_tower_percent[6], grade_tower_gamsi := 6]
all_table[tower_dist <= grade_tower_percent[5], grade_tower_gamsi := 5]
all_table[tower_dist <= grade_tower_percent[4], grade_tower_gamsi := 4]
all_table[tower_dist <= grade_tower_percent[3], grade_tower_gamsi := 3]
all_table[tower_dist <= grade_tower_percent[2], grade_tower_gamsi := 2]
all_table[tower_dist <= grade_tower_percent[1], grade_tower_gamsi := 1]

#감시초소와의 거리 등급
all_table[choso_dist <= grade_choso_percent[10], grade_choso_gamsi := 10]
all_table[choso_dist <= grade_choso_percent[9], grade_choso_gamsi := 9]
all_table[choso_dist <= grade_choso_percent[8], grade_choso_gamsi := 8]
all_table[choso_dist <= grade_choso_percent[7], grade_choso_gamsi := 7]
all_table[choso_dist <= grade_choso_percent[6], grade_choso_gamsi := 6]
all_table[choso_dist <= grade_choso_percent[5], grade_choso_gamsi := 5]
all_table[choso_dist <= grade_choso_percent[4], grade_choso_gamsi := 4]
all_table[choso_dist <= grade_choso_percent[3], grade_choso_gamsi := 3]
all_table[choso_dist <= grade_choso_percent[2], grade_choso_gamsi := 2]
all_table[choso_dist <= grade_choso_percent[1], grade_choso_gamsi := 1]
"
##해당 감시자원이 1개도 없는 경우
#all_table$grade_view_gamsi <- 10
all_table$grade_close_gamsi <- 10
all_table$grade_tower_gamsi <- 10
all_table$grade_choso_gamsi <- 10

#조망형 30%, 밀착형 20%, 감시탑 30%, 감시초소20% 가중치 적용하여 10점 변환
all_table$grade_gamsi <- (all_table$grade_view_gamsi * 0.3) + (all_table$grade_close_gamsi * 0.2) + (all_table$grade_tower_gamsi * 0.3) + (all_table$grade_choso_gamsi * 0.2)

#####################################################################################################

##문화재소실 위험지수 1~10 (문화재와의 거리가 가까울수록 위험지수가 높음) -> 상대값
#10분위수 생성
grade_kuk_percent <- quantile(all_table$mun_kuk_dist, prob = seq(0.1, 1, 0.1))
grade_bo_percent <- quantile(all_table$mun_bo_dist, prob = seq(0.1, 1, 0.1))
grade_etc_percent <- quantile(all_table$mun_etc_dist, prob = seq(0.1, 1, 0.1))

#국보문화재와의 거리 등급
all_table[mun_kuk_dist <= grade_kuk_percent[10], grade_kuk := 1]
all_table[mun_kuk_dist <= grade_kuk_percent[9], grade_kuk := 2]
all_table[mun_kuk_dist <= grade_kuk_percent[8], grade_kuk := 3]
all_table[mun_kuk_dist <= grade_kuk_percent[7], grade_kuk := 4]
all_table[mun_kuk_dist <= grade_kuk_percent[6], grade_kuk := 5]
all_table[mun_kuk_dist <= grade_kuk_percent[5], grade_kuk := 6]
all_table[mun_kuk_dist <= grade_kuk_percent[4], grade_kuk := 7]
all_table[mun_kuk_dist <= grade_kuk_percent[3], grade_kuk := 8]
all_table[mun_kuk_dist <= grade_kuk_percent[2], grade_kuk := 9]
all_table[mun_kuk_dist <= grade_kuk_percent[1], grade_kuk := 10]

#보물문화재와의 거리 등급
all_table[mun_bo_dist <= grade_bo_percent[10], grade_bo := 1]
all_table[mun_bo_dist <= grade_bo_percent[9], grade_bo := 2]
all_table[mun_bo_dist <= grade_bo_percent[8], grade_bo := 3]
all_table[mun_bo_dist <= grade_bo_percent[7], grade_bo := 4]
all_table[mun_bo_dist <= grade_bo_percent[6], grade_bo := 5]
all_table[mun_bo_dist <= grade_bo_percent[5], grade_bo := 6]
all_table[mun_bo_dist <= grade_bo_percent[4], grade_bo := 7]
all_table[mun_bo_dist <= grade_bo_percent[3], grade_bo := 8]
all_table[mun_bo_dist <= grade_bo_percent[2], grade_bo := 9]
all_table[mun_bo_dist <= grade_bo_percent[1], grade_bo := 10]

#기타문화재와의 거리 등급
all_table[mun_etc_dist <= grade_etc_percent[10], grade_etc := 1]
all_table[mun_etc_dist <= grade_etc_percent[9], grade_etc := 2]
all_table[mun_etc_dist <= grade_etc_percent[8], grade_etc := 3]
all_table[mun_etc_dist <= grade_etc_percent[7], grade_etc := 4]
all_table[mun_etc_dist <= grade_etc_percent[6], grade_etc := 5]
all_table[mun_etc_dist <= grade_etc_percent[5], grade_etc := 6]
all_table[mun_etc_dist <= grade_etc_percent[4], grade_etc := 7]
all_table[mun_etc_dist <= grade_etc_percent[3], grade_etc := 8]
all_table[mun_etc_dist <= grade_etc_percent[2], grade_etc := 9]
all_table[mun_etc_dist <= grade_etc_percent[1], grade_etc := 10]

##해당 문화재가 1개도 없는 경우
#all_table$grade_kuk <- 1
#all_table$grade_bo <- 1
#all_table$grade_etc <- 1

#국보60%, 보물30%, 기타10% 가중치 적용하여 10점 변환
all_table$grade_mun <- (all_table$grade_kuk * 0.6) + (all_table$grade_bo * 0.3) + (all_table$grade_etc * 0.1)

#####################################################################################################

## 최종 산불감시 우선지수 생성(산불발생40%, 산불확산10%, 감시취약40%, 문화재소실10% 가중치 적용)
all_table$grade_result <- (all_table$grade_ff_risk * 0.4) + (all_table$grade_dmg_risk * 0.1) + (all_table$grade_gamsi * 0.4) + (all_table$grade_mun * 0.1)

## 최종 산불감시 우선지수 등급화(6등급)
all_table <- all_table %>% mutate(grade_result_6 = rank(grade_result)/nrow(all_table)) %>% 
  mutate(grade_result_6 = ifelse(grade_result_6 >= 0.95, "6.매우높음",
                                 ifelse(grade_result_6 >= 0.80 & grade_result_6 <0.95, "5.높음",
                                        ifelse(grade_result_6 >= 0.50 & grade_result_6 <0.80, "4.다소높음",   
                                               ifelse(grade_result_6 >= 0.20 & grade_result_6 <0.50, "3.다소낮음",
                                                      ifelse(grade_result_6 >= 0.05 & grade_result_6 <0.20, "2.낮음", "1,매우낮음")))))) 
table(all_table$grade_result_6)
all_table_result <- all_table[,c(1, 15:27)] 
str(all_table_result)
View(all_table_result) 


## 변수명 변경 및 결과 저장
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "age0_200"] <- "전체인구"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "age_65"] <- "65세이하"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "age65_"] <- "65세이상"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "fr_etc_area"] <- "기타산림면적"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "fr_chim_area"] <- "침엽수림면적"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "fr_whal_area"] <- "활엽수림면적"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "fr_hon_area"] <- "혼효림면적"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "fr_juk_area"] <- "죽림면적"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "trail_length"] <- "등산로길이"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "myo_dist"] <- "묘거리"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "non_dist"] <- "논거리"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "bat_dist"] <- "밭거리"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "avg_altitude"] <- "평균고도"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "cctv_view_dist"] <- "조망형거리"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "cctv_close_dist"] <- "밀착형거리"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "tower_dist"] <- "감시탑거리"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "choso_dist"] <- "초소거리"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "mun_cnt"] <- "문화재수"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "mun_dist"] <- "문화재거리"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "mun_kuk_dist"] <- "국보거리"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "mun_bo_dist"] <- "보물거리"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "mun_etc_dist"] <- "기타거리"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "temp_spring"] <- "봄평균기온"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "humi_spring"] <- "봄평균습도"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "wind_spring"] <- "봄평균풍속"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "efhumi_spring"] <- "봄평균실습도"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "ff"] <- "산불발생여부"
colnames(dataset_ffp_30)[colnames(dataset_ffp_30) == "ff_damage"] <- "산불확산여부"

grid_result <- merge(dataset_ffp_30, all_table_result, by = "gid_nm", all = F)

colnames(grid_result)[colnames(grid_result) == "grade_ff_risk"] <- "발생위험지수"
colnames(grid_result)[colnames(grid_result) == "grade_dmg_risk"] <- "확산위험지수"
colnames(grid_result)[colnames(grid_result) == "grade_view_gamsi"] <- "조망형등급"
colnames(grid_result)[colnames(grid_result) == "grade_close_gamsi"] <- "밀착형등급"
colnames(grid_result)[colnames(grid_result) == "grade_tower_gamsi"] <- "감시탑등급"
colnames(grid_result)[colnames(grid_result) == "grade_choso_gamsi"] <- "감시초소등급"
colnames(grid_result)[colnames(grid_result) == "grade_gamsi"] <- "감시취약지수"
colnames(grid_result)[colnames(grid_result) == "grade_kuk"] <- "국보등급"
colnames(grid_result)[colnames(grid_result) == "grade_bo"] <- "보물등급"
colnames(grid_result)[colnames(grid_result) == "grade_etc"] <- "기타등급"
colnames(grid_result)[colnames(grid_result) == "grade_mun"] <- "문화재지수"
colnames(grid_result)[colnames(grid_result) == "grade_result"] <- "최종지수"


colnames(grid_result)[colnames(grid_result) == "grade_result_6"] <- "최종지수_6"

write.csv(grid_result, "ffp_ana_result.csv", row.names = F)

