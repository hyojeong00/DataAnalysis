##감시자원의 반경 4km 이내 격자 제외 (반경 수정 가능)
all_table$cctv_view_dist[is.na(all_table$cctv_view_dist)] <- 999999
all_table$cctv_close_dist[is.na(all_table$cctv_close_dist)] <- 999999
all_table$tower_dist[is.na(all_table$tower_dist)] <- 999999
all_table$choso_dist[is.na(all_table$choso_dist)] <- 999999

all_table$gamsi_min <- ifelse(all_table$cctv_view_dist <= all_table$cctv_close_dist, all_table$cctv_view_dist, all_table$cctv_close_dist)
all_table$gamsi_min <- ifelse(all_table$tower_dist <= all_table$gamsi_min, all_table$tower_dist, all_table$gamsi_min)
all_table$gamsi_min <- ifelse(all_table$choso_dist <= all_table$gamsi_min, all_table$choso_dist, all_table$gamsi_min)

all_table$gamsi_YN <- ifelse(all_table$gamsi_min <= 4000, "Y", "N")
all_table_gamsi <- all_table[all_table$gamsi_YN == "N", ]

#####################################################################################################


## 감시자원 배치 추천(산불발생80%, 산불확산10%, 문화재소실10% 가중치 적용)
all_table_gamsi$grade_guide <- (all_table_gamsi$grade_ff_risk * 0.8) + (all_table_gamsi$grade_dmg_risk * 0.1) + (all_table_gamsi$grade_mun * 0.1)

## 감시자원 배치 등급화 (6등급)
all_table_gamsi <- all_table_gamsi %>% mutate(grade_guide_6 = rank(grade_guide)/nrow(all_table_gamsi)) %>% 
  mutate(grade_guide_6 = ifelse(grade_guide_6 >= 0.95, "6.매우높음",
                                ifelse(grade_guide_6 >= 0.80 & grade_guide_6 <0.95, "5.높음",
                                       ifelse(grade_guide_6 >= 0.50 & grade_guide_6 <0.80, "4.다소높음",   
                                              ifelse(grade_guide_6 >= 0.20 & grade_guide_6 <0.50, "3.다소낮음",
                                                     ifelse(grade_guide_6 >= 0.05 & grade_guide_6 <0.20, "2.낮음", "1,매우낮음")))))) 

all_table_gamsi <- all_table_gamsi %>% mutate(grade_guide_num = rank(grade_guide)/nrow(all_table_gamsi)) %>% 
  mutate(grade_guide_num = ifelse(grade_guide_num >= 0.95, "6",
                                  ifelse(grade_guide_num >= 0.80 & grade_guide_num <0.95, "5",
                                         ifelse(grade_guide_num >= 0.50 & grade_guide_num <0.80, "4",   
                                                ifelse(grade_guide_num >= 0.20 & grade_guide_num <0.50, "3",
                                                       ifelse(grade_guide_num >= 0.05 & grade_guide_num <0.20, "2", "1")))))) 
table(all_table_gamsi$grade_guide_6)
all_table_guide <- all_table_gamsi[,c(1, 30:32)] 
str(all_table_guide)
View(all_table_guide) 


## 변수명 변경 및 결과 저장
grid_result_guide <- merge(dataset_ffp_30, all_table_guide, by = "gid_nm", all = F)

colnames(grid_result_guide)[colnames(grid_result_guide) == "grade_guide"] <- "배치추천"
colnames(grid_result_guide)[colnames(grid_result_guide) == "grade_guide_6"] <- "배치추천_6"
colnames(grid_result_guide)[colnames(grid_result_guide) == "grade_guide_num"] <- "배치추천_숫자"

write.csv(grid_result_guide, "ffp_ana_result_guide.csv", row.names = F)

