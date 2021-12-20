##데이터 불러오기
ana_grid<-read.csv('ffp_ana_grid6.csv',header=T)

sum(is.na(ana_grid))
#ana_grid$ff_damage_area<-ifelse(ana_grid$ff_cnt>=1&ana_grid$ff_damage_area==0,
#                                mean(ana_grid$ff_damage_area,na.rm=T),ana_grid$ff_damage_area)
ana_grid$fr_etc_area[is.na(ana_grid$fr_etc_area)] <- 0
ana_grid$fr_hon_area[is.na(ana_grid$fr_hon_area)] <- 0
ana_grid$fr_juk_area[is.na(ana_grid$fr_juk_area)] <- 0
ana_grid$fr_chim_area[is.na(ana_grid$fr_chim_area)] <- 0
ana_grid$fr_whal_area[is.na(ana_grid$fr_whal_area)] <- 0
ana_grid$trail_cnt[is.na(ana_grid$trail_cnt)] <- 0
ana_grid$trail_length[is.na(ana_grid$trail_length)] <- 0
ana_grid$inter_time_sum[is.na(ana_grid$inter_time_sum)] <- 0
ana_grid$cctv_close_dist[is.na(ana_grid$cctv_close_dist)] <- 0
ana_grid$tower_dist[is.na(ana_grid$tower_dist)] <- 0
ana_grid$choso_dist[is.na(ana_grid$choso_dist)] <- 0
ana_grid$wthr_sttn_no[is.na(ana_grid$wthr_sttn_no)] <- 0
ana_grid$val[is.na(ana_grid$val)] <- 0
sum(is.na(ana_grid))
summary(ana_grid)
#write.csv(ana_grid,'a.csv',row.names = F)
## 산림 30% 이상 격자 데이터셋 만들기 
ana_grid$forest_area <- (ana_grid$fr_etc_area + ana_grid$fr_chim_area + 
                           ana_grid$fr_whal_area + ana_grid$fr_hon_area + 
                           ana_grid$fr_juk_area) / 10000
ana_grid$forest_30 <- ifelse(ana_grid$forest_area >= 30 | 
                               ana_grid$ff_damage_area > 0, "Y", "N")
ana_grid_30 <- ana_grid[ana_grid$forest_30 == "Y", ]


## 변수명 바꾸기
colnames(ana_grid_30)[2] <- "gid_nm"


## 거리, 면적 변수 -> 소수점 셋째자리로 반올림
ana_grid_30[,c("fr_etc_area", "fr_chim_area", "fr_whal_area", "fr_hon_area", "fr_juk_area", "trail_length", "myo_dist", "non_dist", "bat_dist",
               "cctv_view_dist", "cctv_close_dist", "tower_dist", "choso_dist", "mun_dist", "mun_kuk_dist", "mun_bo_dist", "mun_etc_dist" )] <- round(ana_grid_30[,c("fr_etc_area", 
                                                                                                                                                                     "fr_chim_area", "fr_whal_area", "fr_hon_area", "fr_juk_area", "trail_length", "myo_dist", "non_dist", "bat_dist", "cctv_view_dist", "cctv_close_dist", "tower_dist", 
                                                                                                                                                                     "choso_dist", "mun_dist", "mun_kuk_dist", "mun_bo_dist", "mun_etc_dist")], 3)


## 산불발생 건수, 산불피해 면적 변수 범주화
ana_grid_30$ff <- ifelse(ana_grid_30$ff_cnt > 0, "Y", "N")
ana_grid_30$ff <- as.factor(ana_grid_30$ff)
ana_grid_30$ff_damage <- ifelse(ana_grid_30$ff_damage_area >0, "Y", "N")
ana_grid_30$ff_damage <- as.factor(ana_grid_30$ff_damage)


## 봄평균 기상변수 생성 (3~5월)
ana_grid_30$temp_spring <- (ana_grid_30$temp_mar + ana_grid_30$temp_apr + ana_grid_30$temp_may) / 3
ana_grid_30$humi_spring <- (ana_grid_30$humi_mar + ana_grid_30$humi_apr + ana_grid_30$humi_may) / 3
ana_grid_30$efhumi_spring <- (ana_grid_30$efhumi_mar + ana_grid_30$efhumi_apr + ana_grid_30$efhumi_may) / 3
ana_grid_30$wind_spring <- (ana_grid_30$wind_mar + ana_grid_30$wind_apr + ana_grid_30$wind_may) / 3
ana_grid_30[,c("temp_spring", "humi_spring", "wind_spring", "efhumi_spring")] <- round(ana_grid_30[,c("temp_spring", "humi_spring", "wind_spring", "efhumi_spring")], 3)



## 모델링 데이터셋 생성
dataset_ffp_30 <- ana_grid_30[, c("gid_nm", "age0_200", "age_65", "age65_", "fr_etc_area", "fr_chim_area", "fr_whal_area", "fr_hon_area", "fr_juk_area", 
                                  "trail_length", "myo_dist", "non_dist", "bat_dist", "avg_altitude", "cctv_view_dist", "cctv_close_dist",
                                  "tower_dist", "choso_dist", "mun_cnt", "mun_kuk_dist", "mun_bo_dist", "mun_etc_dist", "temp_spring", "humi_spring", 
                                  "efhumi_spring", "wind_spring", "ff", "ff_damage")]
## EDA분석 데이터셋 생성
dataset_ffp2_30 <- ana_grid_30[, c("age0_200", "age_65", "age65_", "fr_etc_area", "fr_chim_area", "fr_whal_area", "fr_hon_area", "fr_juk_area", 
                                   "trail_length", "myo_dist", "non_dist", "bat_dist", "avg_altitude", "cctv_view_dist", "cctv_close_dist",
                                   "tower_dist", "choso_dist", "mun_cnt", "mun_kuk_dist", "mun_bo_dist", "mun_etc_dist", "temp_spring", "humi_spring", 
                                   "efhumi_spring", "wind_spring", "ff_cnt", "ff_damage_area")] 



## 데이터 분할 (train, test, train_up)
#formula <- ff ~ age65_ + fr_chim_area + fr_whal_area + fr_hon_area + trail_length + non_dist + bat_dist + myo_dist + 
#  avg_altitude + mun_cnt + temp_spring + efhumi_spring + wind_spring
formula_dmg <- ff_damage ~ fr_chim_area + fr_whal_area + fr_hon_area + avg_altitude + wind_spring + efhumi_spring
formula<-ff ~ age65_ + fr_chim_area + fr_whal_area + fr_hon_area + trail_length + non_dist + bat_dist + myo_dist + 
  avg_altitude + mun_cnt +efhumi_spring + wind_spring
set.seed(80)

#산불발생위험 데이터 분할
int <- createDataPartition(y = dataset_ffp_30$ff, p = 0.70, list = F)
train_30 <- dataset_ffp_30[int, ]; table(train_30$ff)
test_30 <- dataset_ffp_30[-int, ]; table(test_30$ff)
#불균형한 데이터 샘플링
train_up_30 <- ovun.sample(formula, data = train_30, method = "over", N = round(nrow(train_30 %>% filter(ff == 'N'))*2,0))$data


#산불확산위험 데이터 분할
int <- createDataPartition(y = dataset_ffp_30$ff_damage, p = 0.70, list = F)
train_30_dmg <- dataset_ffp_30[int, ]; table(train_30_dmg$ff_damage)
test_30_dmg <- dataset_ffp_30[-int, ]; table(test_30_dmg$ff_damage)
#불균형한 데이터 샘플링
train_up_30_dmg <- ovun.sample(formula_dmg, data = train_30_dmg, method = "over", N = round(nrow(train_30_dmg %>% filter(ff_damage == 'N'))*2,0))$data



