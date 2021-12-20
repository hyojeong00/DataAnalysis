##데이터 구조 확인 
str(dataset_ffp2_30)

##데이터 기초통계 분석
summary(dataset_ffp2_30)


#산불발생이 0인 격자 제거
dataset_ffp3_30 <- dataset_ffp2_30[!(dataset_ffp2_30$ff_cnt == 0),]

##창여러개
par(mfrow=c(3,2))

##빈도분석(히스토그램)
hist(dataset_ffp2_30$ff_cnt, breaks=5, main="산불발생 건수")
hist(dataset_ffp3_30$ff_cnt, breaks=5, main="산불발생 건수(0 제거)")
hist(dataset_ffp2_30$ff_damage_area, breaks=30, main="산불피해 면적")
hist(dataset_ffp3_30$ff_damage_area, breaks=30, main="산불피해 면적(0 제거)")
hist(dataset_ffp2_30$age65_, breaks=30, main="고령 인구")
hist(dataset_ffp2_30$fr_chim_area, breaks=30, main="침엽수 면적")
hist(dataset_ffp2_30$fr_whal_area, breaks=30, main="활엽수 면적")
hist(dataset_ffp2_30$fr_hon_area, breaks=30, main="혼효림 면적")
hist(dataset_ffp2_30$trail_length, breaks=30, main="등산로 길이")
hist(dataset_ffp2_30$myo_dist, breaks=30, main="묘와의 거리")
hist(dataset_ffp2_30$non_dist, breaks=30, main="논과의 거리")
hist(dataset_ffp2_30$bat_dist, breaks=30, main="밭과의 거리")
hist(dataset_ffp2_30$avg_altitude, breaks=30, main="고도")
hist(dataset_ffp2_30$mun_cnt, breaks=10, main="문화재 개수")
hist(dataset_ffp2_30$temp_spring, breaks=10, main="봄평균 기온")
hist(dataset_ffp2_30$humi_spring, breaks=10, main="봄평균 습도")
hist(dataset_ffp2_30$efhumi_spring, breaks=10, main="봄평균 실효습도")
hist(dataset_ffp2_30$wind_spring, breaks=10, main="봄평균 풍속")


##산불발생 건수와의 산점도
ggplot(data=dataset_ffp3_30, aes(x=age65_, y=ff_cnt)) + geom_point(size=3, color="blue") + ggtitle('고령 인구 vs 산불 발생')
ggplot(data=dataset_ffp3_30, aes(x=fr_chim_area, y=ff_cnt)) + geom_point(size=3, color="blue") + ggtitle('침엽수 면적 vs 산불 발생')
ggplot(data=dataset_ffp3_30, aes(x=fr_whal_area, y=ff_cnt)) + geom_point(size=3, color="blue") + ggtitle('활엽수 면적 vs 산불 발생')
ggplot(data=dataset_ffp3_30, aes(x=fr_hon_area, y=ff_cnt)) + geom_point(size=3, color="blue") + ggtitle('혼효림 면적 vs 산불 발생')
ggplot(data=dataset_ffp3_30, aes(x=trail_length, y=ff_cnt)) + geom_point(size=3, color="blue") + ggtitle('등산로 길이 vs 산불 발생')
ggplot(data=dataset_ffp3_30, aes(x=myo_dist, y=ff_cnt)) + geom_point(size=3, color="blue") + ggtitle('묘와의 거리 vs 산불 발생')
ggplot(data=dataset_ffp3_30, aes(x=non_dist, y=ff_cnt)) + geom_point(size=3, color="blue") + ggtitle('논과의 거리 vs 산불 발생')
ggplot(data=dataset_ffp3_30, aes(x=bat_dist, y=ff_cnt)) + geom_point(size=3, color="blue") + ggtitle('밭과의 거리 vs 산불 발생')
ggplot(data=dataset_ffp3_30, aes(x=avg_altitude, y=ff_cnt)) + geom_point(size=3, color="blue") + ggtitle('고도 vs 산불 발생')
ggplot(data=dataset_ffp3_30, aes(x=mun_cnt, y=ff_cnt)) + geom_point(size=3, color="blue") + ggtitle('문화재 개수 vs 산불 발생')
ggplot(data=dataset_ffp3_30, aes(x=temp_spring, y=ff_cnt)) + geom_point(size=3, color="blue") + ggtitle('봄평균 기온 vs 산불 발생')
ggplot(data=dataset_ffp3_30, aes(x=humi_spring, y=ff_cnt)) + geom_point(size=3, color="blue") + ggtitle('봄평균 습도 vs 산불 발생')
ggplot(data=dataset_ffp3_30, aes(x=efhumi_spring, y=ff_cnt)) + geom_point(size=3, color="blue") + ggtitle('봄평균 실효습도 vs 산불 발생')
ggplot(data=dataset_ffp3_30, aes(x=wind_spring, y=ff_cnt)) + geom_point(size=3, color="blue") + ggtitle('봄평균 풍속 vs 산불 발생')



##산불피해 면적과의 산점도
# 산불피해 면적의 이상값 제거 (이상값이 존재할 경우 적용)
dataset_ffp3_30 <- dataset_ffp3_30[!(dataset_ffp3_30$ff_damage_area == 67),]
ggplot(data=dataset_ffp3_30, aes(x=age65_, y=ff_damage_area)) + geom_point(size=3, color="blue") + ggtitle('고령 인구 vs 산불피해 면적')
ggplot(data=dataset_ffp3_30, aes(x=fr_chim_area, y=ff_damage_area)) + geom_point(size=3, color="blue") + ggtitle('침엽수 면적 vs 산불피해 면적')
ggplot(data=dataset_ffp3_30, aes(x=fr_whal_area, y=ff_damage_area)) + geom_point(size=3, color="blue") + ggtitle('활엽수 면적 vs 산불피해 면적')
ggplot(data=dataset_ffp3_30, aes(x=fr_hon_area, y=ff_damage_area)) + geom_point(size=3, color="blue") + ggtitle('혼효림 면적 vs 산불피해 면적')
ggplot(data=dataset_ffp3_30, aes(x=trail_length, y=ff_damage_area)) + geom_point(size=3, color="blue") + ggtitle('등산로 길이 vs 산불피해 면적')
ggplot(data=dataset_ffp3_30, aes(x=myo_dist, y=ff_damage_area)) + geom_point(size=3, color="blue") + ggtitle('묘와의 거리 vs 산불피해 면적')
ggplot(data=dataset_ffp3_30, aes(x=non_dist, y=ff_damage_area)) + geom_point(size=3, color="blue") + ggtitle('논과의 거리 vs 산불피해 면적')
ggplot(data=dataset_ffp3_30, aes(x=bat_dist, y=ff_damage_area)) + geom_point(size=3, color="blue") + ggtitle('밭과의 거리 vs 산불피해 면적')
ggplot(data=dataset_ffp3_30, aes(x=avg_altitude, y=ff_damage_area)) + geom_point(size=3, color="blue") + ggtitle('고도 vs 산불피해 면적')
ggplot(data=dataset_ffp3_30, aes(x=mun_cnt, y=ff_damage_area)) + geom_point(size=3, color="blue") + ggtitle('문화재 개수 vs 산불피해 면적')
ggplot(data=dataset_ffp3_30, aes(x=temp_spring, y=ff_damage_area)) + geom_point(size=3, color="blue") + ggtitle('봄평균 기온 vs 산불피해 면적')
ggplot(data=dataset_ffp3_30, aes(x=humi_spring, y=ff_damage_area)) + geom_point(size=3, color="blue") + ggtitle('봄평균 습도 vs 산불피해 면적')
ggplot(data=dataset_ffp3_30, aes(x=efhumi_spring, y=ff_damage_area)) + geom_point(size=3, color="blue") + ggtitle('봄평균 실효습도 vs 산불피해 면적')
ggplot(data=dataset_ffp3_30, aes(x=wind_spring, y=ff_damage_area)) + geom_point(size=3, color="blue") + ggtitle('봄평균 풍속 vs 산불피해 면적')



## 상관관계 분석
EDA_1 <- cor(dataset_ffp2_30[,c("age0_200", "age_65", "age65_", "myo_dist", "non_dist", "bat_dist", 
                                "trail_length", "avg_altitude", "mun_cnt", "ff_cnt")])
EDA_2 <- cor(dataset_ffp2_30[,c("fr_etc_area", "fr_chim_area", "fr_whal_area", "fr_hon_area", "fr_juk_area",  
                                "temp_spring", "humi_spring", "efhumi_spring", "wind_spring", "ff_cnt")])
EDA_3 <- cor(dataset_ffp2_30[,c("age0_200", "age_65", "age65_", "myo_dist", "non_dist", "bat_dist", 
                                "trail_length", "avg_altitude", "mun_cnt", "ff_damage_area")])
EDA_4 <- cor(dataset_ffp2_30[,c("fr_etc_area", "fr_chim_area", "fr_whal_area", "fr_hon_area", "fr_juk_area",   
                                "temp_spring", "humi_spring", "efhumi_spring", "wind_spring", "ff_damage_area")])

#창한개
par(mfrow=c(1,1))
# 산불발생 건수와의 상관관계
corrplot.mixed(EDA_1) ; EDA_1
corrplot.mixed(EDA_2) ; EDA_2
# 산불피해 면적과의 상관관계
corrplot.mixed(EDA_3) ; EDA_3
corrplot.mixed(EDA_4) ; EDA_4




