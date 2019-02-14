###########################################
#CCTV데이터 '서울' 추출
ex= read.csv("전국cctv표준데이터.csv")
seoul = subset(ex, substr(소재지도로명주소,1,2) == "서울")

library(RColorBrewer)
library(ggmap)
library(plyr)
library(ggplot2)
library(psych)
library(reshape2)

#범죄데이터 2011~2016년 통합
c_2016 = read.csv("2016년.csv")
c_2015 = read.csv("2015년.csv")
c_2014 = read.csv("2014년.csv")
c_2013 = read.csv("2013년.csv")
c_2012= read.csv("2012년.csv")
c_2011 = read.csv("2011년.csv")
crime <- cbind(c_2011,c_2012$검거, c_2013$건수, c_2014$건수, c_2015$건수,c_2016$건수)
colnames(crime)[4:9] <- as.character(2011:2016)

'
cr_sum <- apply(crime[4:9],1,sum)
crime <- cbind(crime,cr_sum)
colnames(crime)[10] <- "total"
'''
crime = crime[c(1:310),]

#지도맵
ggmap(get_googlemap(center = "seoul", zoom = 11, maptype = "roadmap"))+
  geom_point(aes(x = 경도, y = 위도), alpha = 0.2,data = seoul)

## 범죄 '구' 전처리
crime$구분 = as.character(crime$구분)
crime = subset(crime, 구분 != "중부" & 구분 != "서부")
crime$구분[crime$구분 == "남대문"] = "중"
crime$구분[crime$구분 == "수서"] = "강남"
crime$구분[crime$구분 == "방배"] = "서초"
crime$구분[crime$구분 == "종암"] = "성북"
crime$구분[crime$구분 == "혜화"] = "종로"
crime$구분 = paste(crime$구분,"구", sep = "")
head(crime)

##범죄 발생 & 검거 
crime_발생 = subset(crime, subset = 발생검거 == "발생")
crime_검거 = subset(crime, subset = 발생검거 == "검거")
##
##CCTV 지역 전처리
sp  = strsplit(as.character(seoul$소재지도로명주소), " ")
gu = c()

for (i in 1:length(sp)){
  gu[i]<- sp[[i]][2]
}
seoul$구 = gu
seoul = seoul[,c(-4,-6,-10,-14)]

### cctv 설치년월 전처리
seoul = seoul[-c(1:35),]
seoul = subset(seoul, subset = 설치년월 != "")
seoul$설치년월 = as.character(seoul$설치년월)
seoul$설치년도 = substr(seoul$설치년월,1,4)
seoul$설치년도 = as.numeric(seoul$설치년도)

cctv_2011 = subset(seoul, 설치년도 <= 2011)
cctv_2012 = subset(seoul, 설치년도 <= 2012)
cctv_2013 = subset(seoul, 설치년도 <= 2013)
cctv_2014 = subset(seoul, 설치년도 <= 2014)
cctv_2015 = subset(seoul, 설치년도 <= 2015)
cctv_2016 = subset(seoul, 설치년도 <= 2016)

c_11 = sum(cctv_2011$카메라대수)
c_12 = sum(cctv_2012$카메라대수)
c_13 =sum(cctv_2013$카메라대수)
c_14 = sum(cctv_2014$카메라대수)
c_15 = sum(cctv_2015$카메라대수)
c_16 = sum(cctv_2016$카메라대수)

cctv_year = data.frame(year = 2011:2016, cctv_count = c(c_11,c_12,c_13,c_14,c_15,c_16))

tot_crime_year = t(crime_발생[,c(-1,-2,-3)])
tot = t(crime_검거[,c(-1,-2,-3)])
발생건수총합 = rowSums(tot_crime_year)
검거건수총합 = rowSums(tot)

## 검거/발생 상관계수
검거율 = 검거건수총합/발생건수총합
cctv_year = cbind(cctv_year,발생건수총합, 검거건수총합,검거율)
pairs.panels(cctv_year[,c(-1,-4)])

#연도별 CCTV대수 증가 추이
plot(cctv_year$year, cctv_year$cctv_count, type="s", main= "연도별 cctv증가 대수",
     xlab="Year", ylab="cctv대수")
text(cctv_year$year, min(cctv_year$cctv_count)+1000, cctv_year$cctv_count, cex=1.2)
a <- c()
for (i in 1:length(rownames(cctv_year))){
  a[i]<- cctv_year$cctv_count[i+1]-cctv_year$cctv_count[i]
}
text(cctv_year$year+0.5, 15000, a, cex=1.2, col="red")

#연도별 범죄 검거율 증가 추이
plot(cctv_year$year, cctv_year$검거율, type="s", main="연도별 검거율 증가현황",
     xlab="Year", ylab="검거율")
text(cctv_year$year, min(cctv_year$검거율)+0.01, round(cctv_year$검거율,3), cex=1.2)
b <- c()
for (i in 1:length(rownames(cctv_year))){
  b[i]<- cctv_year$검거율[i+1]-cctv_year$검거율[i]
}
text(cctv_year$year+0.5, 0.64, round(b,3), cex=1.2, col="red")

#연도별 범죄발생 건수 감소 추이
plot(cctv_year$year, cctv_year$발생건수총합, type="s", main="연도별 발생건수",
     xlab="Year", ylab="발생건수")
text(cctv_year$year, min(cctv_year$발생건수총합)+1000, cctv_year$발생건수총합, cex=1.2)
c <- c()
for (i in 1:length(rownames(cctv_year))){
  c[i]<- cctv_year$발생건수총합[i+1]-cctv_year$발생건수총합[i]
}
text(cctv_year$year+0.5, 120000, c, cex=1.2, col="red")



#######

crime.m = melt(crime, id.vars = c("구분","죄종","발생검거"))
year_portion = ddply(crime.m, ~variable+구분, summarize,
                     por = sum(value[발생검거 == "검거"])/sum(value[발생검거 == "발생"]))
year_발생 = ddply(crime.m, ~variable+구분,summarize,
                tot = sum(value[발생검거 == "발생"]))
#연도별 지역의 검거율
ggplot(year_portion, aes(x = variable, y = por, group = 구분 , col = 구분))+
  geom_line(size = 1)+
  labs(x = "연도",
       y = "검거율",
       title="연도별 지역의 검거율")
#연도별 지역의 범죄발생 건수
ggplot(year_발생, aes(x = variable, y = tot, group = 구분, col = 구분)) +
  geom_line(size = 1)+
  labs(x = "연도",
       y = "발생 건수",
       title="연도별 지역의 범죄발생 건수")

#죄종별 검거율 추이
category_발생 = ddply(crime.m, ~죄종+variable, summarize, count= sum(value[발생검거 == "발생"]))
category_검거율 = ddply(crime.m, ~죄종+variable, summarize,
                     por = sum(value[발생검거 == "검거"])/sum(value[발생검거 == "발생"])*100)

windows()
ggplot(category_검거율, aes(x = 죄종, y = por, group = variable, fill = variable))+
  geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "죄종", y = "검거율", title = "죄종별 검거율 추이",fill = "연도")+
  scale_fill_brewer(palette = "Reds") + theme_dark()
