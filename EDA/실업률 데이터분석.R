# 실업률 데이터 분석
#시계열/계절성
UN <- read.csv("DATA/Unemployment.csv", header=TRUE)

#시계열
UN.ts = ts(UN$total, start= c(1999,1), frequency=12)
plot(UN.ts, main = "실업률")
lines(lowess(UN.ts), col="red")
lines(lowess(UN.ts, f=1/5), col="blue")
legend("topright",legend = c("UN.ts","f = 2/3", "f = 1/5"), 
       lty=c(1,1), col = c("black","red","blue"))

# 계절성 그래프 (월별 실업률)
log.UN.ts = log(UN.ts)
plot(log.UN.ts)
de = decompose(log.UN.ts)
plot(de$seasonal, main="계절성 그래프")
abline(v=c(1999.06:2017.07), lty=2 , col="red")

par(mar=c(5,5,5,5),xpd=F)
de$seasonal
part <- de$seasonal[1:36]
plot(part, type = "b", xlab = "month", ylab = "seasonal",xaxt="n") #xaxt="n" : x축 없애기
axis(1,1:36,rep(1:12,3)) #x축 설정
abline(v=c(9,21,33), lty=1, col="blue")
text(c(9,21,33),0.2,c("1999.09","2000.09","2001.09"))
abline(v=c(12,24,36), lty=2, col="red")
text(c(12,24,36),-0.036,c("1999.12","2000.12","2001.12"))

#계절성 제거한 자료 추세선
plot(de$x, main = "계절성 조절 그래프")
lines(de$trend, col="red")
lines(de$x - de$seasonal, col="blue")
legend("topright", legend=c("x", "Trend", "계절성 조절"), 
       lty=c(1,1), col = c("black","red","blue"))

#잡음 과정 그래프
plot(de$random, main="잡음 그래프")
de$random

plot(de$x)
lines(de$trend, col="red")
legend("topright", legend=c("x", "Trend"), 
       lty=c(1,1), col = c("black","red"))
