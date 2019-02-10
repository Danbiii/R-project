# sales: 구매정보 데이터
# demo: 각 고객의 개인정보

sales <- read.csv("DATA/sales.csv")
demo <- read.csv("DATA/demo.csv")
sales <- sales[sales$Price >0 & sales$Amount > 0,]
str(sales)
library(plyr)

# 각 고객 별 변수 
sales_t <- ddply(sales, ~ID, summarize,
                 to_amount = sum(Amount), # 총 구매수량
                 to_price = sum(Price*Amount), # 총 구매금액
                 to_count = length(Price), # 구매횟수
                 me_price = round(mean(Price*Amount))) # 평균 구매금액
sade <- merge(sales_t, demo)
head(sade)

# 산점도, 히스토그램 (이상점 파악, 특징 탐색) 
library(psych)
sade <- sade[,-c(6,8)]
pairs.panels(sade[,-c(1,6)])
par(mfcol=c(1,2))

plot(sade$ID, sade$to_amount)
abline(h=1000, col="red")
am <- sade[sade$to_amount>1000,]
am$ID <- as.character(am$ID)
text(am$ID, am$to_amount-50, paste("ID=", am$ID, sep=""), col="red")
hist(sade$to_amount, main="총 구매수량", breaks=40)

plot(sade$ID, sade$to_price)
abline(h=4e+08, col="red")
pr <- sade[sade$to_price>4e+08,]
pr$ID <- as.character(pr$ID)
text(pr$ID, pr$to_price-0.5e+08, paste("ID=", pr$ID, sep=""), col="red")
hist(sade$to_price, main="총 구매금액", breaks=40)

plot(sade$ID, sade$to_count)
abline(h=400, col="red")
co <- sade[sade$to_count>400,]
co$ID <- as.character(co$ID)
text(co$ID, co$to_count-30, paste("ID=", co$ID, sep=""), col="red")
hist(sade$to_count, main="구매횟수", breaks=40)

plot(sade$ID, sade$me_price)
abline(h=2e+07, col="red")
mpr <- sade[sade$me_price>2e+07,]
mpr$ID <- as.character(mpr$ID)
text(mpr$ID, mpr$me_price-0.3e+07, paste("ID=", mpr$ID, sep=""), col="red")
hist(sade$me_price, main="평균 구매금액", breaks=40)


# 이상점 제거 
pairs.panels(sade[-c(792,910,1203,2141,2408,2593),-1])

## 로그 변환 후 산점도, 히스토그램 
sade_rm <- sade[-c(792,910,1203,2141,2408,2593),-1]
log_sade <- log(sade_rm)
pairs.panels(log_sade)


# 각 고객의 카테고리 대분류 별 변수 
sales_c <- ddply(sales, ~ID+CT1, summarize,
                 to_amount = log(sum(Amount)), # 총 구매수량
                 to_price = log(sum(Price*Amount)), # 총 구매금액
                 to_count = log(length(Price)), # 구매횟수
                 me_price = log(round(mean(Price*Amount)))) # 평균 구매금액

## 카테고리별 각 변수의 변화 관찰 
library(lattice)
?histogram
histogram(~to_amount|CT1, data=sales_c, xlab="총 구매수량")
histogram(~to_price|CT1, data=sales_c, xlab="총 구매금액")
histogram(~to_count|CT1, data=sales_c, xlab="구매 횟수")
histogram(~me_price|CT1, data=sales_c, xlab="평균 금액")
summary(sales_c$to_count)

# 각 카테고리 대분류 별 변수 
sales$Price <- as.numeric(sales$Price)
sales$Amount <- as.numeric(sales$Amount)
sales_d <- ddply(sales, ~CT1, summarize,
                 to_amount = sum(Amount), # 총 구매수량
                 to_price = sum(Price*Amount), # 총 구매금액
                 to_count = length(Price), # 구매횟수 
                 me_price = round(mean(Price*Amount)), # 평균 구매금액
                 cus_count = length(unique(ID))) # 구매한 적이 있는 고객의 수 

# bubble plot
library(RColorBrewer)
color=brewer.pal(7, name = "Set3")

symbols(sales_d$to_amount, sales_d$to_price, circle=sqrt(sales_d$cus_count),inches=0.5,
        bg=color, xlab="총 구매수량", ylab="총 구매금액")
text(sales_d$to_amount, sales_d$to_price, sales_d$CT1, cex=1)
legend("topright", legend= sales_d$CT1, fill=color)

symbols(sales_d$to_amount, sales_d$to_count, circle=sqrt(sales_d$cus_count),inches=0.5,
        bg=color, xlab="총 구매수량", ylab="구매횟수")
text(sales_d$to_amount, sales_d$to_count, sales_d$CT1, cex=1)
legend("topleft", legend= sales_d$CT1, fill=color)

symbols(sales_d$to_amount, sales_d$me_price, circle=(sales_d$cus_count),inches=0.5,
        bg=color, xlab="총 구매수량", ylab="평균 구매금액")
text(sales_d$to_amount, sales_d$me_price, sales_d$CT1, cex=1)
legend("topright", legend= sales_d$CT1, fill=color)

symbols(sales_d$to_price, sales_d$to_count, circle=(sales_d$cus_count),inches=0.5,
        bg=color, xlab="총 구매금액", ylab="구매 횟수")
text(sales_d$to_price, sales_d$to_count, sales_d$CT1, cex=1)
legend("topleft", legend= sales_d$CT1, fill=color)

symbols(sales_d$to_price, sales_d$me_price, circle=(sales_d$cus_count),inches=0.5,
        bg=color, xlab="총 구매금액", ylab="평균 구매금액")
text(sales_d$to_price, sales_d$me_price, sales_d$CT1, cex=1)
legend("topright", legend= sales_d$CT1, fill=color)

symbols(sales_d$to_count, sales_d$me_price, circle=(sales_d$cus_count),inches=0.5,
        bg=color, xlab="구매 횟수", ylab="평균 구매금액")
text(sales_d$to_count, sales_d$me_price, sales_d$CT1, cex=1)
legend("topright", legend= sales_d$CT1, fill=color)
