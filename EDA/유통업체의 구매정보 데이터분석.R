# 유통업체의 2014년 1년간 3507명의 구매정보에 관한 데이터
#pie/barplot/mosaicplot
sales <- read.csv("sales.csv")

#상품의 대분류(CT1)별 구매방문 빈도의 분포 시각화
CT1_f <- table(sales$CT1)
CT1_f <- sort(CT1_f, decreasing = TRUE)
CT1_f
name=paste(rownames(CT1_f),",",round(CT1_f/sum(CT1_f)*100 ,1),"%",sep="")
name

library(RColorBrewer)
display.brewer.all(n=10, exact.n=FALSE)
color=brewer.pal(7,"Set3")
par(mar=c(4,4,4,4))
pie(CT1_f, labels=name, col=color, clockwise = FALSE, cex=0.7,
    main = "상품의 대분류별 구매방문 빈도의 분포")
legend("topright",rownames(CT1_f), fill=color, cex=0.8)

# 구매지역(시도) 간에 상품 대분류별 구매방문 빈도 차이 확인
## 각 구매지역과 상품 대분류 별 구매빈도의 분포 
str(sales)
head(sales)
tab <- table(sales$CT1, sales$Location1)
color=brewer.pal(7,"Paired")
barplot(tab, legend=rownames(tab), col=color, beside = T,
        main = "구매지역(시도) 간에 상품 대분류별 구매 빈도",
        xlab="location", ylab="frequency",ylim=c(0,16000),
        args.legend = list(x="topright", inset=c(-0.15,0), cex=0.6))
text(c(1:7,9:15,17:23)+0.5,tab+400, tab, cex=0.8)


## 각 구매지역과 상품 대분류 별 구매비율
prop <- prop.table(tab,2)
prop
barplot(prop, legend=rownames(tab), col=color,
        main = "구매지역(시도) 간에 상품 대분류별 구매 비율",
        xlab="location", ylab="rate",
        args.legend = list(x="topright", inset=c(-0.15,0), cex=0.7))
mosaicplot(~Location1+CT1, sales, color=T)


## 상품 대분류별 구매지역의 비유
prop_t <- prop.table(t(tab),2)
prop_t <- prop_t[c(1,3,2),]
color=brewer.pal(3,"Set2")
barplot(prop_t, legend=rownames(prop_t), col=color,
        main = "상품 대분류별 구매지역의 비율",
        xlab="category", ylab="rate",
        args.legend = list(x="topright", inset=c(-0.15,0), cex=0.7))
mosaicplot(~CT1+Location1, sales, color=T)

## mosaic plot
mosaicplot(~Location1 + CT1, data=sales, color=T)
mosaicplot(~CT1 + Location1, data=sales, color=T)

##########################################################

## 상품 대분류 별 구매 총금액의 월별 변화
month<- months(as.Date(as.character(sales[,5]),format="%Y%m%d"))
month <- factor(month, levels=paste(c(1:12),"월",sep=""))
sales <- cbind(sales, month)
str(sales)
head(sales)
p <- sales$Price
a <- abs(sales$Amount)
pa <- p*a
sales <- cbind(sales, pa)
head(sales)
sales_1 <- ddply(sales, ~CT1 + month, summarize,
                 sum_pa=sum(pa))
sales_1
str(sales_1)
ggplot(sales_1, aes(x=month, y=sum_pa, fill=CT1)) +
  geom_area(aes(group=CT1)) +
  scale_fill_brewer(palette="Paired") +
  labs(list(x="Month", y="Total", title="구매총금액의 월별 변화"))

## 상품 대분류 별 구매 총금액이 차지하는 비율의 월별 변화 
sales_2 <- xtabs(pa~CT1+month, data=sales)
sales_2 <- as.data.frame(sales_2)
head(sales_2)
ggplot(sales_2,aes(x=month, y=Freq, fill=CT1)) +
  geom_area(aes(group=CT1),position="fill") +
  scale_fill_brewer(palette="Paired") +
  labs(list(x="Month", y="Rate", title="구매총금액의 월별 비율"))

## (레포트 카테고리) 구매 총금액의 월별 변화 지역별 비교
sales_3 <- xtabs(pa~CT1+month+Location1, data=sales)
str(sales_3)
sales_3 <- sales_3[3,,]
sales_3 <- as.data.frame(sales_3)
sales_3
ggplot(sales_3, aes(x=month, y=Freq, color=Location1)) +
  geom_line(size=1.5, aes(group=Location1))+
  scale_color_brewer(palette="Set1")+
  labs(list(x="Month",y="Total",title="레포츠 구매총금액의 월별 지역별 변화"))

#amount
sales_4 <- xtabs(Amount~CT1+month+Location1, data=sales)
str(sales_4)
sales_4 <- sales_4[3,,]
sales_4 <- as.data.frame(sales_4)
head(sales_4)
ggplot(sales_4, aes(x=month, y=Freq, color=Location1)) +
  geom_line(size=1.5, aes(group=Location1))+
  scale_color_brewer(palette="Set1")+
  labs(list(x="Month",y="Total",title="레포츠 구매총량의 월별 지역별 변화"))
xtabs(Price~CT1+Location1, data=sales)

