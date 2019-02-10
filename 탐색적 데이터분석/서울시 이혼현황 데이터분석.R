# 서울시 이혼현황 데이터 분석
#barplot

OC <- read.csv("DATA/OctagonExcel.csv")
OC <- as.matrix(OC)
OC <- t(OC)
colnames(OC) <- OC[1,] # 연도별 데이터 
barplot(OC)
OC <- OC[-1,]
OC <- OC[,-18]

# 색상 수정
library(RColorBrewer)
color=brewer.pal(5, name = "Set3")

#legend값 수정
rownames(OC) <- gsub("\\.","~",rownames(OC))
rownames(OC) <- gsub("X","",rownames(OC))

barplot(OC, col=color, legend.text = rownames(OC), 
        xlab = "Year", ylab = "Number", main = "혼인지속기간별 이혼건수")

#비율
rate <- t(OC)/colSums(OC)
rate <- t(rate)
rate
colSums(rate)
par(mar=c(5,5,5,5))
barplot(rate, col=color, legend.text = rownames(OC),
        xlab="Year", ylab="Rate", main = "혼인지속기간별 이혼건수 비율",
        args.legend = list(x="topright", inset=c(-0.15,0), cex=0.7))

