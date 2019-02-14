# 2010≥‚ ±π∞°∫∞ ¿œ¿Œ¥Á ±ππŒ√—º“µÊ(Gross National Income;GNI)
## treemap
gni <- read.csv("GNI2010.csv")
head(gni)
library(portfolio)
table(gni$continent)
Mean_GNI <- mean(gni$GNI)
GG <- gni$GNI-Mean_GNI
map.market(id=gni$iso3 ,area=gni$population, group=gni$continent,
           color =GG, main="2010≥‚ ±π∞°∫∞ ¿œ¿Œ¥Á ±ππŒ√—º“µÊ(GNI)",
           lab=c("continent"=TRUE, "iso3"=TRUE))

