# 2017년 시즌 ranking 1~47위 타자의 성적 데이터
 
baseball <- read.csv("DATA/baseball_2017.csv")
library(lattice)

# minmaxscaling 사용 heatmap
minmaxscale=function(x) (x-min(x))/(max(x)-min(x))
baseball_m <- as.data.frame(lapply(baseball[,3:15], minmaxscale))
rownames(baseball_m) <- baseball[,1]
baseball_m <- as.matrix(baseball_m)
head(baseball_m)
heatmap(baseball_m)
baseball_m[rownames(baseball_m)=="김하성"]

# clustering 그룹 4개, 각 그룹에 대한 heatmap
baseball_s <- baseball[,3:15]
rownames(baseball_s) <- baseball[,1]
baseball_s <- as.matrix(scale(baseball_s))
head(baseball_s)
bc <- hclust(dist(baseball_s))
plot(bc)
bct <- hclust(dist(t(baseball_s)))
plot(bct)
cluster <- cutree(bc, 4)
heatmap(baseball_s, Colv=as.dendrogram(bct), scale='none')

for (i in 1:4) {
  windows()
  heatmap(baseball_s[cluster==i,], Colv=as.dendrogram(bct), scale='none', main=paste("GROUP",i))
}


# 각 chart(nightingale chart, face plot, star chart)

##nightingales
stars(baseball_s, flip.labels = FALSE, draw.segments = TRUE, key.loc=c(15,1.5))
for (i in 1:4) {
  windows()
  stars(baseball_s[cluster==i,], main=paste("Group",i),
        flip.labels = FALSE, draw.segments = TRUE)
}
stars(baseball_s[cluster==1,], main="GROUP1",
      flip.labels = FALSE, draw.segments = TRUE, key.loc=c(7.5,2))
stars(baseball_s[cluster==2,], main="GROUP2",
      flip.labels = FALSE, draw.segments = TRUE, key.loc=c(9.5,2))
stars(baseball_s[cluster==3,], main="GROUP3",
      flip.labels = FALSE, draw.segments = TRUE, key.loc=c(8,2))
stars(baseball_s[cluster==4,], main="GROUP4",
      flip.labels = FALSE, draw.segments = TRUE, key.loc=c(11.5,2))
##face
library(aplpack)
faces(baseball_s, scale=F)
for (i in 1:4){
  windows()
  faces(baseball_s[cluster==i,], scale=F, main=paste("GROUP",i))
}

##star
stars(baseball_s, flip.labels = FALSE, key.loc = c(14.5,1.7))
for (i in 1:4) {
  windows()
  stars(baseball_s[cluster==i,], flip.labels = FALSE, main=paste("GROUP",i))
}
stars(baseball_s[cluster==1,], flip.labels = FALSE, main="GROUP1", key.loc=c(7,2))
stars(baseball_s[cluster==2,], flip.labels = FALSE, main="GROUP2", key.loc=c(9.5,2))
stars(baseball_s[cluster==3,], flip.labels = FALSE, main="GROUP3", key.loc=c(8,2))
stars(baseball_s[cluster==4,], flip.labels = FALSE, main="GROUP4", key.loc=c(11.5,2))

# 평행좌표 플롯
parallelplot(baseball_s, horizontal.axis=F, col=1)
parallelplot(~baseball_s | factor(cluster), horizontal.axis = F, col=1)

