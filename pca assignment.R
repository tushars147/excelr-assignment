wine <-read.csv("C:\\Users\\USER\\Downloads\\wine.csv")
View(wine)

W.pca <- princomp(wine[,-1], cor = TRUE, scores = TRUE, covmat = NULL)
summary(W.pca)
plot(W.pca)

library(cluster)
library(fpc)
library(NbClust)
library(factoextra)
#h clustering on original data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
norm_data <- as.data.frame(lapply(wine[2:14], norm))
d <- dist(wine[2:14],method = "euclidean")
fit <- hclust(d, method = "average")
plot(fit)
rect.hclust(fit,k=7)
groups <- cutree(fit,k=6)
final_wine <- data.frame(wine[,-1],groups)
View(final_wine)
#h clustering on pca score
View(W.pca$scores)
pca_score<-W.pca$scores[,1:3]
pca_scale <-scale(pca_score)
pca_scale <-pca_scale[-122,]
d1 <- dist(pca_scale,method = "euclidean")
fit1 <- hclust(d1,method = "average")
plot(fit1)7
rect.hclust(fit1,k=)
group1 <-cutree(fit1,k=4) 
#using original data optimum value for cluster k=6 and using pca score k=4

#kmeans clusterring on original data
wss <- c()
for(i in 2:15)wss[i] <- sum(kmeans(wine[2:14] , centers = i)$withinss)
plot(1:15,wss,type = "b",xlab="no of clusters",ylab = "avg distance")
#kmeans clustering on pca score
wss <- c()
for(i in 2:15)wss[i] <- sum(kmeans(pca_scale , centers = i)$withinss)
plot(1:15,wss,type = "b",xlab="no of clusters",ylab = "avg distance")
#optimum value for k=5 for both using orginal data and pca scores