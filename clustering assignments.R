crime <-read.csv("C:\\Users\\USER\\Downloads\\crime_data.csv")
View(crime)
summary(crime)

plot(crime)
cor(crime[2:5])

norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
crime1 <- as.data.frame(lapply(crime[2:5], norm))
d <- dist(crime1, method = "euclidean")
fit <- hclust(d, method = "average")
plot(fit, labels = crime$X)

rect.hclust(fit,k=5)

groups <- cutree(fit,k=5)
final <- data.frame(crime,groups)
View(final)


summary(final)

member1 <- split(final,final$groups,f)

View(member1)

crime_data_final <- cbind(crime, groups)
aggregate(crime_data_final[,2:5], by=list(crime_data_final$groups), FUN = mean)


library(plyr)

#using kmeans


km <- kmeans(crime1,7)

km$centers
km$cluster
wss <- c()
for(i in 2:15)wss[i] <- sum(kmeans(crime1 , centers = i)$withinss)
plot(1:15,wss,type = "b",xlab="no of clusters",ylab = "avg distance")

#by the plot it shows k=5 is an optimal value


