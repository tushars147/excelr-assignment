library(caTools)
library(dplyr)
library(ggplot2)
library(caret)
library(class)
library(corrplot)
glass <- read.csv("C:\\Users\\USER\\Downloads\\glass.csv")
glass
str(glass)
cor(glass[1:9])
table(glass$Type)
#proportion for type of glasse
s
hist(glass$Type)
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
glass_norm <- as.data.frame(lapply(glass[1:9], norm))
glass_data <-cbind(glass_norm,glass[,10])
glass_data
corrplot(cor(glass_data))
set.seed(101)

glass_sample <- sample.split(glass_data,SplitRatio = 0.70)

train_g <- subset(glass_data,glass_sample==TRUE)

test_g <- subset(glass_data,glass_sample==FALSE)

devel <- knn(train_g[1:9],test_g[1:9],cl=train_g$`glass[, 10]`,k=12)
devel
o <-table(devel,test_g$`glass[, 10]`)
accuracy1 <- sum(diag(o)/sum(o))
accuracy1

predicted.type <- NULL
error.rate <- NULL

for (i in 1:10) {
  predicted.type <- knn(train_g[1:9],test_g[1:9],train_g$`glass[, 10]`,k=i)
  error.rate[i] <- mean(predicted.type!=test_g$`glass[, 10]`)
  
}

knn.error <- as.data.frame(cbind(k=1:10,error.type =error.rate))

ggplot(knn.error,aes(k,error.type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:10)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')
#k=6 is the optimum vale
devel1 <- knn(train_g[1:9],test_g[1:9],cl=train_g$`glass[, 10]`,k=6)
devel
o1 <-table(devel1,test_g$`glass[, 10]`)
accuracy12 <- sum(diag(o1)/sum(o1))
accuracy12
