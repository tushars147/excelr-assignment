company <- read.csv("C:\\Users\\USER\\Downloads\\Company_Data.csv")
max <- ifelse(company$Sales <=8, "low","high")
View(company)
set.seed(100)
new <- data.frame(company,max)
new<-new[,-1]
new
scatter.smooth(company$Sales,company$CompPrice)
plot(company$Sales,company$CompPrice)
plot(company)
scatter.smooth(company$Sales,company$Advertising)
#positive correlation between sales and advertisment 
scatter.smooth(company$Price,company$Sales)
#with the increase in price.. sales get reduced
scatter.smooth(company$Age,company$Sales)
scatter.smooth(company$US,company$Sales)



train <- sample(nrow(new), 0.7*nrow(new), replace = FALSE)
TrainSet <- new[train,]
ValidSet <- new[-train,]
summary(TrainSet)
summary(ValidSet)
library(randomForest)
mod.com <- randomForest(max~.,data = TrainSet,importance=TRUE)
mod.com
summary(mod.com)

mod.com2 <- randomForest(max~.,data = TrainSet, ntree=500,mtry=6, importance=TRUE)
mod.com2

predic <- predict(mod.com2,ValidSet,type = "class")
ab <- table(predic,ValidSet$max)
ab
38+62+20
100/120
accuracy =0.8333

#best tree using for loop
a=c()
i=5
for (i in 3:8) {
  model3 <- randomForest(max~., data = TrainSet, ntree = 1000, mtry = i, importance = TRUE)
  predValid <- predict(model3, ValidSet, type = "class")
  a[i-2] = mean(predValid == ValidSet$max)
}

a

plot(3:8,a,type = "b")

mod.com3 <- randomForest(max~.,data = TrainSet, ntree=1000,mtry=5, importance=TRUE)
mod.com3

predic1 <- predict(mod.com3,ValidSet,type = "class")
abc <- table(predic1 ,ValidSet$max)
abc
95/120
