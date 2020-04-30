concrete <- read.csv("C:\\Users\\USER\\Downloads\\concrete.csv")
View(concrete)
plot(concrete)
cor(concrete)
concrete<-as.data.frame(concrete)
library(corrplot)
corrplot(cor(concrete))
#slag,superplastic,age have a postive effect on strength
#with the increase in them strength increases

#ash,water,coaresegg,fineagg have a negative effect
#when we increase them the strength decreases

norm <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
conc_norm <- as.data.frame(lapply(concrete,norm))
conc_norm <-cbind(conc_norm,concrete$strength)

concrete_train<-conc_norm[1:773,]
concrete_test<-conc_norm[774:1030,]
library(neuralnet)
library(nnet)
model.con <- neuralnet(concrete_train$strength~.,data=concrete_train,hidden = 5)
plot(model.con)
model_results <-compute(model.con,concrete_test[1:8])
pred.str <- model_results$net.result
cor(pred.str,concrete_test$strength)
#0.8066
RMSE1 <- sqrt(mean(pred.str-concrete_test$`concrete$strength`)^2)
0.339
#lets add more hidden layers to improve the model accuracy 
model.con1 <- neuralnet(concrete_train$strength~.,data=concrete_train,hidden =5)
plot(model.con1)
model_results1 <-compute(model.con,concrete_test[1:8])
pred.str1 <- model_results1$net.result
cor(pred.str1,concrete_test$strength)
#0.91793
RMSE12 <- sqrt(mean(pred.str1-concrete_test$strength)^2)
0.00296

#we do a cross validation to find the best network
library(plyr)
set.seed(450)
rmse.error <- NULL
hidden <- NULL
k <- 10
pbar <- create_progress_bar('text')
pbar$init(k)
for(i in 1:k){
  index <- sample(1:nrow(concrete),round(0.9*nrow(concrete)))
  train.cv <- concrete[index,]
  test.cv <- concrete[-index,]
  nn <- neuralnet(train.cv$strength~.,data=train.cv,hidden=c(5,2))
  pr.nn <- compute(nn,test.cv[,1:8])
  pr.nns <- pr.nn$net.result
  rmse.error[i]= sqrt(mean(pr.nns-test.cv$strength)^2)
  pbar$step()
}
rmse.error
min(rmse.error)
mean(rmse.error)
boxplot(rmse.error)
