library(plyr)
library(neuralnet)
Startups <- read.csv("C:\\Users\\USER\\Downloads\\50_Startups (1).csv")
Startups$State <- as.numeric(revalue(Startups$State,
                                     c("New York"="0", "California"="1",
                                       "Florida"="2")))
str(Startups)
View(Startups)
Startups <- as.data.frame(Startups)
plot(Startups)
cor(Startups)
library(corrplot)
corrplot(cor(Startups))
plot(Startups$R.D.Spend,Startups$Profit)
#increase in RD also increases the profit...
plot(Startups$Marketing.Spend,Startups$Profit)
#increase in investment in marketting also increases the profit
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
Startups_norm<-as.data.frame(lapply(Startups,FUN=normalize))
summary(Startups_norm$Profit)
set.seed(123)
ind <- sample(2, nrow(Startups_norm), replace = TRUE, prob = c(0.7,0.3))
Startups_train <- Startups_norm[ind==1,]
startups_test  <- Startups_norm[ind==2,]
# Creating a neural network model on training data
startups_model <- neuralnet(Profit~R.D.Spend+Administration
                            +Marketing.Spend+State,data = Startups_train)
str(startups_model)
plot(startups_model, rep = "best")
model_results <- compute(startups_model,startups_test[1:4])
predicted_profit <- model_results$net.result
# Predicted profit Vs Actual profit of test data.
cor(predicted_profit,startups_test$Profit)
#0.9556

rmse_start <- sqrt(mean(predicted_profit-startups_test$Profit)^2)
rmse_start
#0.0001698294


startups_model <- neuralnet(Profit~R.D.Spend+Administration
                            +Marketing.Spend+State,data = Startups_train,hidden = 3)
str(startups_model)
plot(startups_model, rep = "best")
model_results <- compute(startups_model,startups_test[1:4])
predicted_profit <- model_results$net.result
# Predicted profit Vs Actual profit of test data.
cor(predicted_profit,startups_test$Profit)
#0.9689

rmse_start <- sqrt(mean(predicted_profit-startups_test$Profit)^2)
rmse_start
#0.0045

startups_model <- neuralnet(Profit~R.D.Spend+Administration
                            +Marketing.Spend+State,data = Startups_train,hidden = 5)
str(startups_model)
plot(startups_model, rep = "best")
model_results <- compute(startups_model,startups_test[1:4])
predicted_profit <- model_results$net.result
# Predicted profit Vs Actual profit of test data.
cor(predicted_profit,startups_test$Profit)
#0.97028

rmse_start <- sqrt(mean(predicted_profit-startups_test$Profit)^2)
rmse_start
#0.00456

#we do a cross validation to find the best network
library(plyr)
set.seed(450)
rmse.error1 <- NULL
hidden <- NULL
corr <- NULL
k <- 10
pbar <- create_progress_bar('text')
pbar$init(k)
for(i in 1:k){
  index1 <- sample(1:nrow(Startups_norm),round(0.9*nrow(Startups_norm)))
  train.cv1 <- Startups_norm[index1,]
  test.cv1 <- Startups_norm[-index1,]
  nn1 <- neuralnet(train.cv1$Profit~.,data=train.cv1,hidden=c(5,2))
  pr.nn1 <- compute(nn1,test.cv1[,1:4])
  pr.nns1 <- pr.nn1$net.result
  corr[i] =cor(pr.nns1,test.cv1$Profit)
  #rmse.error1[i]= sqrt(mean(pr.nns1-test.cv1$Profit)^2)
  pbar$step()
}
corr
max(corr)
#0.99918
boxplot(corr)

rmse.error1
min(rmse.error1)
mean(rmse.error1)
boxplot(rmse.error1)
