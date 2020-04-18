Startups <-read.csv("C:\\Users\\USER\\Downloads\\Startsss.csv")
View(Startups)
plot(Startups)
boxplot(Startups)
plot(density(Startups$R.D.Spend))
plot(density(Startups$Marketing.Spend))
plot(density(Startups$Profit))

#convert into numerical values
library(plyr)
Startups$State <- revalue(Startups$State,
                          c("New York"="0", "California"="1", "Florida"="2")) 
attach(Startups)
Startups <- cbind(RD_Spend=R.D.Spend,Administration,Marketing_Spend=Marketing.Spend,State,Profit)


Startups <- as.data.frame(Startups)

attach(Startups)

summary(Startups)

Model.Startups <- lm(Profit~.,data = Startups)
summary(Model.Startups)
#most of the variables are not significant
plot(Model.Startups)

library(mvinfluence)

influenceIndexPlot(Model.Startups, id.n=7)

library(car)

vif(Model.Startups)

library(MASS)
stepAIC(Model.Startups)

Model.Startups1 <-lm(formula = Profit ~ RD_Spend + Marketing_Spend, data = Startups)
summary(Model.Startups1)

Model.Startups2 <- lm(Profit~RD_Spend,data = Startups[c(-49,-50),])
influenceIndexPlot(Model.Startups2, id.n=3)
summary(Model.Startups2)
x <-Startups[c(-49,-50),] 
x$Profit
plot(Model.Startups2)


hist(residuals(Model.Startups2))

train <- x[1:40,]
test <- x[41:48,]

trainmodel <-lm(Profit~RD_Spend,data = train)
distPred <- predict(trainmodel, test) 

actuals_preds <- data.frame(cbind(actuals=test$Profit, predicteds=distPred))
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
correlation_accuracy <- cor(actuals_preds)

RSS <- c(crossprod(trainmodel$residuals))
MSE <- RSS / length(trainmodel$residuals)
RMSE <- sqrt(MSE)


RSS <- c(crossprod(Model.Startups$residuals))
MSE <- RSS / length(Model.Startups$residuals)
RMSE <- sqrt(MSE)

RSS <- c(crossprod(Model.Startups1$residuals))
MSE <- RSS / length(Model.Startups1$residuals)
RMSE <- sqrt(MSE)
