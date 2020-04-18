cc <- read.csv("C:\\Users\\USER\\Downloads\\creditcard.csv")
View(cc)

str(cc)
summary(cc)

library(car)
library(caTools)



library(plyr)
contrasts(cc$owner)
cc$card  <- revalue(cc$card,c("yes"="1","no"="0")) 

cc$card <- as.factor(cc$card)

model <- glm(card~.,data = cc[2:13],family = "binomial")
summary(model)

prob <- predict(model,cc,type="response")
cc
library(MASS)
stepAIC(model)

model2 <-glm(card~reports+expenditure+dependents+active, data = cc,family="binomial")
summary(model2)

model3 <-glm(card~reports+dependents+active, data = cc[2:13],family="binomial")
summary(model3)

cc <- cc[2:13]
train <-cc[1:1000,]
test <-cc[1001:1319,]


model4 <- model3 <-glm(card~reports+dependents+active, data = train,family="binomial")
summary(model4)
probab <- predict(model,test,type="response")

head(probab,10)

probab1 <- ifelse(probab>0.5,1,0)
head(probab1,10)

accuracy <-table(probab1,test$card)

sum(diag(accuracy)/sum(accuracy))

x <- data.frame(test$card, probab1)

library(ROCR)
rocrpred<-prediction(prob,cc$card)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf)