computer <- read.csv("C:\\Users\\USER\\Downloads\\Computer_Data.csv")
View(computer)
#remove the first column as it does not play any role in regression

computer <- computer[2:11]
computer
boxplot(computer$speed)
cor(computer$price,computer$hd)
plot(computer$price,computer$hd)
#with increase in HD value computer price increases too
cor(computer$price,computer$speed)
plot(computer$price,computer$speed)
cor(computer$price,computer$ram)
plot(computer$price,computer$ram)
#strong correlation between price and ram
cor(computer$price,computer$screen)
cor(computer$price,computer$ads)

hist(computer$price)
#follows a normal distribution
scatter.smooth(computer$price,computer$speed)

comp.mod <- lm(price~.,data = computer)
summary(comp.mod)

#calculate the AIC value to find the best feature model for ur dataset
library(MASS)
stepAIC(comp.mod)

library(car)
vif(comp.mod)
#there's no problem for multi collinearity

comp.train <- computer[1:5000,]
comp.test <- computer[5001:6259,]

comp.mod2 <- lm(formula = price ~ speed + hd + ram + screen + cd + multi + 
                  premium + ads + trend, data = comp.train)
summary(comp.mod2)

comp.mod3 <- lm(formula = price ~ speed + hd + ram + screen + cd + multi + 
                  premium + ads, data = comp.train)
summary(comp.mod3)
comp.mod4 <- lm(formula = price ~ speed + hd + ram + screen + cd + 
                  premium + ads, data = comp.train)
summary(comp.mod4)
plot(comp.mod2)
stepAIC(comp.mod2)
comp.pred <- predict(comp.mod2,comp.test)
compare <- data.frame(comp.pred,comp.test$price)
compare
head(compare,10)

boxplot(computer)
#there are many values that can be treated as outliers 
outlierTest(comp.mod2)
#let's remove the outliers
influenceIndexPlot(comp.mod2,id.n=3)
comp.mod2 <- lm(formula = price ~ speed + hd + ram + screen + cd + multi + 
                  premium + ads + trend, data = comp.train[c(-3784,-4478),])
comp.mod2$residuals
sqrt(mean(comp.mod2$residuals^2))
#277.1931
comp.mod2 <- lm(formula = price ~ speed + hd + ram + screen + cd + multi + 
                  premium + ads + trend, data = comp.train[c(-1441,1701),])
comp.mod2$residuals
sqrt(mean(comp.mod2$residuals^2))
#277.1391

#even by removing some outliers it does not play much change in the prediction