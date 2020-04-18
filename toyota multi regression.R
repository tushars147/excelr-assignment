toyota <-read.csv("C:\\Users\\USER\\Downloads\\ToyotaCorolla.csv")
View(toyota)

Corolla<-toyota[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
Corolla
boxplot(toyota)
plot(Corolla$Price,Corolla$Age_08_04, type = "p")
cor(Corolla)
#there's a strong correlatiob between price and age... with increase in age price decreases
summary(Corolla)

plot(Corolla$Price,Corolla$Quarterly_Tax)
plot(Corolla$Price,Corolla$KM)
#we observe that there's a strong negative correlation between price and age, column

t1.mod <- lm(Price~.,data = Corolla)
summary(t1.mod) 
#cc and doors are not signifcant data

library(car)

vif(t1.mod)

library(MASS)
stepAIC(t1.mod)

t2.model <- lm(Price~.-cc,data = Corolla)
summary(t2.model)

stepAIC(t2.model)

t3.model <-lm(formula = Price ~ Age_08_04 + KM + HP + Gears + Quarterly_Tax + 
                Weight, data = Corolla)
summary(t3.model)

library(mvinfluence)
influenceIndexPlot(t3.model, id.n=7)

datat <- Corolla[c(-222,-96),]
View(datat)

t4.model <-lm(formula = Price ~ Age_08_04 + KM + HP + Gears + Quarterly_Tax + 
                Weight, data = datat)
influenceIndexPlot(t4.model,id.n=7)

t5.model <-lm(formula = Price ~ Age_08_04 + KM + HP + Gears + Quarterly_Tax + 
                Weight, data = datat[-961,])
influenceIndexPlot(t5.model,id.n=7)

datat <- Corolla[c(-222,-96,-961,-602),]
t7.model <-lm(formula = Price ~ Age_08_04 + KM + HP + Gears + Quarterly_Tax + Weight, data = datat)
plot(t7.model)



trained <- datat[1:1000,]
tested <- datat[1001:1434,]
t8.model <- lm(formula = Price ~ Age_08_04 + KM + HP + Gears + Quarterly_Tax + 
                 Weight, data = trained)
pred.model <- predict(t8.model,tested)
dt <- data.frame(pred.model,tested$Price)
head(dt)

sqrt(mean(t8.model$residuals^2))
#1327.483

boxplot(toyota)
#the boxplot shows there are many outliers in KM coloumn

t9.model <- lm(formula = Price ~ Age_08_04+ HP + Gears + Quarterly_Tax + 
                 Weight, data = trained)
sqrt(mean(t9.model$residuals^2))
