library(readxl)
airlines <- read_excel("C:\\Users\\USER\\Downloads\\Airlines+Data.xlsx")
View(airlines)
plot(airlines$Passengers,type = "o")

#create dummy variables 
X1 <- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )
View(X1)
colnames(X1)<-month.abb 
View(X1)
air <-cbind(airlines,X1)
View(air)

air["t"] <- 1:96
View(air)
air["log_pas"] <- log(air["Passengers"])
View(air)
air["t_square"]<-air["t"]*air["t"]

train_ <- air[1:80,]
test_ <- air[81:96,]

#linear model 
linear_model <- lm(Passengers~t,data = train_)
summary(linear_model)
predict_linear <- data.frame(predict(linear_model,interval="predict",newdata=test_))
rmse_linear <- sqrt(mean((predict_linear$fit-test_$Passengers)^2,na.rm = T))
#47.54262

#expo model 
expo_model <- lm(log_pas~t,data = train_)
summary(expo_model)
predict_expo <- data.frame(predict(expo_model,interval = "predict",newdata = test_))
rmse_expo <- sqrt(mean((exp(predict_expo$fit)-test_$Passengers)^2,na.rm=T))
#43.79374

#quadratic model
quad_model <- lm(Passengers~t+t_square,data = train_)
summary(quad_model)
predict_quad <- data.frame(predict(quad_model,interval="predict",newdata=test_))
rmse_quad <- sqrt(mean((predict_linear$fit-test_$Passengers)^2,na.rm = T))
#47.54262

#linear trend with additive seasonality
linear_model_add <-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train_)
summary(linear_model_add)
predict_linear_add <- data.frame(predict(linear_model_add,interval = "predict",newdata = test_))
rmse_Add_sea_Linear <- sqrt(mean((predict_linear_add$fit-test_$Passengers)^2,na.rm=T))                                 
#33.04571

#quad trend with additive seasonality
quad_model_add <-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train_)
summary(quad_model_add)
predict_quad_add <- data.frame(predict(quad_model_add,interval = "predict",newdata = test_))
rmse_Add_sea_Quad <- sqrt(mean((predict_quad_add$fit-test_$Passengers)^2,na.rm=T))
#23.91098

#multiplicative linear trend seasonality
multi_add_sea_model<-lm(log_pas~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train_)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test_,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test_$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea #9.469
plot(multi_add_sea_pred$fit,type="o")

#multiplicative quadratic seasonality
multi_add_sea_model_quad<-lm(log_pas~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train_)
summary(multi_add_sea_model_quad) 
multi_add_sea_pred_q<-data.frame(predict(multi_add_sea_model_quad,newdata=test_,interval='predict'))
rmse_multi_add_sea_quad<-sqrt(mean((test_$Passengers-exp(multi_add_sea_pred_q$fit))^2,na.rm = T))
rmse_multi_add_sea_quad
#23.08635
