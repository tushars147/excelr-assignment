plastic_sales <- read.csv("C:\\Users\\USER\\Downloads\\PlasticSales (3).csv")
library(forecast)
View(plastic_sales)
library(fpp)
library(smooth)
plot(plastic_sales$Sales,type="o")
#so there's a slight trend in the data
#creating dummmy variables 
X<- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )
View(X)
colnames(X)<-month.abb 
View(X)
plastic_sales<-cbind(plastic_sales,X)
View(plastic_sales)

plastic_sales["t"]<- 1:60
View(plastic_sales)
plastic_sales["log_Sales"]<-log(plastic_sales["Sales"])
plastic_sales["t_square"]<-plastic_sales["t"]*plastic_sales["t"]
attach(plastic_sales)
train<-plastic_sales[1:48,]
test<-plastic_sales[49:60,]

#linear model
plastic_linear <- lm(Sales~t,data = train)
summary(plastic_linear)
#Multiple R-squared:  0.3305
plastic_predict <-data.frame(predict(plastic_linear,interval='predict',newdata =test))
rmse_linear_plastic <- sqrt(mean((plastic_predict$fit-test$Sales)^2,na.rm=T))
#260.9738

#expo model
plastic_expo_model<-lm(log_Sales~t,data=train)
summary(plastic_expo_model)
plastic_expo_pred<-data.frame(predict(plastic_expo_model,interval='predict',newdata=test))
rmse_expo_plastic<-sqrt(mean((test$Sales-exp(plastic_expo_pred$fit))^2,na.rm = T))
rmse_expo #268.6338

#quadratic model
Quad_model_plastic<-lm(Sales~t+t_square,data=train)
summary(Quad_model_plastic)
Quad_pred<-data.frame(predict(Quad_model_plastic,interval='predict',newdata=test))sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(sea_add_model)
plot(test$Sales,type="o")
plot(Quad_pred$fit,type="o")
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad
#297.4067

#additive seasonality
sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add #235.6027
plot(sea_add_pred$fit,type="o")

#additive seasonality with linear trend
Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear
#135.5536
plot(Add_sea_Linear_pred$fit,type="o")

#additive seasonality with quad trend
Add_sea_Quad_model<-lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad
#218.1939

#multiplicative seaonality
multi_sea_model<-lm(log_Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea #239.6543

#multiplicative linear trend seasonality
multi_add_sea_model<-lm(log_Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea #160.6833
plot(multi_add_sea_pred$fit,type="o")

#multiplicative quadratic seasonality
multi_add_sea_model<-lm(log_Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea
#239.606

#multiplicative model with linear trend seasonality has the best rmse value. so will be using that model for prediction

