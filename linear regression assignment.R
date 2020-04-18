#salary dataset
sal <- read.csv("C:\\Users\\USER\\Downloads\\Salary_Data.csv")
View(sal)
plot(sal$Salary~sal$YearsExperience)


model_sal <- lm(Salary~YearsExperience , data = sal)
summary(model_sal)
pred <- predict(model_sal)

library(ggplot2)
ggplot(data = sal, aes(x = YearsExperience, y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sal, aes(x=YearsExperience, y=pred))


model_sal$residuals
sum(reg$residuals)
#r-square 0.957 

mean(model_sal$residuals)
sqrt(mean(model_sal$residuals^2))

model_sal1 <- lm(Salary~log(YearsExperience) , data = sal)
summary(model_sal1)
#r-square value 0.8539

model_sal2 <- lm(log(Salary)~(YearsExperience) , data = sal)
summary(model_sal2)
#r-square value 0.932

model_sal3 <- lm(log(Salary) ~ YearsExperience + I(YearsExperience*YearsExperience) , data = sal)
summary(model_sal3)
#R-squared:  0.9486 

model_sal4 <- lm(Salary ~ YearsExperience + I(YearsExperience*YearsExperience) , data = sal)
summary(model_sal4)
#R-squared:  0.957 so this table has the best r square value

#delivery_time
del <- read.csv("C:\\Users\\USER\\Downloads\\delivery_time.csv")
model_del <- lm(Delivery.Time~Sorting.Time, data = del)
summary(model_del)
#R-squared:  0.6823

model_del1 <- lm(Sorting.Time~log(Delivery.Time), data = del)
summary(model_del1)
# R-squared:  0.7109
model_del2 <- lm(log(Sorting.Time)~Delivery.Time, data = del)
summary(model_del2)
#R-squared:  0.6954

model_del3 <- lm(Sorting.Time~Delivery.Time+I(Delivery.Time*Delivery.Time), data = del)
summary(model_del3)
#R-squared:  0.7163

model_del4 <- lm(log(Sorting.Time)~Delivery.Time+I(Delivery.Time*Delivery.Time), data = del)
summary(model_del4)
#R-squared:  0.7937

model_del5 <- lm(log(Sorting.Time)~Delivery.Time+I(Delivery.Time*Delivery.Time)+I(Delivery.Time*Delivery.Time*Delivery.Time), data = del)
summary(model_del5)
#R-squared:  0.7976 so this is the best model

#emp_data
emp <- read.csv("C:\\Users\\USER\\Downloads\\emp_data.csv")
View(emp)

emp1 <-lm (Salary_hike~Churn_out_rate, data = emp)
summary(emp1)
#R-squared:  0.8312

emp2 <-lm (Salary_hike~log(Churn_out_rate), data = emp)
summary(emp2)
#R-squared:  0.8735

emp3 <-lm (log(Salary_hike)~Churn_out_rate, data = emp)
summary(emp3)
#R-squared:  0.8486


emp4 <-lm (Salary_hike~Churn_out_rate+I(Churn_out_rate*Churn_out_rate), data = emp)
summary(emp4)
#R-squared:  0.9741

emp5 <-lm (Salary_hike~Churn_out_rate+I(Churn_out_rate*Churn_out_rate)+I(Churn_out_rate*Churn_out_rate*Churn_out_rate), data = emp)
summary(emp5)
#R-squared:  0.9901


#calories consumed

cal <- read.csv("C:\\Users\\USER\\Downloads\\calories_consumed.csv")
View(cal)

cal1 <- lm(Weight.gained..grams.~Calories.Consumed,data = cal)
summary(cal1)
#R-squared:  0.8968

cal2 <- lm(log(Weight.gained..grams.)~Calories.Consumed,data = cal)
summary(cal2)
#R-squared:  0.8776

cal3 <- lm(Weight.gained..grams.~log(Calories.Consumed),data = cal)
summary(cal3)
# R-squared:  0.8077

cal4 <- lm(Weight.gained..grams.~Calories.Consumed+I(Calories.Consumed*Calories.Consumed),data = cal)
summary(cal4)
#R-squared:  0.9521 best table

cal5 <- lm(log(Weight.gained..grams.)~Calories.Consumed+I(Calories.Consumed*Calories.Consumed),data = cal)
summary(cal5)
#R-squared:  0.8776

