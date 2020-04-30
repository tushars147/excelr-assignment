train_1 <- read.csv("C:\\Users\\USER\\Downloads\\SalaryData_Train.csv")
test_1 <- read.csv("C:\\Users\\USER\\Downloads\\SalaryData_Test.csv")
train_1
library(naivebayes)
library(e1071)
library(caret)
library(psych)
str(train_1)
train_1$educationno <- as.factor(train_1$educationno)
str(train_1)
test_1$educationno <- as.factor(test_1$educationno)

plot(train_1$workclass,train_1$Salary)
#private sector has people with maximum count having salary < 50k

ggplot(data=train_1,aes(x=train_1$Salary, y = train_1$age, fill = train_1$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
#avg age for people having salary greater than t0k is around 44,45

plot(train_1$maritalstatus,train_1$Salary)
#married-civ-spouse people contribute more to people having salary greater than 50k

plot(train_1$occupation,train_1$Salary)
#exec-managerial and protective-serv have salary >50k mostly
plot(train_1$relationship,train_1$Salary)
#most of the husbands in the family earn more than 50k 
plot(train_1$race,train_1$Salary)

plot(train_1$sex,train_1$Salary)
#most of the male earn more than 50k avg salary

plot(train_1$education,train_1$Salary)
#masters and Bachelors degree students earn more than 50k on an average

#model building
Model1 <- naiveBayes(train_1$Salary ~ ., data = train_1)
Model1
Model_pred <- predict(Model1,test_1)
mean(Model_pred==test_1$Salary)
confusionMatrix(Model_pred,test_1$Salary)
