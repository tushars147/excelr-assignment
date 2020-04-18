library(caret)
library(C50)
library(caTools)
library(plyr)
sale <- read.csv("C:\\Users\\USER\\Downloads\\Company_data.csv")
View(sale)
head(sale$Sales)
head(HighSales)
hist(sale$Sales)
cor(sale$Sales,sale$Advertising)
table(x$HighSales)
#this shows data is imbalanced

summary(sale)
#as the mean value is 7.45 will create a partion between one side of 7.45 and the other side.
HighSales <- ifelse(sale$Sales <= 8,"Low","High") #sales below 8 is low otherwise HIGH
head(sale)
head(HighSales)

x <- data.frame(sale, HighSales)
x <- x[,-1]
x
#create data partion
set.seed(101)
train=sample(1:nrow(x), 250)


library(tree)

model_sales <- tree(HighSales~.,data = x)
summary(model_sales)
plot(model_sales)
text(model_sales, pretty=0)
#factors such as shelving location plays the most important partt. For high sales good location and price <135 is needed. 
#advertising also plays an important role

modeltrain <- tree(HighSales~.,x,subset =train )
plot(modeltrain)
text(modeltrain)

pred <- predict(modeltrain,x[-train,],type = "class")
with(x[-train,], table(pred, HighSales))

accuracy =110/150
accuracy
#73.33%


#using library(party)
install.packages("party")
library(party)
table(x$HighSales)
tree1 <- ctree(HighSales~.,data = x)
plot(tree1)
#

tree2 <- ctree(HighSales~.,x,subset = train)
plot(tree2)

preds <- predict(tree2,x[-train,],type="response")
with(x[-train,], table(pred, HighSales))

#because of data imbalance we have less accuracy 
library(ROSE)
n_legit <- 236
new_frac_legit <- 0.50
new_n_total <- n_legit/new_frac_legit
oversampling_result <- ovun.sample(HighSales ~ .,
                                   data = x,
                                   method = "over"
                                   ,
                                   N = new_n_total,
                                   seed = 2018)
oversampled_credit <- oversampling_result$data
table(oversampled_credit$HighSales)

View(oversampled_credit)

xy <- oversampled_credit
xy

newmod <- tree(HighSales~.,data = xy)
plot(newmod)
text(newmod)
library(party)
newmods <- ctree(HighSales~.,data = xy)
plot(newmods)


training=sample(1:nrow(xy), 350)
new_m <- tree(HighSales~.,xy,subset = training)
pred_n <-predict(new_m,xy[-training,],type = "class")
with(xy[-training,],table(pred_n,HighSales))
accuracy1 <-92/122
accuracy1*100
accuracy1
#new accuracy is 75.54% not much change even after over sampling of data

