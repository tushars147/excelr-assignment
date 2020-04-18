library(caret)
library(C50)
library(caTools)
library(plyr)
fraud <- read.csv("C:\\Users\\USER\\Downloads\\Fraud_check.csv")
View(fraud)
choice <- ifelse(fraud$Taxable.Income <=30000 ,"Risky","Good")
choice1 <-data.frame(fraud,choice)
choice1 <- choice1[,-3]
choice1

library(plyr)
choice1$Undergrad <- revalue(choice1$Undergrad,c("YES"="1","NO"="0"))
choice1$Urban <- revalue(choice1$Urban,c("YES"="1","NO"="0"))
choice1$Marital.Status <- revalue(choice1$Marital.Status,c("Single"="0","Married"="1","Divorced"="2"))

table(choice1$choice)
#imbalanced data
library(ROSE)
n_legit <- 476
new_frac_legit <- 0.50
new_n_total <- n_legit/new_frac_legit
overs <- ovun.sample(choice ~ .,
                                   data = choice1,
                                   method = "over"
                                   ,
                                   N = new_n_total,
                                   seed = 2018)
oversampled <- overs$data
table(oversampled$choice)
#now table is being balanced.
oversampled$Undergrad<-as.factor(oversampled$Undergrad)

View(oversampled)
set.seed(123)
sap<- sample(1:nrow(oversampled),700)

xyz <-tree(choice~.,oversampled,subset = sap)
plot(xyz)
