comp <- read.csv("C:\\Users\\USER\\Downloads\\Computer_Data.csv")
View(comp)
summary(comp)
cor(comp)

library(caTools)

library(plyr)

comp$cd <- revalue(comp$cd,c("yes"="1","no"="0"))
comp$multi <- revalue(comp$multi,c("yes"="1","no"="0"))
comp$premium <- revalue(comp$premium,c("yes"="1","no"="0"))

comps <- comp[2:11]

plot(comps)
hist(comps$price)

comp_model <- lm(comps$price~.,data = comps)
summary(comp_model)


