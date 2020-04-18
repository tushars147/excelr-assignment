data(iris)
iris

table(iris$Species)
#all the species are in equal proportion 
cor(iris[,-5])

library(party)

mod.1 <- ctree(iris$Species~.,data = iris)
plot(mod.1)
#from the plot we observe if petal length <= 1.9 most of the species are Setosa and
#If petal length >1.9 and Petal width > 1.7 then it is virginica 

iris_setosa<-iris[iris$Species=="setosa",] # 50
iris_versicolor <- iris[iris$Species=="versicolor",] # 50
iris_virginica <- iris[iris$Species=="virginica",] # 50
iris_train22 <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test22 <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])
library(party)
mod.3 <- ctree(iris$Species~.,data =iris_train22)
plot(mod.3)
pred.1 <-predict(mod.3,iris_test22)

table(pred.1,iris_test22$Species)     
#accuracy 0.65  