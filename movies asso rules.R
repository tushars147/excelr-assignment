movies <- read.transactions("C:\\Users\\USER\\Downloads\\my_movies.csv", format="basket")
View(movies)
str(movies)
movies1 <- read.csv("C:\\Users\\USER\\Downloads\\my_movies.csv")
movies1 <-movies1[,6:11]
View(movies1)
write.csv(movies1,'movi.csv')

movi2 <- read.csv("C:\\Users\\USER\\Documents\\movi.csv")
movi2 <-movi2[2:7]
rulem <- apriori(as.matrix(movi2),parameter = list(supp=0.001,conf=0.50))
inspect(rulem[1:10])
plot(rulem,method = "graph",control = list(type="list"))
plot(rulem,method = "mosaic",control = list(type="list"))
plot(rulem,method = "grouped",control = list(type="list"))
plot(rulem[1:10],method = "paracoord",control = list(type="list"))
plot(rulem,method = "scatter",control = list(type="list"))
plot(rulem,method = "matrix",control = list(type="list"))

rulem1 <- apriori(as.matrix(movi2),parameter = list(supp=0.001,conf=0.80,minlen=3))
inspect(rulem)
plot(rulem1,method = "graph",control = list(type="list"))
plot(rulem1,method = "mosaic",control = list(type="list"))
plot(rulem1,method = "grouped",control = list(type="list"))
plot(rulem1,method = "paracoord",control = list(type="list"))
plot(rulem1,method = "scatter",control = list(type="list"))
plot(rulem1,method = "matrix",control = list(type="list"))

rulem2 <- apriori(as.matrix(movi2),parameter = list(supp=0.001,conf=0.80,minlen=4))
inspect(rulem2)
plot(rulem1,method = "graph",control = list(type="list"))
plot(rulem1,method = "mosaic",control = list(type="list"))
plot(rulem1,method = "grouped",control = list(type="list"))
plot(rulem1[1:10],method = "paracoord",control = list(type="list"))
plot(rulem1,method = "scatter",control = list(type="list"))
plot(rulem1,method = "matrix",control = list(type="list"))

#SO there's a strong association which says people who have watched LOTR2 definetly watch LOTR1
#people who watch LOTR1,LOTR2 also watch harry potter
