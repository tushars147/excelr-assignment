library(arules)
library(arulesViz)
books1 <- read.csv("C:\\Users\\USER\\Downloads\\book.csv")
books1
books2 <- apriori(as.matrix(books1),parameter = list(support=0.002,confidence=0.5,minlen=2))
sortbooks <-sort(books2,by="confidence",decreasing = "TRUE")
plot(sortbooks[1:15],method = "graph",control = list(type="list"))
plot(sortbooks[1:15],method = "grouped",control = list(type="list"))
plot(sortbooks[1:15],method = "paracoord",control = list(type="list"))
plot(sortbooks[1:15],method = "matrix",control = list(type="list"))
p <- head(sortbooks)
detach(package:tm, unload=TRUE)
library(arules)
inspect(p)

books3<-apriori(as.matrix(books1),parameter = list(support=0.002,confidence=0.5,minlen=3))
sortbooks1 <-sort(books3,by="confidence",decreasing = "TRUE")
plot(sortbooks1[1:15],method = "graph",control = list(type="list"))
plot(sortbooks1[1:15],method = "grouped",control = list(type="list"))
plot(sortbooks1[1:15],method = "paracoord",control = list(type="list"))
plot(sortbooks1[1:15],method = "matrix",control = list(type="list"))
inspect(sortbooks1[1:10])

books4<-apriori(as.matrix(books1),parameter = list(support=0.003,confidence=0.8,minlen=2))
sortbooks2 <-sort(books4,by="confidence",decreasing = "TRUE")
plot(sortbooks2[1:15],method = "graph",control = list(type="list"))
plot(sortbooks2[1:15],method = "grouped",control = list(type="list"))
plot(sortbooks2[1:15],method = "paracoord",control = list(type="list"))
plot(sortbooks2[1:15],method = "matrix",control = list(type="list"))
inspect(sortbooks2[1:10])

books5<-apriori(as.matrix(books1),parameter = list(support=0.003,confidence=0.8,minlen=3))
sortbooks3<-sort(books4,by="confidence",decreasing = "TRUE")
plot(sortbooks3[1:15],method = "graph",control = list(type="list"))
plot(sortbooks3[1:15],method = "grouped",control = list(type="list"))
plot(sortbooks3[1:15],method = "paracoord",control = list(type="list"))
plot(sortbooks3[1:15],method = "matrix",control = list(type="list"))
inspect(sortbooks3[1:10])

#so we can say from the above association rules that ItalAtlas and Refbks have strong association
#ItalArt and RefBks should be kept together
#ItalCook and Cookbks should be kept together