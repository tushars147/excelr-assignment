groc <- read.transactions("C:\\Users\\USER\\Downloads\\Groceries.csv", format="basket")
library(arules)
inspect(head(groc,5))

itemFrequencyPlot(groc,topN=20,main='Relative Item Frequency Plot',type="relative",ylab="Item Frequency (Relative)")
#bags , vegetable , beer are some of the most commonly bought amongst the customers
rules <- apriori(groc,parameter=list(supp=0.001,conf=0.50))
inspect(rules[1:10])
t <-sort(rules, by="conf")
inspect(t[1:10])
#when minlen is not set it shows roll/bun => cheese ... and bags are fundamentally required for most of the items
library(arulesViz)
plot(rules[1:20],method = "graph",control = list(type = "items"))
#from the above plot it indicates hamburger => meat , beer,liquor => Wine ,articles => hygine are one of the top assosication rules

plot(rules[1:20],method = "grouped",control = list(type="items"))

plot(rules[1:20],method = "scatterplot",control = list(type="items"))

itemsets <- eclat(groc, parameter = list(support = 0.002,minlen=2))
tup <-sort(itemsets,by="support",decreasing = TRUE)
inspect(head(tup,10))
plot(tup[1:10],method = "graph")

rule1 <- apriori(groc, parameter = list(supp=0.002,conf=0.80,minlen=3))
x <-sort(rule1, by="lift",decreasing = TRUE)
inspect(x[1:10])
plot(x)
plot(rule1[1:10],method = "graph",control = list(type="items"))
plot(rule1[1:20], method="paracoord", control=list(reorder=TRUE))
rule2 <- apriori(groc, parameter = list(supp=0.001,conf=0.80,minlen=4))
xa <-sort(rule1, by="lift",decreasing = TRUE)
inspect(xa[1:10])
plot(rule2[1:10],method="graph",control = list(type="items"))
plot(rule2[1:20], method="paracoord", control=list(reorder=TRUE))
write(rule1,file='rule.csv',sep = ,)
head(quality(rule1))
#assoication rules for favourable items
#we inspect after trying various parameters:
#sparkling, liquor ,beer , wine should be kept together
#bread,rolls,bakery and cheese should be kept together
#long and life together