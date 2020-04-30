library(rvest)
library(XML)
library(magrittr)

aurl <- "https://www.amazon.in/Apple-MacBook-Air-13-3-inch-Integrated/product-reviews/B073Q5R6VR/ref=cm_cr_arp_d_paging_btm_3?showViewpoints=1&pageNumber"
amazon_reviews <- NULL
for (i in 1:100){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
amazon_reviews
write.table(amazon_reviews,"apple_mac.txt",row.names = F)
install.packages("tm")
library(tm)

install.packages("slam")
library(slam)

install.packages("topicmodels")
library(topicmodels)

apple <- readLines("C:\\Users\\USER\\Documents\\apple_mac.txt")
length(apple)

corpus <- iconv(apple, "UTF-8")
corpus1 <- Corpus(VectorSource(corpus))
inspect(corpus1[1:5])

corpus1 <- tm_map(corpus1, tolower)

corpus1 <- tm_map(corpus1, removePunctuation)

corpus1 <- tm_map(corpus1,removeNumbers)
mystop <- read.table("C:\\Users\\USER\\Downloads\\stop.txt",header = TRUE)
class(mystop)

stop_vec <- as.vector(mystop$a)
stop_vec
aa <- "a"
cleanset <- tm_map(corpus1 , removeWords, c(stop_vec,aa,"air","wont","apple","macbook","pro","windows","amazon","laptop","display","battery","mac","good","product","great"))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

clean_tdm <- TermDocumentMatrix(cleanset)
clean_tdm
clean_tdm <- as.matrix(clean_tdm)
clean_tdm[1:10,1:20]
w1 <- rowSums(clean_tdm)
w2 <-subset(w1,w1 >30)
w2
barplot(w1,las=2,col = rainbow(50))
library(wordcloud)
w3 <- sort(rowSums(clean_tdm),decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w3),freq = w3,colors = brewer.pal(8,'Dark2'))

#sentimental analysis
apple <- readLines("C:\\Users\\USER\\Downloads\\apple (1).txt")
reviews1 <- iconv(apple, "UTF-8")
library("syuzhet")
my_test <- get_sentences(reviews1)
class(my_test)
head(my_test)

my_test_sent <- get_nrc_sentiment(my_test)
head(my_test_sent)
barplot(colSums(my_test_sent))

my_test_bing <- get_sentiment(my_test, method = "bing")
head(my_test_bing)

afinn_test <- get_sentiment(my_test, method = "afinn")
head(afinn_test)

plot(my_test_bing, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
most_negative <- my_test[which.min(my_test_bing)]
most_negative

most_postive <- my_test[which.max(my_test_bing)]
most_postive

poa_v <- my_test
poa_sent <- get_sentiment(poa_v, method="bing")
plot(
  poa_sent, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)


# percentage based figures
percent_vals <- get_percentage_values(poa_sent)

plot(
  percent_vals, 
  type="l", 
  main="Throw the ring in the volcano Using Percentage-Based Means", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)

ft_values <- get_transformed_values(
  poa_sent, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  ft_values, 
  type ="h", 
  main ="LOTR using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

# categorize each sentence by eight emotions
nrc_data <- get_nrc_sentiment(my_test)
nrc_score_sent <- get_nrc_sentiment(most_negative)
nrc_score_word <- get_nrc_sentiment('grim')
# subset

sad_items <- which(nrc_data$negative > 0)
head(my_test[sad_items])

# To view the emotions as a barplot
barplot(sort(colSums(prop.table(nrc_data[, 1:10]))), horiz = T, cex.names = 0.7,
        las = 1, main = "Emotions", xlab = "Percentage",
        col = 1:8)


