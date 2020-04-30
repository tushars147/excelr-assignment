aurl <- "https://www.imdb.com/title/tt1477834/reviews?ref_=tt_ov_rt"
IMDB_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
length(IMDB_reviews)
write.table(IMDB_reviews,"Aquaman.txt",row.names = F)

aqua <- readLines("C:\\Users\\USER\\Documents\\Aquaman.txt")
library(tm)
library(slam)

aqua <- iconv(aqua,"UTF-8")
corpus <- Corpus(VectorSource(aqua))
inspect(corpus)

corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removeNumbers)
mystop <- read.table("C:\\Users\\USER\\Downloads\\stop.txt",header = TRUE)
class(mystop)
stop_vec <- as.vector(mystop$a)
stop_vec
aa <- "a"
corpus <- tm_map(corpus,removeWords,c(stop_vec,aa,"reviews","whats","marvel","aquaman","film",
                                      "characters","momao","tattooed","underwater","superhero","read","dceu","movie",
                                      "mc2u","woman","review","director","based"))
clean <- tm_map(corpus, stripWhitespace)
inspect(clean[1:5])

clean_ <- TermDocumentMatrix(clean)
clean_ <- as.matrix(clean_)
c <- rowSums(clean_)
c1 <- subset(c,c >20)
c1
barplot(c1,col = rainbow(50))
#so most frequent words were good , excited , pretty

library(wordcloud)
w <- sort(rowSums(clean_), decreasing = TRUE) 
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  20, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)
#the movie has a lot of good reviews as the word cloud indicates

library(syuzhet)
library(ggplot2)
library(reshape2)
library(dplyr)
#sentimental analysis
aqua <- readLines("C:\\Users\\USER\\Documents\\Aquaman.txt")
reviews1 <- iconv(aqua, "UTF-8")

my_test <- get_sentences(reviews1)
class(my_test)
head(my_test)

my_test_sent <- get_nrc_sentiment(my_test)
head(my_test_sent)
barplot(colSums(my_test_sent))
#most of them are positive reviews
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
