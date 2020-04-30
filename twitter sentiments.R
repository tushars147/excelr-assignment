library("twitteR")
library("ROAuth")
cred <- OAuthFactory$new(consumerKey='FXTquJNbgDG2dH81XYVqNZFAb', # Consumer Key (API Key)
                         consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")
library(base64enc)
library(httpuv)

setup_twitter_oauth("FXTquJNbgDG2dH81XYVqNZFAb", # Consumer Key (API Key)
                    "3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO", #Consumer Secret (API Secret)
                    "529590041-qOXLd769cQEUTbXg3iRqCd33pC1K6xoORrGOMJDh",  # Access Token
                    "WlqZJwXFQzf64IuojkbKh1jdT5cnSY8U44pqmz6Sc1d4A")  #Access Token Secret
Tweets <- userTimeline('Trump', n = 1000,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)

write.csv(TweetsDF, "TweetsTrump.csv",row.names = F)

library(rtweet)
library(tm)
trump <- read.csv("C:\\Users\\USER\\Documents\\TweetsTrump.csv")
trump <- trump$text
trump1 <- iconv(trump,"UTF-8")
trump_tdm <- Corpus(VectorSource(trump1))
trump_tdm <- tm_map(trump_tdm, tolower)
trump_tdm <- tm_map(trump_tdm, removePunctuation)
trump_tdm <- tm_map(trump_tdm, removeWords,stopwords(kind = "english"))
trump_tdm <- tm_map(trump_tdm,stripWhitespace)
inspect(trump_tdm)

trump_tdm = gsub('[[:punct:]]',' ',trump_tdm)
trump_tdm = gsub('[[:cntrl:]]','',trump_tdm)
trump_tdm = gsub('\\d+','',trump_tdm)
trump_tdm   = gsub('\n','',trump_tdm)
trump_tdm <- as.character(trump_tdm)
tdm_trump <- TermDocumentMatrix(trump_tdm)
tdm_trump <- as.matrix(tdm_trump)
h <- rowSums(tdm_trump)
h1 <- subset(h,h>10)
h1
library(wordcloud)
h2 <- sort(rowSums(tdm_trump),decreasing = TRUE)
set.seed(222)
wordcloud(words = names(h2),freq = h2,colors = brewer.pal(8,'Dark2'))

#positive and negative words matching 
pos.words <- scan('C:\\Users\\USER\\Downloads\\positive-words.txt', what='character', comment.char=';') 
neg.words <- scan('C:\\Users\\USER\\Downloads\\negative-words.txt', what='character', comment.char=';')

positive_match <- match(names(h),pos.words)
positive_match =!is.na(positive_match)
freq_pos <- h[positive_match]
p_names <-names(freq_pos)

negative_match <- match(names(h),neg.words)
negative_match =!is.na(negative_match)
freq_neg <- h[negative_match]
n_names <- names(freq_neg)
n_names
wordcloud(words = n_names,freq = freq_neg)

#sentimenal analysis
trump <- read.csv("C:\\Users\\USER\\Documents\\TweetsTrump.csv")
trump <- trump$text
trump2 <- iconv(trump,"UTF-8")
library(syuzhet)
trump3 <- get_nrc_sentiment(trump2)
head(trump3,n=5)
barplot(colSums(trump3),col=rainbow(50))

trump_1 <- get_sentences(trump2) 
trump_2 <- get_sentiment(trump_1,method = "bing")
plot(
  trump_2, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

trump_3 <- get_percentage_values(trump_2)
plot(
  trump_3, 
  type="l", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

trump_values1 <- get_transformed_values(
  trump_2, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  trump_values1, 
  type ="l", 
  main ="LOTR using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)
#using afinn
trump_2a <- get_sentiment(trump_1,method = "afinn")
plot(
  trump_2a, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

trump_3a <- get_percentage_values(trump_2a)
plot(
  trump_3a, 
  type="l", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

trump_values1a <- get_transformed_values(
  trump_2a, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  trump_values1a, 
  type ="l", 
  main ="LOTR using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

postivve <- trump_1[which.max(trump_2)]
negativve <- trump_1[which.min(trump_2)]

