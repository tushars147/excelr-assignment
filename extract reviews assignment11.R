
library(rvest)
library(XML)
library(magrittr)

aurla <- "https://www.amazon.in/Apple-iPhone-Xs-64GB-Gold/product-reviews/B07J316BT2/ref=cm_cr_arp_d_viewopt_srt?sortBy=recent&pageNumber=1"
amazona_reviews <- NULL
for (i in 0:20){
  murla <- read_html(as.character(paste(aurla,i,sep="=")))
  reva <- murla %>%
    html_nodes(".review-text") %>%
    html_text()
  amazona_reviews <- c(amazona_reviews,reva)
}
amazona_reviews
write.table(amazona_reviews,"iphoneXs.txt",row.names = F)

library(tm)
library(slam)
library(topicmodels)


ipho <- readLines("C:\\Users\\USER\\Documents\\iphoneXs.txt")
ipho

corp <- iconv(ipho,"UTF-8")
corp1 <- Corpus(VectorSource(corp))
inspect(corp1[1:5])

corp1 <- tm_map(corp1, tolower)
corp1 <- tm_map(corp1,removePunctuation)
corp1 <- tm_map(corp1,removeNumbers)

mystop1 <- read.table("C:\\Users\\USER\\Downloads\\stop.txt",header = TRUE)
class(mystop1)
stop_vec1 <- as.vector(mystop1$a)
stop_vec1
aa <- "a"

clean1 <- tm_map(corp1 , removeWords, c(stop_vec1,aa,"phone","product","apple","fan"))
inspect(clean1)
clean1 <- tm_map(clean1,stripWhitespace)
inspect(clean1)
cleaning <- clean1

clean1_tdm <- TermDocumentMatrix(clean1)
clean1_tdm <- as.matrix(clean1_tdm)
clean1_tdm[1:10,1:20]
library(stringr)
library(plyr)

q <- rowSums(clean1_tdm)
q1<- subset(q,q>30)
q1
pos.words <- scan('C:\\Users\\USER\\Downloads\\positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
neg.words <- scan('C:\\Users\\USER\\Downloads\\negative-words.txt', what='character', comment.char=';')
po <- "my name is tushar and i'm a good and bad boy also great"
word.list1 = str_split(po, '\\s+')
words1 = unlist(word.list1)
matching <- match(words1,neg)

word.list2 <- str_split(cleaning, '\\s+')
wrd <- unlist(word.list2)
wrd
inspect(cleaning)

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  list=lapply(sentences, function(sentence, pos.words, neg.words)
  {
    sentence = gsub('[[:punct:]]',' ',sentence)
    sentence = gsub('[[:cntrl:]]','',sentence)
    sentence = gsub('\\d+','',sentence)
    sentence = gsub('\n','',sentence)
    
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    pp=sum(pos.matches)
    nn = sum(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    list1=c(score, pp, nn)
    return (list1)
  }, pos.words, neg.words)
  score_new=lapply(list, `[[`, 1)
  pp1=score=lapply(list, `[[`, 2)
  nn1=score=lapply(list, `[[`, 3)
  
  scores.df = data.frame(score=score_new, text=sentences)
  positive.df = data.frame(Positive=pp1, text=sentences)
  negative.df = data.frame(Negative=nn1, text=sentences)
  
  list_df=list(scores.df, positive.df, negative.df)
  return(list_df)
}

result12 <- score.sentiment(text, pos.words, neg.words)


library(wordcloud)
set.seed(222)
q2 <- sort(rowSums(clean1_tdm),decreasing = TRUE)
wordcloud(words = names(q2),freq = q2,colors = brewer.pal(8,'Dark2'))
library(syuzhet)
xs <- iconv(ipho,"UTF-8")
xs_test <- get_sentences(xs)
head(xs_test)
class(xs_test)

xs_nrc <- get_nrc_sentiment(xs_test)
barplot(colSums(xs_nrc),col=rainbow(50))
#most of them are positive

get_nrc_sentiment("excited")
get_sentiment("excitement",method = "afinn")

xs_sent <- get_sentiment(xs_test,method = "nrc")

poa_v1 <- xs_test
poa_sent1 <- get_sentiment(poa_v1, method="bing")
plot(
  poa_sent, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)


# percentage based figures
percent_vals1 <- get_percentage_values(poa_sent)

plot(
  percent_vals1, 
  type="l", 
  main="Throw the ring in the volcano Using Percentage-Based Means", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)

ft_values1 <- get_transformed_values(
  poa_sent1, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  ft_values1, 
  type ="h", 
  main ="LOTR using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

best_review <- xs_test[which.max(xs_sent)]
best_review

worst_review <-xs_test[which.min(xs_sent)]
worst_review
