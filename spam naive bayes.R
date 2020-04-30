sms <- read.csv("C:\\Users\\USER\\Downloads\\sms_raw_NB (4).csv")
prop.table(table(sms$type))
#shows the prop of ham and spam
library(tm)
sms_corpus = Corpus(VectorSource(sms$text))
inspect(sms_corpus)
corpus_clean = tm_map(sms_corpus, tolower)                
corpus_clean = tm_map(corpus_clean, removeNumbers)            
mystop <- read.table("C:\\Users\\USER\\Downloads\\stop.txt",header = TRUE)
class(mystop)
stop_vec <- as.vector(mystop$a)
stop_vec
aa <- "a"
corpus_clean = tm_map(corpus_clean, removeWords,c(stop_vec,aa)) 
corpus_clean = tm_map(corpus_clean, removePunctuation)        
corpus_clean = tm_map(corpus_clean, stripWhitespace) 
inspect(corpus_clean[1:5])

dtm <- TermDocumentMatrix(corpus_clean)

# split the raw data:
sms.train = sms[1:4200, ] # about 75%
sms.test  = sms[4201:5574, ] # the rest

# then split the document-term matrix
dtm.train = dtm[1:4200, ]
dtm.test  = dtm[4201:5574, ]

# and finally the corpus
corpus.train = corpus_clean[1:4200]
corpus.test  = corpus_clean[4201:5574]

# let's just assert that our split is reasonable: raw data should have about 87% ham
# in both training and test sets:
round(prop.table(table(sms.train$type))*100)
library(wordcloud)
wordcloud(corpus.train,min.freq=40,random.order = FALSE)

spam = subset(sms.train, type == "spam")
ham  = subset(sms.train, type == "ham")

wordcloud(spam$text,max.words=40,scale=c(3, 0, 5))
wordcloud(ham$text,max.words=40,scale=c(3, 0, 5))

freq_terms = findFreqTerms(dtm.train, 5)
reduced_dtm.train = DocumentTermMatrix(corpus.train, list(dictionary=freq_terms))
reduced_dtm.test =  DocumentTermMatrix(corpus.test, list(dictionary=freq_terms))

convert_counts = function(x) {
  x = ifelse(x > 0, 1, 0)
  x = factor(x, levels = c(0, 1), labels=c("No", "Yes"))
  return (x)
}
reduced_dtm.train = apply(reduced_dtm.train, MARGIN=2, convert_counts)
reduced_dtm.test  = apply(reduced_dtm.test, MARGIN=2, convert_counts)
library(e1071)
# store our model in sms_classifier
sms_classifier = naiveBayes(reduced_dtm.train, sms.train$type)
sms_test.predicted = predict(sms_classifier,
                             reduced_dtm.test)
confusionMatrix(sms_test.predicted,sms.test$type)
