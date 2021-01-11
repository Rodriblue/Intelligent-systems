#install.packages(c('tm', 'SnowballC', 'wordcloud', 'topicmodels'))
#install.packages(c('rpart', 'rpart.plot', 'e1071', 'nnet'))
library(tm)
library(SnowballC)
library(wordcloud)
library(rpart)
library(rpart.plot)
library(e1071)
library(nnet)

# Leemos de una carpeta llena de .txt
source.pos = DirSource("Datasets/Texts", encoding = "UTF-8")
review_corpus = Corpus(source.pos)

# Procesamos el corpus.
review_corpus = tm_map(review_corpus, content_transformer(tolower))
review_corpus = tm_map(review_corpus, removeNumbers)
review_corpus = tm_map(review_corpus, removePunctuation)
review_corpus = tm_map(review_corpus, removeWords, stopwords("english"))
review_corpus =  tm_map(review_corpus, stripWhitespace)
review_corpus = tm_map(review_corpus,stemDocument)

inspect(review_corpus[1])

review_dtm <- DocumentTermMatrix(review_corpus)
review_dtm

inspect(review_dtm[500:505, 500:505])

review_dtm = removeSparseTerms(review_dtm, 0.99)
review_dtm

findFreqTerms(review_dtm, 1000)

freq = data.frame(sort(colSums(as.matrix(review_dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))




review_dtm_tfidf <- DocumentTermMatrix(review_corpus, control = list(weighting = weightTfIdf))

review_dtm_tfidf = removeSparseTerms(review_dtm_tfidf, 0.95)
review_dtm_tfidf

# The first document
inspect(review_dtm_tfidf[2,1:20])
matrix = as.matrix(review_dtm_tfidf)


freq = data.frame(sort(colSums(as.matrix(review_dtm_tfidf)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(1, "Dark2"))

reviews_final = cbind(subset(train, select=c("sentiment")), matrix)
reviews_final$sentiment = as.factor(reviews_final$sentiment)

id_train  <- sample(nrow(reviews_final),nrow(reviews_final)*0.80)
reviews_final.train = reviews_final[id_train,]
reviews_final.test = reviews_final[-id_train,]

reviews_final.tree = rpart(sentiment~.,  method = "class", data = reviews_final.train);  
prp(reviews_final.tree)

reviews_final.glm = glm(sentiment~ ., family = "binomial", data =reviews_final.train, maxit = 100);  
reviews_final.svm = svm(sentiment~., data = reviews_final.train);
reviews_final.nnet = nnet(sentiment~., data=reviews_final.train, size=1, maxit=500)

pred.tree = predict(reviews_final.tree, reviews_final.test,  type="class")
table(reviews_final.test$sentiment,pred.tree,dnn=c("Obs","Pred"))
