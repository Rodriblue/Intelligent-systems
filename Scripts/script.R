#Author: Rodrigo José Arias Capel
library(tm)
library(SnowballC)
library(wordcloud)
library(rpart)
library(rpart.plot)
library(e1071)
library(nnet)
library(ggplot2)

# Ponga la ubicación del dataset
dir <- "C:\\Users\\RandomShowAnimations\\Documents\\Intelligent systems\\Datasets\\IMDB Dataset.csv"

reviews <- read.csv(dir, header = TRUE)

## Reducimos el dataset a la mitad
smp_size <- floor(1 * nrow(reviews))

## Seed para que sea siempre el mismo resultado
set.seed(123)
train_ind <- sample(seq_len(nrow(reviews)), size = smp_size)

train <- reviews[train_ind, ]

# Eliminamos los text creados anteriormente
unlink("Datasets/Texts", recursive = TRUE)
dir.create("Datasets/Texts")
for(i in 1:nrow(train)) {
  text <- as.character(train[i,]$review)
  if(i<10){
    name = sprintf("Datasets/Texts/0000%d.txt",i)
  }
  else if(i<100){
    name = sprintf("Datasets/Texts/000%d.txt",i)
  }
  else if(i<1000){
    name = sprintf("Datasets/Texts/00%d.txt",i)
  }
  else if(i<10000){
    name = sprintf("Datasets/Texts/0%d.txt",i)
  }
  else if(i<100000){
    name = sprintf("Datasets/Texts/%d.txt",i)
  }
  print(name)
  fileConn<-file(name)
  writeLines(text, fileConn)
  close(fileConn)
}

# Leemos de una carpeta llena de .txt
source.pos = DirSource("Datasets/Texts", encoding = "UTF-8")
review_corpus = Corpus(source.pos)

# Procesamos el corpus.
review_corpus = tm_map(review_corpus, content_transformer(tolower))
review_corpus[["00001.txt"]][["content"]]
review_corpus = tm_map(review_corpus, removeNumbers)
review_corpus[["00001.txt"]][["content"]]
review_corpus = tm_map(review_corpus, removeWords, c(stopwords(),"film","films","movie","movies","br"))
review_corpus[["00001.txt"]][["content"]]
review_corpus = tm_map(review_corpus, removePunctuation)
review_corpus[["00001.txt"]][["content"]]
review_corpus = tm_map(review_corpus, stripWhitespace)
review_corpus[["00001.txt"]][["content"]]
review_corpus = tm_map(review_corpus, stemDocument)
review_corpus[["00001.txt"]][["content"]]

inspect(review_corpus[1])

review_dtm <- DocumentTermMatrix(review_corpus)
review_dtm

inspect(review_dtm[500:505, 500:505])

review_dtm = removeSparseTerms(review_dtm, 0.99)
review_dtm

findFreqTerms(review_dtm, 1000)

freq = data.frame(sort(colSums(as.matrix(review_dtm)), decreasing=T))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(3, "Dark2"))

high.freq=tail(sort(colSums(as.matrix(review_dtm)), decreasing=T),n=10)
hfp.df=as.data.frame(sort(high.freq))
hfp.df$names <- rownames(hfp.df) 
ggplot(hfp.df, aes(reorder(names,high.freq), high.freq)) +
  geom_bar(stat="identity",fill = "#379AD5") + coord_flip() + 
  xlab("Terms") + ylab("Frequency") +
  ggtitle("Term frequencies")

freq=rowSums(as.matrix(review_dtm))
plot(sort(freq, decreasing = T),col="blue",main="Word frequencies", xlab="Frequency-based rank", ylab = "Frequency")

review_dtm_tfidf <- DocumentTermMatrix(review_corpus, control = list(weighting = weightTfIdf))

review_dtm_tfidf = removeSparseTerms(review_dtm_tfidf, 0.95)
review_dtm_tfidf

freq=rowSums(as.matrix(review_dtm_tfidf))

plot(sort(freq, decreasing = T),col="blue",main="Word TF-IDF frequencies", xlab="TF-IDF-based rank", ylab = "TF-IDF")

matrix = as.matrix(review_dtm_tfidf)

freq = data.frame(sort(colSums(as.matrix(review_dtm_tfidf)), decreasing=T))
wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(1, "Dark2"))

reviews_final = cbind(subset(train, select=c("sentiment")), matrix)
reviews_final$sentiment = as.factor(reviews_final$sentiment)

# Dividimos entre set de pruebas y test
id_train  <- sample(nrow(reviews_final),nrow(reviews_final)*0.80)
reviews_final.train = reviews_final[id_train,]
reviews_final.test = reviews_final[-id_train,]

# Arbol de decisión
reviews_final.tree = rpart(sentiment~.,  method = "class", data = reviews_final.train);  
prp(reviews_final.tree)
pred.tree = predict(reviews_final.tree, reviews_final.test,  type="class")
table = table(reviews_final.test$sentiment,pred.tree,dnn=c("Obs","Pred"))
(table[1,1]+table[2,2])/(table[1,1]+table[1,2]+table[2,1]+table[2,2])

# Red neuronal
reviews_final.nnet = nnet(sentiment~., data=reviews_final.train, size=2, maxit=1000)
pred.nnet = predict(reviews_final.nnet, reviews_final.test,  type="class")
table = table(reviews_final.test$sentiment,pred.nnet,dnn=c("Obs","Pred"))
(table[1,1]+table[2,2])/(table[1,1]+table[1,2]+table[2,1]+table[2,2])
