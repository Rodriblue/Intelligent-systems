library(tm)
library(ggplot2)
library(wordcloud)
library(RWeka)
library(reshape2)
library("readr")

# change this file location to suit your machine
file_loc <- "C:\\Users\\RandomShowAnimations\\Documents\\Intelligent systems\\Datasets\\IMDB Dataset.csv"
# change TRUE to FALSE if you have no column headings in the CSV
reviews <- read.csv(file_loc, header = TRUE)

## 10% of the sample size
smp_size <- floor(0.10 * nrow(reviews))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(reviews)), size = smp_size)

train <- reviews[train_ind, ]

for(i in 1:nrow(train)) {
  text <- as.character(train[i,]$review)
  if(i<10){
    name = sprintf("Datasets/Texts/000%d.txt",i)
  }
  else if(i<100){
    name = sprintf("Datasets/Texts/00%d.txt",i)
  }
  else if(i<1000){
    name = sprintf("Datasets/Texts/0%d.txt",i)
  }
  else if(i<10000){
    name = sprintf("Datasets/Texts/%d.txt",i)
  }
  print(name)
  fileConn<-file(name)
  writeLines(text, fileConn)
  close(fileConn)
}

for(i in nrow(train):1) {
  print(i)
}
