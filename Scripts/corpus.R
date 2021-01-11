# Needed for OutOfMemoryError: Java heap space 
library(rJava)
.jinit(parameters="-Xmx4g")
# If there are more memory problems, invoke gc() after the POS tagging

# The openNLPmodels.en library is not in CRAN; it has to be installed from another repository
#install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at")

library(NLP)
library(openNLP) 
library(openNLPmodels.en)
library(tm)

source.pos = DirSource("Datasets/Texts", encoding = "UTF-8")
corpus = Corpus(source.pos)

length(corpus)
meta(corpus[[1]])
inspect(corpus[[1]])

tdm = TermDocumentMatrix(corpus)
tdm

freq=rowSums(as.matrix(tdm))
head(freq,10)

plot(sort(freq, decreasing = T),col="blue",main="Word frequencies", xlab="Frequency-based rank", ylab = "Frequency")
