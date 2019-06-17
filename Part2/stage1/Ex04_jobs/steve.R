install.packages("tm")
install.packages("wordcloud2")
library(tm)
library(wordcloud)

data1 <- readLines("steve.txt")
data1

corp1 <- Corpus(VectorSource(data1))
tdm <- TermDocumentMatrix(corp1)

m <- as.matrix(tdm)
colnames(m)
corp2 <- tm_map(corp1,stripWhitespace)
corp2 <- tm_map(corp2, tolower)
corp2 <- tm_map(corp2,removePunctuation)
stopword2 <- c(stopwords('en'), "and","but")
corp2 <- tm_map(corp2, removeWords,stopword2)
corp3 <- TermDocumentMatrix(corp2, control = list(word=c(1,Inf)))
corp3
findFreqTerms(corp3,10)
findAssocs(corp3,"apple",0.5)

corp4 <- as.matrix(corp3)
freq1 <- sort(rowSums(corp4),decreasing = T)
freq2 <- sort(colSums(corp4),decreasing = T)
head(freq2,20)
dim(corp4)
colnames(corp4)
freq2 <- sort(colSums(corp4),decreasing = T)
wordcloud(names(freq1),freq=freq1,scale=c(5,1),min.freq=5)
