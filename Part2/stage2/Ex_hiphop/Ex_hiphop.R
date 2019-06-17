# hiphop.txt 파일을 가지고 다음 문제를 해결하여 파워포인트 파일로 제출하시오.
# 
# 1. 워드 클라우드 만들기
# 2. Top 10 단어에 대해서 원 그래프 만들기 (ggplot 사용할 것)
# 3. Top 10 단어에 대해서 막대 그래프 만들기 (ggplot 사용할 것)
# 4. ppt로 작성해서 제출할것 
library(KoNLP)
library(NLP)
library(stringr)
library(tm)
useSejongDic()
library(wordcloud2)

hh <- readLines("hiphop.txt")
hh

corp1 <- Corpus(VectorSource(hh))


corp2 <- tm_map(corp1, stripWhitespace)
corp2 <- tm_map(corp2, tolower)
corp2 <- tm_map(corp2, removeNumbers)
corp2 <- tm_map(corp2, PlainTextDocument)
sword2 <- c(stopwords('en'), "and","but","not")
corp2 <- tm_map(corp2, removeWords,sword2)
View(corp2)

tdm <- TermDocumentMatrix(corp2)


uhh <- unlist(hh)
uhh

fhh <- Filter(function(x) {nchar(x) >= 2},uhh)
fhh

wordcount <- table(fhh)
wordcount

corp1 <- Corpus(VectorSource(hh))
corp1
inspect(corp1)


