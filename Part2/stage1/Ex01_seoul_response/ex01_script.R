install.packages("rJava")
install.packages("KoNLP")
install.packages("wordcloud")
library(wordcloud)
library(KoNLP)
library(rJava)
library(RColorBrewer)
useSejongDic()


data1 <- readLines("seoul_new.txt")
data1
data2 <- sapply(data1, extractNoun,USE.NAMES = F)
data2
head(unlist(data2))
data3 <-  unlist(data2)
data3

data3 <- gsub("\\d+", "",data3)
data3 <- gsub("서울시", "",data3)
data3 <- gsub("서울", "",data3)
data3 <- gsub("요청", "",data3)
data3 <- gsub("제안", "",data3)
data3 <- gsub(" ", "",data3)
data3 <- gsub("-", "",data3)

write(unlist(data3),"seoul_2.txt")
data4 <- read.table("seoul_2.txt")
data4
wordcount <- table(data4)
wordcount
head(sort(wordcount, decreasing = T),20)

data3 <- gsub("OO", "",data3)
data3 <- gsub("개선", "",data3)
data3 <- gsub("문제", "",data3)
data3 <- gsub("관리", "",data3)
data3 <- gsub("민원", "",data3)
data3 <- gsub("이용", "",data3)
data3 <- gsub("관련", "",data3)
data3 <- gsub("시장", "",data3)

write(unlist(data3), "seoul_3.txt")
data4 <- read.table("seoul_3.txt")
wordcount <- table(data4)
head(sort(wordcount,decreasing = T),20)

palete <- brewer.pal(9, "Set3")
wordcloud(names(wordcount),freq = wordcount,scale = c(7,1),rot.per = 0.25,min.freq = 1,
          random.order = F,random.color = T,colors = palete)


