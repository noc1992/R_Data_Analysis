library(KoNLP)
library(wordcloud)
useSejongDic()
data1 <- readLines("remake.txt")
data2 <- sapply(data1, extractNoun,USE.NAMES = F)
data3 <- unlist(data2)
data3 <- Filter(function(x) (nchar(x) <=10),data3)
data3

data3 <- gsub("\\d+", "",data3)
data3 <- gsub("쌍수","쌍꺼풀" "",data3)
data3 <- gsub("쌍커풀", "쌍꺼풀",data3)
data3 <- gsub("메부리코", "매부리코",data3)
data3 <- gsub("\\.", "",data3)
data3 <- gsub(" ", "",data3)
data3 <- gsub("\\'", "",data3)

write(unlist(data3),"remake_2.txt")

data4 <- read.table("remake_2.txt")
data4
nrow(data4)
wordcount <- table(data4)
wordcount

txt <- readLines("성형gsub.txt")

cnt_txt <- length(txt)

i <- 1
for (i in 1:cnt_txt) {
  data3 <- gsub(txt[i],"",data3)
}
data3
data3 <- Filter(function(x) (nchar(x) >= 2),data3)
write(unlist(data3),"remake_2.txt")
data4 <- read.table("remake_2.txt")
data4
nrow(data4)
wordcount <- table(data4)
wordcount
