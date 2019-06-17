library(KoNLP)
library(stringr)
library(wordcloud)
library(RColorBrewer)
useSejongDic()
mergeUserDic(data.frame(readLines("제주도여행지.txt"),"ncn"))
txt <- readLines("jeju.txt")
place <- sapply(txt,extractNoun,USE.NAMES = F)

cdata <- unlist(place)
place <- str_replace_all(cdata, "[^[:alpha:]]","")
place <- gsub(" ", "", place)

txt <- readLines("제주도여행코스gsub.txt")

cnt_txt <- length(txt)

i <- 1
for (i in 1:cnt_txt) {
  data3 <- gsub(txt[i],"",place)
}

place <- Filter(function(x) (nchar(x) >= 2),place)
write(unlist(place), "jeju_2.txt")
rev <- read.table('jeju_2.txt')
nrow(rev)
wordcount <- table(rev)
wordcount
head(sort(wordcount, decreasing = T),30)

palete <- brewer.pal(9,"Set1")
wordcloud(names(wordcount),freq=wordcount,scale = c(7,1),rot.per=0.25,min.freq = 1,
          random.order = F,random.color = T,colors = palete)
