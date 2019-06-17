# hiphop.txt 파일을 가지고 다음 문제를 해결하여 파워포인트 파일로 제출하시오.
# 
# 1. 워드 클라우드 만들기
# 2. Top 10 단어에 대해서 원 그래프 만들기 (ggplot 사용할 것)
# 3. Top 10 단어에 대해서 막대 그래프 만들기 (ggplot 사용할 것)
# 4. ppt로 작성해서 제출할것 
devtools::install_github('cran/ggplot2')
install.packages("tidyverse")
library(KoNLP)
library(RColorBrewer)
library(NLP)
library(open)
library(stringr)
library(ggplot2)
library(openNLP)
library(tm)
library(dplyr)
useSejongDic()
library(wordcloud2)
library(wordcloud)
library(ggmosaic)
library(tidyverse)


hh <- readLines("hiphop.txt")
hh1 <- gsub("'\n'","", hh)
hh1 <- gsub("\'", "", hh1)
hh1 <- gsub("'", "", hh1)
hh2 <- tolower(hh1)
hh2 <- sapply(hh2, extractNoun,USE.NAMES = F)
hh2 <- Filter(function(x) {nchar(x) >= 2},hh2)
hh2
hh2 <- unlist(hh2)
hh2
wordcount <- table(hh2)
head(sort(wordcount,decreasing = T),200)


txt <- readLines("hiphop_gsub.txt")
cnt_txt <- length(txt)

i<-1
for (i in 1:cnt_txt) {
  hh2 <- gsub((txt[i]),"",hh2)
}


write(unlist(hh2),"r_hh.txt")
data <- read.table("r_hh.txt")

nrow(data)
wordcount1 <- table(data)
result <- head(sort(wordcount1,decreasing = T),50)
result
wordcloud(names(result),freq = result, scale=c(3,1),
          rot.per=0.5,min.freq=3,random.order=F,random.color=T,
          colors=brewer.pal(n=7,name = "Set2"))
legend(0.25,0.92, "hiphop 노래 가사속의 단어 빈도수", cex = 0.8,fill=NA,border=NA,bg="white",
       text.col="black",text.font=3, box.col="white")
wordcloud2(result,size = 0.5)


###그래프###
result1 <- head(sort(wordcount1,decreasing = T),10)
df <- as.data.frame(result1)
result1
df
write(names(result1), "rs.txt")
red <- read.table("rs.txt")
red
ggplot(df,aes(x=data,y=Freq,fill=Freq)) + geom_bar(stat = 'identity', width = 0.8)

#############
pct = pct = df$Freq /sum(df$Freq) * 100
ylabel = paste(sprintf("%s\n%4.1f",red, pct), '%',sep = '')
pct
ggplot(df) + geom_mosaic(aes(weight=Freq, x='',fill=data)) + coord_polar(theta = "y",start = 0) 
