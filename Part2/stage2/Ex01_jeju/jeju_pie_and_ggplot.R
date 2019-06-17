library(KoNLP)
library(wordcloud)
library(stringr)
library(ggplot2)
useSejongDic()

mergeUserDic(data.frame(readLines("제주도여행지.txt")))
txt <- readLines("jeju.txt")
place <- sapply(txt,extractNoun,USE.NAMES = F)

cdata <- unlist(place)
place <- str_replace_all(cdata, "[^[:alpha:]]","")
place <- gsub(" ", "", place)

txt <- readLines("제주도여행코스gsub.txt")

cnt_txt <- length(txt)

i <- 1
for (i in 1:cnt_txt) {
  place <- gsub((txt[i]),"",place)
}

place <- Filter(function(x) (nchar(x) >= 2),place)
write(unlist(place), "jeju_2.txt")
rev <- read.table('jeju_2.txt')
nrow(rev)
wordcount <- table(rev)
wordcount
head(sort(wordcount, decreasing = T),30)

palete <- brewer.pal(9,"Set1")

top10 <- head(sort(wordcount,decreasing = T),10)
pie(top10,col = brewer.pal(n=10,name="RdBu"),radius = 1,main = "제주코스top10")


pct <- round(top10/sum(top10) *100,1)
names(top10)
lab <- paste(names(top10),"\n",pct,"%")

pie(top10,col = brewer.pal(n=10,name="RdBu"),radius = 1,main = "제주코스top10",cex=0.8,labels=lab)

## ggplot draw
str(top10)
df_top10 <- as.data.frame(top10)
df_top10

ggplot(df_top10,aes(x='',y=Freq,fill=rev)) + geom_bar(stat = 'identity', width = 1)
ggplot(df_top10,aes(x='',y=Freq,fill=rev)) + geom_bar(stat = 'identity', width = 1) + coord_polar("y",start = 0)

## difficult thing
library(dplyr)
install.packages("extrafont")
library(extrafont)
theme_update(text= element_text(family = "malgun"))


options(digits = 2)          ## 소수점 다음 두자리까지 쓸거야
df_top10 <- df_top10 %>%
  mutate(pct = Freq /sum(Freq) * 100) %>%
  mutate(ylabel = paste(sprintf("%s\n%4.1f",rev,pct), '%',sep = '')) %>%
  arrange(desc(rev)) %>%
  mutate(ypos = cumsum(pct) - 0.5*pct)

df_top10
ggplot(df_top10, aes(x='',y=Freq,fill=rev)) + 
  geom_bar(width = 1, stat = 'identity') + coord_polar("y", start = 0) +
  geom_text(aes(y=ypos,label=ylabel),color='black')


##bar chart###

bch <- head(sort(wordcount, decreasing = T),10)
bch
bp <- barplot(bch, main = "바차트" , col = rainbow(10), cex.names = 0.7, las = 2, ylim=c(0,25))
pct <- round(bch/sum(bch) * 100,1)
text(x = bp, y=bch*1.05,labels = paste("(",pct,"%",")"), col = "black",cex = 0.7)




