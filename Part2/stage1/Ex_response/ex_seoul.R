#1. 제거해야할 단어를 저장한 gsub 파일을 만들 것

#2. stringr 패키지를 사용할 것

#3. R Markdown 보고서를 만들어서 제출할 것

library(stringr)
library(KoNLP)
library(wordcloud)
useSejongDic()

data1 <- readLines("data/응답소_2015_01.txt")
# data2 <- readLines("data/응답소_2015_02.txt")
# data3 <- readLines("data/응답소_2015_03.txt")
# data4 <- readLines("data/응답소_2015_04.txt")
# data5 <- readLines("data/응답소_2015_05.txt")
# data6 <- readLines("data/응답소_2015_06.txt")
# data7 <- readLines("data/응답소_2015_07.txt")
# data8 <- readLines("data/응답소_2015_08.txt")
# data9 <- readLines("data/응답소_2015_09.txt")
# data10 <- readLines("data/응답소_2015_10.txt")
# data11 <- readLines("data/응답소_2015_11.txt")
# data12 <- readLines("data/응답소_2015_12.txt")

# data <- paste(data1,data2,data3,data4,data5,data6,data7,data8,data9,
             # data10,data11,data12)
data2 <- sapply(data1, extractNoun,USE.NAMES = F)
data2
data3 <- unlist(data2)
data3

data3 <- gsub("\\d+", "", data3)
data3 <- gsub("=", "", data3)
data3 <- str_replace_all(data3,"[^[:alpha:]]"," ")

data3

write(unlist(data3),"data01.txt")
data4 <- read.table("data01.txt")


wordcount <- table(data3)
head(sort(wordcount,decreasing = T),100)
txt <- readLines("exsub.txt")
txt
cnt_txt <- length(txt)
i <- 1
for (i in 1:cnt_txt) {
  data3 <- gsub(txt[i],"", data3)
}
data3


data3 <- Filter(function(x) {nchar >= 2}, data3)

