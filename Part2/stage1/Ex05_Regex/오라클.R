library(KoNLP)
library(wordcloud)
library(RColorBrewer)

alert <- readLines("oracle_alert_testdb.log")
error_1 <- gsub(" ", "_",alert)
head(unlist(error_1),20)

error_2 <- unlist(error_1)
error_2 <- Filter(function(x) {nchar(x) >= 5},error_2)

error_3 <- grep("^OR-+", error_2, value = T)                 # grep <- find  "^OR 문장의 처음
head(unlist(error_3),20)
write(unlist(error_3),"alert_testdb.log")
