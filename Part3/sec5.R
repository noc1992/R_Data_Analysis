var1 <- "aaa"
var2 <- 111
var3 <- Sys.Date()
var4 <- c("a","b","c")

'i said "im a man of my word"'


seq1 <- 1:5
seq1
seq2 <- 'a':'g'
date1 <- seq(from=as.Date('2019-05-30'), to=as.Date('2019-06-06'),by=1)
date1

date2 <- seq(from=as.Date('2019-01-30'), to=as.Date('2019-12-06'), by='month')
date2
objects()        # 옆에 있으니 굳이 확인하려고 코드 안쳐도 됨

rm(var1)
rm(list = ls())
.hidden
rm(.hidden)
