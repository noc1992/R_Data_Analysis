print(15%/%2)        # 나눗셈의 몫
print(5%%2)           # 나눗셈의 나머지 
print(5^2)

10000000 + 10000000
1/10000000000

as.numeric('1') + as.numeric('2')

'first'
"second"
first 
first <- 1
first
second

3 & 1
3 | 0
!0
!3

cat(1, NA, 2)
cat(1, NULL, 2)

sum(1, NA, 2)
sum(1, NULL, 2)

sum(1, NA, 2, na.rm = T)    #결측치 처리법


txt1 <- read.csv("factor_test.csv")
txt1

factor1 <- factor(txt1$blood)
factor1
factor2 <- factor(txt1$location)
factor2
summary(factor1, stringasFactors = FALSE)
View(txt1)


Sys.Date()
Sys.time()
as.Date()
as.Date("10-02-13")
as.Date("01-11-2014", format="%d-%m-%Y")

as.Date(160, origin="2019-05-30")
as.Date("2019-11-14") - Sys.Date()
as.POSIXct("2019-05-31 09:00:00") - as.POSIXct("2019-05-30 09:44:00")    

install.packages("lubridate")
library(lubridate)

now()
date <- now()
month(date, label = F)
wday(date, label = T) ## label = T 월이나 요일 라벨 표시

date+months(3)












