?read.csv          # help function

vec1 <- c(1:5)
vec2 <- c('a','b','c','d','e')
max(vec1)
max(vec2)
mean(vec2)               #NA
min(vec1)
sd(vec1)
sd(vec2)              #x
sum(vec1)
var(vec1)

library(googleVis)
Fruits
aggregate(Sales ~ Year, Fruits ,sum )
aggregate(Profit ~ Year, Fruits ,sd)
aggregate(Year ~ Fruit, Fruits,max)

mat1 <- matrix(c(1:6),nrow = 2, byrow = T);
apply(mat1, 1, sum) # 1 - 행 단위 계산
apply(mat1, 2, sum) # 2 - 열 단위 계산

apply(mat1, 2, prod)

apply(mat1[,c(2,3)], 1, sum)

list1 <- list(Fruits$Sales)
list2 <- list(Fruits$Profit)

lapply(c(list1, list2), max)

sapply(c(list1, list2), max)

attach(Fruits)
tapply(Sales, Fruit, sum)
tapply(Sales, Year , sum)

vec1 <- c(1:5)
vec2 <- seq(10,50,10)
vec3 <- seq(100, 500 ,100)
mapply(sum,vec1, vec2, vec3)

data1 <- read.csv('Data/data1.csv')

data1

attach(data1)

list1 <- list(data1$X2000년)
list2 <- list(data1$X2001년)
list3 <- list(data1$X2002년)
list4 <- list(data1$X2003년)
list5 <- list(data1$X2004년)
list6 <- list(data1$X2005년)
list7 <- list(data1$X2006년)
list8 <- list(data1$X2007년)
list9 <- list(data1$X2008년)
list10 <- list(data1$X2009년)
list11 <- list(data1$X2010년)
list12 <- list(data1$X2011년)
list13 <- list(data1$X2012년)
list14 <- list(data1$X2013년)

sapply(c(list1,list2,list3,list4,list5,list6,list7,list8,list9,list10,list11,list12,list13,list14), sum)

apply(data1[,c(2:13)], 2, sum)    # apply(data1[c(1:5),c(2:13)], 2, sum) original code
apply(data1[,c(2:15)], 1, sum)
apply(data1[,-1], 1, sum)


data2 <- read.csv('Data/1-4호선승하차승객수.csv')
data2
aggregate(승차 ~ 노선번호, data2, sum) 
attach(data2)
tapply(승차, 노선번호, sum)
tapply(하차, 노선번호, sum)

list01 <- list(data2$승차)
list01
list02 <- list(data2$하차)
sapply(c(list01,list02), sum)

sapply(data2[,c(3,4)], sum)

aggregate(승차+하차 ~ 노선번호, data2, sum)         # simple thinking '+'

aggregate(승차 ~ 노선번호, data2, sum)
aggregate(하차 ~ 노선번호, data2, sum)

