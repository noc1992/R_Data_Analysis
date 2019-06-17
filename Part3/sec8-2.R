install.packages("plyr")
install.packages("dplyr")
library(googleVis)
library(dplyr)
library(plyr)
sort1 <- Fruits$Sales
sort1
sort(sort1)
sort(sort1, decreasing = T)

fruits <- read.csv("Data/fruits_10.csv", header = T)
fruits
ddply(fruits, 'name', summarise, sum_qty=sum(qty),sum_price=sum(price))
ddply(fruits, c('year',  'name'), summarise, max_qty=sum(qty),min_price=sum(price))
ddply(fruits, 'name', transform, sum_qty=sum(qty),sum_price=sum(price))
ddply(fruits, 'name', transform, qty=sum(qty),pct_qty=(100*qty)/sum(qty))


data1 <- read.csv("Data/2013년_프로야구선수_성적.csv")
data2 <- filter(data1, 경기 >120)
data2
data3 <- filter(data1, 경기 >120 & 득점 >80)
data3

data4 <- filter(data1, 포지션 %in% c('1루수','3루수'))
data4

select(data1, 선수명, 포지션, 팀)
select(data1, 순위:타수)

data1 %>%
  select(선수명, 팀, 경기,타수) %>%
  filter(타수 > 400)  %>%
  arrange(desc(타수), desc(경기)) 

data1 %>%
  select(선수명, 팀, 경기, 타수) %>%
  mutate(경기x타수 = 경기*타수)  %>%
  arrange(경기x타수) 

data1 %>%
  select(선수명, 팀, 출루율, 장타율) %>%
  mutate(OPS = 출루율 + 장타율)  %>%
  arrange(desc(OPS)) 

data1 %>%
  group_by(팀)%>%
  summarise(avarage = mean(경기, na.rm = TRUE))

data1 %>%
  group_by(팀) %>%
  summarise_each(funs(mean),경기, 타수)   #funs deprecated -> use list

data1 %>%
  group_by(팀) %>%
  summarise_each(funs(mean,n()),경기, 타수) 
attach(Fruits)
Fruits
data01 <- filter(Fruits, Expenses > 80)
data01

data01 <- filter(Fruits, Expenses > 90 | Sales > 80)
data01 <- filter(Fruits, Expenses==79 | Expenses==91)     #Expense %in% c(79,91)
data01

data02 <- select(Fruits, Fruit:Sales, -Location)
data02

Fruits %>%
  group_by(Fruit) %>%
  summarise_each(funs(sum), Sales)

Fruits %>%
  group_by(Fruit) %>%
  summarise_each(funs(sum), Sales, Profit)

install.packages("ggplot2")
library(ggplot2)

mpg
View(mpg)


































