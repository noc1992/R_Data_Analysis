install.packages("sqldf")
library(sqldf)
library(googleVis)
Fruits
sqldf('SELECT * FROM Fruits')
sqldf("select * from Fruits where Fruit ='Apples'")           # "    ' ' "   or '  \' \' '
sqldf('select * from Fruits limit 3')           ## limit 3, 5 세번째 데이터부터 다섯개
sqldf('select * from Fruits order by Year')
sqldf('select * from Fruits order by Year DESC')

sqldf('select Fruit, sum(Sales) from Fruits group by Fruit')
sqldf('select Fruit, sum(Sales), sum(Expenses), sum(Profit) from Fruits group by Fruit')
sqldf('select Year, avg(Sales), avg(Expenses), avg(Profit)  from Fruits group by Year order by avg(profit) desc')
sqldf('select Year, min(Sales), min(Expenses), min(Profit)  from Fruits group by Year order by min(profit) desc')
sqldf('select min(Sales), max(Sales) Fruit from Fruits group by Fruit')   # min max 동시에 안나옴
sqldf('select * from Fruits where Expenses=
      (select max(Expenses) from Fruits)')           #sub quary

sqldf('select * from Fruits where Expenses in
      (select min(Expenses), max(Expenses) from Fruits)')           #안됨 

song <- read.csv("Data/song.csv", header = F, fileEncoding = 'utf-8')
song
label <- c('_id','title','lyrics','girl_group_id')
names(song) <- label
song
GG <- read.csv("Data/girl_group.csv",header = F, fileEncoding = 'utf-8'); GG
names(GG) <- c("_id","name","debut")
GG
sqldf('select GG._id, GG.name, GG.debut, song.title from GG inner join song on song.girl_group_id=GG._id;')

sqldf(c('update Fruits set Profit =60 where Fruit=\'Apple\' and Year=2008', 'select * from Fruits'))






