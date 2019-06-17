#반복문
number <- 0
while(number < 5) {
  print(number)
  number <- number +1
}
number <- -1
while(number <= 10) {
  number <- number + 1
  if(number %% 2 == 1)
    next
    print(number)
}
sum <- 0
for (i in 1:10) {
  sum <- sum + i
}
print(sum)

sum <- 0
for (i in 1:100){
  if (i %% 3 == 0)
  sum <- sum + i 
}
print(sum)

multiple <- function(limit, number) {
  sum <- 0
  for ( i in 1:limit) {
    if (i %% number == 0)
      sum <- sum + i
  }
  return(sum)
}

multiple(1000, 4)

var1 <- read.csv("Data/채소.txt")
var1

x <- var1
for (i==0 in 1:5) {
  print(x)
  if (x==3) {
    next
  }
  
  
}

library(stringr)
for (i in 1:5) {
  line <- ''
  for (k in 1:i) {
    line <- str_c(line, '$')
    
  }
  print(line)
}

for (i in 1:5) {
  line <- ''
  if (i != 1) {
    for (k in 1:(i-1))
      line <- str_c(line, '')
  }
  for (k in i:5){
    line <- str_c(line, '#')
  }
  print(line)
}

for (i in 1:9){
  for(j in 1:9){
    sum = i * j
    print(sum)
  }
}

str_c(line)

for (i in 1:3) {
  line <- ''
  for (k in 1:i) {
    line <- str_c('#', line)
  }
  for (k in i:3){
    line <- str_c(line, '#')
  }
  print(line)
}

for (i in 1:5) {
  line <- ''
  for ()
  if (i != 1) {
    for (k in 1:(i-1))
      line <- str_c(line, '')
  }
  for (k in i:5){
    line <- str_c(line, '#')
  }
  print(line)
}

## 연습문제 -> 1.구구단 ( 1 x 2 = 2 와같이 표기) 2.다이아몬드 만들기 3.약수 배수 구하는 function 
