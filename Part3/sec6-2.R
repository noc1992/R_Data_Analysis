mat1 <- matrix(c(1,2,3,4))
mat2 <- matrix(c(1,2,3,4), nrow = 2)
mat3 <- matrix(c(1,2,3,4), nrow = 2, byrow = T)

mat4 <- matrix(seq(1:9), nrow = 3, byrow = T)
mat4 <- rbind(mat4, c(10,11,12))
mat4 <- cbind(mat4, c(13,14,15,16))
mat4

mat5 <- matrix(c('a','b','c','d'), nrow=2, byrow = T)
mat5
mat6 <- matrix(c('a','b','c','d'), nrow=2, byrow = F)
mat6

mat5[,2]
mat5 <- rbind(mat5, c('초봄','초가을'))
mat5
mat5 <- cbind(mat5, c('h','i','j'))     #세로추가 
mat5
array1 <- array(c(1:12), dim = c(4,3))
array1
array2 <- array(c(1:12), dim = c(2,2,3))
array2


list1 <- list(name = '홍길동', address = '전국', tel = '010-1234-5678',age = 300)
list1$name
list1$birth <- "1000-10-23"
list1
list1$name <- c("leonard", "smart hong")
list1$age <- NULL
list1

No <- c(1:3)
Name <- c('banana','peach','melon')
price <- c(150, 300, 400)

sales <- data.frame(NO=No,NAME=Name, PRICE=price)
sales
subset(sales, price> 200)
subset(sales, price != 300) # !와 =을 같이 붙여서 쓰면 !=

No1 <- c(10,20,30)
Name1 <- c('train','car','airplane')
price1 <- c(1000, 2000, 4000)

sales1 <- data.frame(NO=No1,NAME=Name1, PRICE=price1)
sales

rbind(sales,sales1
      )
df1 <- data.frame(name=c('banana','peach','melon'), price=c(300,200,100))
df2 <- data.frame(name=c('banana','cherry','watermelon'), qty=c(10, 20, 30))
merge(df1,df2)
merge(df1,df2, all=T)

new <- data.frame(name='mango',price=300)
df1 <- rbind(df1,new)
df1
df1 <- rbind(df1, data.frame(name='strawberry',price=240))     #가로 추가 cbind가 세로추가 
df1

no <- c(1,2,3,4,5)
name <- c("seo", "joo","choi","li","yoon")
address <- c("seoul","deajeon","pohang","kyeongju","kyeonggi")
tel <- c(1111,2222,3333,4444,5555)
hobby <- c("reading","art","yolo","loyo","watching")
member <- data.frame(NO=no,NAME=name,ADDRESS=adress, TEL=tel, HOBBY=hobby)
member

member2 <- subset(member, select = c(NO,NAME,TEL))   # 그 해당 이름 차트 빼오기
member2
member3 <- subset(member, select = -TEL)
member3
colnames(member) <- c("TEL","NO","ADDRESS","HOBBY")   #열 위에 이름 바꾸기 -> 이름만 바꾸고 순서는 그대로이고
member             

sales
sales[c(2,3,1)]   #순서 아예 바꾸기입니다 
