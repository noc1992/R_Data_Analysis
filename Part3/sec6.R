c<-c(1,2,3,4,5)

vec1 <- c
vec1[3]
vec1[-3]
length(vec1)
vec1[1:(length(vec1)-2)]
vec1[(length(vec1))]        #행렬의 끝값

vec1[-1]
vec1[1] <- 10
vec1 <- c(vec1,8)
vec1
append(vec1, 25, after = 1
       )
q2 <- seq(1,20,4)
q2

var9 <- rep(1:3, each=3)
var9

3 %in% var9      #특정값이 있는지 찾는 함수 %in%


prac1 <- c("사과", "배", '감', '버섯', '고구마')
prac1[-3]

prac2 <- c('봄', '여름','가을', '겨울')
prac3 <- c('봄', '여름', '늦여름','초가을')

intersect(prac2, prac3)
prac2 + prac3
union(prac2, prac3)
