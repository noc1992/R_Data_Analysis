var1 <- seq(1,7,1)
plot(var1)
var2 <- c(2,2,2)
plot(var2)
x <- 1:3
y <- 4:2
plot(x,y, xlim= c(1,11), ylim= c(1,7), xlab = "x축", ylab = "y축",main= "그래프")

v1 <- c(100,130,150,160 ,120)
plot(v1, type = 'o', col="red", ylim = c(0,200) , axes = FALSE,ann = FALSE)
axis(1, at=1:5,labels = c('mon','tue','wen','thu','fri'))
axis(2, ylim=c(50,200))
title(main = 'FRUIT', col.main="red", font.main=4)

par(mfrow =c(1,4))          #print bunch of graphes
plot(v1,type = 'o')
plot(v1,type = 's')
plot(v1,type = 'l')        #press alt and move button 
pie(v1)
barplot(v1)


par(mfrow = c(1,1))
v1 <- c(1:5)
v2 <- c(5:1)
v3 <- c(3:7)
plot(v1,type = 's', col = "red", ylim = c(1,10))
par(new=T)            #중복허용
plot(v2,type = 'o', col = "blue", ylim = c(1,10))
par(new=T)
plot(v3,type = 'l', col = "green",ylim = c(1,10))

legend(4,9,c("v1","v2","v3"),cex = 0.9, col = c("red","blue","green"), lty=1)

s<- 1:5
barplot(x)
barplot(x, horiz = T)
x <- matrix(c(5,4,3,2), nrow = 2)
x
barplot(x)
barplot(x, beside = T)

par(oma=c(0,0,0,0))
v1<- c(100,120,140,160,190)
v2<- c(120,140,150,120,150)
v3<- c(140,180,120,110,150)
qty <- data.frame(BAN=v1,CHE=v2,ORA=v3)
qty
barplot(as.matrix(qty), main = "Fruit's of sales QTY",
        beside = T, col = rainbow(nrow(qty)),ylim = c(0,400))

t(qty)
barplot(t(qty), main = "Fruit's of sales QTY",cex = 0.8,las=1,names.arg = c('mon','tue','wen','thu','fri')
         col = rainbow(length(qty)),space=0.1,ylim = c(0,900))

peach <- c(190,180,200,250,140)
colors <- c()
for (i in length(peach)) {
  if (peach[i] >= 200){
    colors <- c(colors, 'red')
  }
  else if (peach[i] >= 180) {
    colors <- c(colors,'yellow')
  }
  else{
    colors <- c(colors,"green")
  }
}
barplot(peach,main="peach sales",
        names.arg = c('m','tu','w','th','f'),col = colors)


install.packages("plotrix")
library(plotrix)
