install.packages("treemap")
library(treemap)
total <- read.csv("data/학생시험결과_전체점수.csv", header=T, sep=",")
treemap(total,vSize="점수",index=c("점수","이름"))
treemap(total,vSize="점수",index=c("조","이름"))
treemap(total,vSize="점수",index=c("점수","이름","조"))

total <- read.table("data/학생별전체성적_new.txt",header= T,sep=",")
total
row.names(total) <- total$이름
total
total <- total[,2:7]
total
stars(total,flip.labels = FALSE,draw.segments = FALSE,frame.plot = TRUE, full = TRUE,
      main = "학생별 과목별 성적분석-STAR Chart")

savePlot("star_1.png", type = "png")
lab <- names(total)
value <- table(lab)
value
pie(value,labels=lab, radius=0.1,cex=0.6,col = NA)

install.packages("fmsb")
library(fmsb)
layout <- data.frame(
  분석력 = c(5,1),
  창의력 = c(15,3),
  판단력 = c(3,0),
  리더력 = c(5,1),
  사교력 = c(5,1)
)
set.seed(123)           #임의의 값

data1 <- data.frame(
  분석력=runif(3,1,5),             #0 ~ 5 까지 숫자중에 3개를 뽑아줘
  창의력=rnorm(3,10,2),            #정규분포 10 평균 2 표준편차
  판단력=c(0.5,NA,3),
  리더력=runif(3,1,5),
  사교력=c(5,2.5,4)
)

data2 <- rbind(layout,data1)
op <- par()