install.packages("ggplot2")
install.packages("gridExtra")
library(gridExtra)
library(ggplot2)
korean <- read.table("data/학생별국어성적_new.txt",header = T, sep=',')
ggplot(korean , aes(x=이름,y=점수)) +
  geom_point()

gg <- ggplot(korean, aes(x=이름,y=점수)) + geom_bar(stat = "identity", fill="green", colour='red')
gg + theme(axis.text = element_text(angle=45,hjust = 1,vjust = 1,color = "blue",size = 8))
score_kem <- read.csv("data/학생별과목별성적_국영수_new.csv",header=T)
score_kem

library(dplyr)
library(plyr)
sort_kem <- arrange(score_kem, 이름,과목)
sort_kem
sort_kem2<- ddply(sort_kem,"이름",transform,누적합계=cumsum(점수))
sort_kem2

sort_kem3 <- sort_kem2 %>%
  group_by(이름) %>%
  mutate(label=cumsum(점수) -0.5*점수)
sort_kem3

sort_kem4 <- sort_kem2 %>%
  group_by(이름) %>%
  mutate(label=cumsum(점수) -0.5*점수, 누적합계 = cumsum(점수))
sort_kem4

ggplot(sort_kem4, aes(x=이름,y=점수,fill=과목)) +
  geom_bar(stat="identity") +
  geom_text(aes(y=label,label=paste(점수,'점')),colour="black",size=4)

score <- read.table("data/학생별전체성적_new.txt",header=T,sep=",")
score[,c("이름","영어")  ]
ggplot(score,aes(x=영어,y=reorder(이름,영어))) +
  geom_point(size=6) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "red",linetype = "dashed"))

mtcars
str(mtcars)

ggplot(mtcars, aes(x=hp,y=mpg)) +
  geom_point(aes(color = factor(am))) +
  geom_line() +
  labs(x="마력", y="연비")

three <- read.csv("data/학생별과목별성적_3기_3명.csv",header=T)
three
sort_score <- arrange(three, 이름,과목)
ggplot(sort_score,aes(x=과목,y=점수,color=이름,group=이름,fill= 이름)) + geom_line() +
  geom_point(size=5,shape=15) + ggtitle("학생별 과목별 성적")


dis <- read.csv("data/1군전염병발병현황_년도별.csv",stringsAsFactor=F)
dis
str(dis)

ggplot(dis,aes(x=년도별,y=장티푸스,group=1)) + geom_area(fill="cyan",alpha=0.4) + geom_line()
