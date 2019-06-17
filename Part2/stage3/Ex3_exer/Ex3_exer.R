library(readxl)
library(stringr)
library(KoNLP)
library(reshape2)
library(plotly)
library(extrafont)
windowsFonts()
windowsFonts(malgun = "맑은 고딕")
library(RColorBrewer)
theme_update(text = element_text(family = "malgun"))


###### 3-3 #######3
data1 <- read.csv("data/2000-2013년 연령별실업율_15-19세.csv",header=T)
data2 <- read.csv("data/2000-2013년 연령별실업율_20-29세.csv",header=T)
data3 <- read.csv("data/2000-2013년 연령별실업율_30-39세.csv",header=T)
data4 <- read.csv("data/2000-2013년 연령별실업율_40-49세.csv",header=T)
data5 <- read.csv("data/2000-2013년 연령별실업율_50-59세.csv",header=T)
data6 <- read.csv("data/2000-2013년 연령별실업율_60세이상.csv",header=T)
data1
array(dim=c(13,14))

a1 <- data1[13,]
a2 <- data2[13,]
a3 <- data3[13,]
a4 <- data4[13,]
a5 <- data5[13,]
a6 <- data6[13,]
t(a1)
t(a2)
t(a3)
t(a4)
t(a5)
t(a6)
m1 <- melt(a1, id=c('월'), 
     variable.name='년도', value.name='평균')
colnames(m1) <- c("월","년도","10대")

m2 <- melt(a2, id=c('월'), 
           variable.name='년도', value.name='평균')
colnames(m2) <- c("월","년도","20대")

m3 <- melt(a3, id=c('월'), 
           variable.name='년도', value.name='평균')
colnames(m3) <- c("월","년도","30대")

m4 <- melt(a4, id=c('월'), 
           variable.name='년도', value.name='평균')
colnames(m4) <- c("월","년도","40대")

m5 <- melt(a5, id=c('월'), 
           variable.name='년도', value.name='평균')
colnames(m5) <- c("월","년도","50대")

m6 <- melt(a6, id=c('월'), 
           variable.name='년도', value.name='평균')
colnames(m6) <- c("월","년도","60대")

ms1 <- m1%>%
  select("년도","10대")
ms2 <- m2%>%
  select("년도","20대")
ms3 <- m3%>%
  select("년도","30대")
ms4 <- m4%>%
  select("년도","40대")
ms5 <- m5%>%
  select("년도","50대")
ms6 <- m6%>%
  select("년도","60대")

mm1 <- merge(ms1,ms2, by="년도")
mm2 <- merge(mm1,ms3, by="년도")
mm3 <- merge(mm2,ms4, by="년도")
mm4 <- merge(mm3,ms5, by="년도")
mm5 <- merge(mm4,ms6, by="년도")
mm5

mm7 <- melt(mm5, id=c('년도'))
mm7

count1 <- ggplot(mm7, aes(x=년도, y=value, group=variable, fill=variable,color=variable,shape=variable)) +
  geom_line(size=1) +
  geom_point(size=3) +
  theme_bw(base_family="malgun", base_size = 8) +
  ggtitle('연령별 실업율 (10대포함)') +
  theme(plot.title = element_text(family="malgun",
                                  face = "bold",
                                  hjust = 0.5, 
                                  size = 15, 
                                  color = "black")) 


count1
ggplotly(count1)


############# 3-5 ############
options(digits = 1)
data01 <- read.csv("data/버스노선별이용현황합계.csv",header=T)
View(data)

df01 <- melt(data01,id.vars = 1)
df01
colnames(df01) <- c("노선번호","기준","인원")
ggplot(df01,aes(x=노선번호,y=인원/1000,fill=기준)) + 
  geom_bar(stat = "identity" ,position="dodge") + 
  labs(y="탑승인원 (단위:천명)") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,color = "darkblue",size=7)) +
  geom_text(aes(label=round(인원/1000)), size=3, hjust=1, color='black') +
  ggtitle(paste('서울 주요 마을 버스 이용 승객 현황(2014년 1월)', '\n', '(단위:천명)')) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue")) +
  coord_cartesian(xlim = c(1,25), ylim = c(1,300))



######### 3-7 ################

data001 <- read.csv("data/마포09번이용현황.csv")
data001
fcdata <- factor(data001$정류소명)
sapply(fcdata[1,32],)
fcdata

df001 <- melt(data001, id.vars = 1)
colnames(df001) <- c("정류소명","기준","인원")
df001


ggplot(df001,aes(x=정류소명,y=인원,group=기준,fill=기준,color=기준)) +
  geom_point() +
  geom_line(size=1) +
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 1,color = "black",size=5)) +
  geom_text(aes(label=인원), size=3, hjust=1, color='black') +
  ggtitle(paste('마포 9번이용 승객수')) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))
  
  
