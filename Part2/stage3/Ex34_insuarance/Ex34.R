library(ggplot2)
library(reshape)
library(extrafont)

count <- read.csv("연도별요양기관별보험청구금액_2004_2013_세로.csv",stringsAsFactors=FALSE)

count<- melt(count,id=c('년도'),variable.name ='병원종류',value.name='금액')
count
colnames(count) <- c('년도','병원종류','금액')
count$금액 <- count$금액/1000000

ggplot(count, aes(x=년도, y=금액, fill=병원종류,color=병원종류)) +
  geom_line(linetype=1,size=1) + geom_point(size=3) + geom_hline(yintercept = seq(0,8000,1000), lty='dotted', size=0.1) +
  theme_bw(base_family = "malgun", base_size = 10) + ggtitle(paste('연도별 요양기관 보험청구 금액 2004~2013', '\n',
                                                                   '(단위(백만원))')) +  theme(plot.title = element_text(family = "malgun",
                                                                                                                   face = "bold",
                                                                                                                   hjust = 0.5,
                                                                                                                   size = 15,
                                                                                                                   color = "darkblue"))
  
