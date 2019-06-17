library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(extrafont)
windowsFonts(malgun = "맑은 고딕")
theme_update(text = element_text(family = "malgun"))

bb <- read.csv('야구성적.csv')
str(bb)
bb <- bb %>%
  mutate(OPS=출루율+장타율, 연봉대비OPS=OPS/연봉*100)
head(bb)
ggplot(bb, aes(x=선수명, y=연봉대비OPS, fill=선수명)) +
  geom_bar(stat='identity') 

palete = c(brewer.pal(12, 'Paired'), 
           brewer.pal(12, 'Paired'), '#56B4E9')
mean_ops_per_income = mean(bb$연봉대비OPS)
ggplot(bb, aes(x=선수명, y=연봉대비출루율)) +
  geom_bar(stat='identity', color=palete, fill=palete) +
  geom_text(aes(y=연봉대비출루율+0.8, label=연봉대비출루율), 
            color="black", size=3) +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1,
                                 colour="black", size=8)) +
  ggtitle('프로야구선수 밥값은 하고 있나?') +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue")) +
  geom_hline(yintercept=mean_ops_per_income, 
             color='red', linetype = 'dashed', size=0.5) 