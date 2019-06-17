library(RColorBrewer)
install.packages("viridis")
library(viridis)
data <- read.csv("주요선수별성적-2013년.csv")

ops <- data %>%
  select(순위:연봉대비타점율) %>%
  group_by(선수명) %>%
  mutate(ops = sum(출루율, 장타율))%>%
  mutate(rate = ops/연봉 *100)

View(ops)


data_o <- select(ops,선수명, ops ) 
data_o              
#melt(data,id=c('선수명'),variable.name =ops,value.name=rate)

data
# colnames(count) <- c('년도','병원종류','금액')
# count$금액 <- count$금액/1000000

ggplot(ops, aes(x=선수명, y=ops, fill=ops,color=ops)) +
  geom_bar(stat = "identity", color = viridis(25),fill=viridis(25)) + theme(axis.text.x=element_text(angle = 45,hjust = 1,vjust = 1,size=7))
  ggtitle(paste('선수별 OPS')) +  theme(plot.title = element_text(family = "malgun",face = "bold",
                                                                                     hjust = 0.5,
                                                                                    size = 15,
                                                                                    color = "darkblue"))
