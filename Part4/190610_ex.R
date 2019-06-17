install.packages("rmarkdown")
install.packages("knitr")
library(knitr)
library(rmarkdown)
library(ggplot2)
library(dplyr)
library(plyr)

###1번###
mpg 
ggplot(mpg, aes(x=cty,y=hwy)) + geom_point()

###2번###
midwest
scat_pop <- midwest %>%
  select(poptotal, popasian) %>%
  filter(poptotal < 500000 & popasian < 10000) 
ggplot(scat_pop, aes(x=poptotal,y=popasian)) +
  geom_point()

###3번####

View(mpg)
sort_suv <- subset(mpg, class=="suv")
sort_cty <- sort_suv %>%
  group_by(manufacturer) %>%
  summarise_each(funs(mean), cty)

sort_cty1 <- sort_cty %>%
  arrange(desc(cty))
h_c <- head(sort_cty1,5)

ggplot(h_c, aes(x=manufacturer,y=cty,fill=manufacturer)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=45,size=8) )


### 4번###

ggplot(mpg, aes(class)) + geom_bar() + theme(axis.text.x = element_text(angle=45,size=8) )


###5번###
ec <- economics
graph1 <- ggplot(ec, aes(x=date,y=psavert)) + geom_point()
graph1 + geom_line()

###6번###
cls <- mpg %>%
  select(class, cty) %>%
  filter(class=="compact" | class=="subcompact" | class=="suv") %>%
  group_by(class) %>%
  summarise_each(funs(mean), cty)
cls

ggplot(cls,aes(x=class,y=cty,color=class,fill=class)) + geom_bar(stat = "identity")

###7번###
#(1)#
View(dia) <- diamonds
View(dia)
ggplot(dia,aes(cut, fill=cut)) + geom_bar() + 
  theme(axis.text = element_text(angle=45,size=8))

#(2)#
ggplot(dia, aes(x=cut,y=price,fill=cut)) + geom_bar(stat = "identity")

#(3)#
dia_f <- dia %>%
  group_by(cut=="Fair") 
  #summarise(color,price)

ggplot(dia_f, aes(x=color,y=price,fill=color)) + geom_bar(stat = "identity")

dia_g <- dia %>%
  group_by(cut=="Good") %>%
  summarise(color,price)
dia_g
ggplot(dia_g, aes(x=color,y=price,fill=color)) + geom_bar(stat = "identity")

dia_v <- dia %>%
  group_by(cut=="Very Good") %>%
  summarise(color,price)

ggplot(dia_v, aes(x=color,y=price,fill=color)) + geom_bar(stat = "identity")
