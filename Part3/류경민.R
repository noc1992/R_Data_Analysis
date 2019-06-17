library(ggplot2)
library(googleVis)
library(dplyr)
library(plyr)
mpg

#######1번 입니다.#######
data1 <- mpg
data001 <- data1 %>%
  select(displ, hwy) %>%
  filter(displ <= 4 | displ >=5)  %>%
  arrange(desc(hwy)) 
View(data001)        # displ이 낮은쪽이 더 높음


#######2번 입니다.#######
data1 %>%
  # group_by("toyota") %>%
  group_by("audi") %>%
  summarise(average = mean(cty,na.rm = TRUE))   ## 모두 조사해본 결과 도요타가 더 높음

#######3번 입니다.#######
data1 %>%
  group_by(manufacturer) %>%
  summarise(H_average = mean(hwy))


#######4번 입니다.#######
data002 <- select(data1, class, cty)
data002   #class and cty


#######5번 입니다.#######
data002 %>%
  group_by(class) %>%
  summarise(C_average = mean(cty,na.rm = TRUE))    # 컴팩트가 훨씬 높음 
data1



#######6번 입니다.#######
View(data1)
data002 <- data1 %>%
  select(manufacturer, model,displ, hwy) %>%
  filter(manufacturer == "audi") %>%
  arrange(desc(hwy))  # %>% 삽질
head(data002,5)

#######7번 입니다.#######
p_data1 <- mpg
p_data1

p_data1 %>%
  select(manufacturer:class) %>%
  mutate(ch = cty+hwy) %>%
  arrange(ch)

data003 <- p_data1 %>%
  select(manufacturer:class) %>%
  mutate(a_ch = (cty+hwy)/2) %>%
  arrange(desc(a_ch))
data003
head(data003,3)

mpg <- mpg %>%
  select(manufacturer:class) %>%
  mutate(ch = cty+hwy) %>%
  mutate(a_ch = (cty+hwy)/2) %>%
  arrange(ch, desc(a_ch))         # 3)추가는 뭘 의미하는지 잘 모르겠네요
mpg

#######8번 입니다.#######
num09 <- mpg %>%
  group_by(class) %>%
  summarise(aver_cty = mean(cty))


#######9번 입니다.#######
num09

data004 <- num09 %>%
  select(class,aver_cty) %>%
  mutate(L_cty = desc(aver_cty)) %>%
  arrange(L_cty)
data004


#######10번 입니다.#######
data005 <- mpg %>%
  group_by(manufacturer) %>%
  summarise_each(funs(mean), hwy)
head(data005, 3)


#######11번 입니다.#######
num11 <- data1 %>%
  group_by(manufacturer) %>%
  summarise_each(funs(n()), class)

mpg %>%
  filter(class == 'compact') %>%
  group_by(manufacturer) %>%
  


data005 <- num11 %>%
  select(manufacturer, class) %>%
  arrange(desc(class))
data005
