library(dplyr)
library()
library(ggplot2)
library(ggiraphExtra)
library(extrafont)
windowsFonts(malgun = "맑은 고딕")
theme_update(text = element_text(family = "malgun"))

### 1번 

vec1 <- c(1:6)
vec2 <- c(10:5)
vec3 <- rep(1:3,each=2)
vec4 <- rep(1:3,2)
vec5 <- seq(from=1,by=2, length.out = 6 )


### 2번

df_score <- data.frame(이름=c("강경학","김태균","이성열","정은원"),중간 = c("90", "78","94","70"), 기말 =c("50","60","90","92"))
df_score

avg <- df_score %>%
  group_by(이름) %>%
  mutate(평균 = mean(c(중간,기말)))
평균  

### 3번







??sep

### 4번
i <- 1
sum(seq(from=i,by=2,length.out = 4))

oddsum <- function(x) {
  if (x > 0) {
    for (x in 1:x) {
      i = sum(seq(from=1,by=2,length.out = x))
    }
  }
  else {
  }
}
oddsum(100)



### 5번

i <- iris

is_b <- i %>%
  select(Species, Sepal.Width) %>%
  filter(Species=="setosa") %>%
  mutate(평균 = mean(Sepal.Width)) %>%
  mutate(표준편차 = var(Sepal.Width))

is_b           ### 이전의 평균 표준편차 입니다.


is_a <- i %>%
  select(Sepal.Width) %>%
  filter(Sepal.Width > 3 && Sepal.Width < 4) %>%
  mutate(평균 = mean(Sepal.Width)) %>%
  mutate(표준편차 = var(Sepal.Width))

is_a           ###이후의 평균 표준편차 입니다.

box <- boxplot()
box_i <- ggplot(is, aes(x=Species,y=Sepal.Width)) + geom_boxplot()
box_i


#### 6번
m <- mpg
View(m)
tm <- m %>%
  filter(manufacturer == "toyota") %>%
  select(model,cty,hwy) %>%
  group_by(model) %>%
  mutate(avg_c = mean(cty)) %>%
  mutate(avg_h = mean(hwy)) %>%
  summarise(avg = mean(c(avg_h,avg_c))) %>%
  arrange(desc(avg))
head(tm,15)

#### 7번

sm <- m %>%
  filter(class == "suv") %>%
  group_by(manufacturer) %>%
  summarise(avg = mean(cty)) %>%
  arrange(desc(avg))
head(sm,7)

##### 8번



