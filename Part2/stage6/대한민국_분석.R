library(googleVis)

install.packages("foreign")
library(readxl)
library(ggplot2)
library(MASS)
library(dplyr)
library(foreign)
library(reshape)

raw_we = read.spss(file="data/09-1.Koweps_hpc10_2015_beta1.sav",
                   to.data.frame=T)

welfare = raw_we

head(welfare)
welfare <- rename(welfare,
                  sex = h10_g3,
                  birth = h10_g4,
                  marriage= h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region = h10_reg7)

class(welfare$sex)

table(welfare$sex)
welfare$sex <- ifelse(welfare$sex ==1, "male","female")
qplot(welfare$sex)

class(welfare$income)
summary(welfare$income)
qplot(welfare$income) + xlim(0, 1000)
welfare$income <- ifelse(welfare$income %in% c(0,9999), NA, welfare$income)
table(is.na(welfare$income))

sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(sex) %>%
  summarise(mean_income =mean(income))
sex_income
ggplot(data = sex_income, aes(x = sex, y = mean_income, fill=sex)) + geom_col()


qplot(welfare$birth)
welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

welfare$age <- 2015 - welfare$birth +1
qplot(welfare$age)


age_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income= mean(income))
head(age_income)
summary(age_income)
welfare %>%
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income= mean(income)) %>%
  arrange(desc(mean_income)) %>%
  head(5)


ggplot(data = age_income, aes(x=age, y=mean_income)) + geom_line()


welfare <- welfare %>%
  mutate(ageg = ifelse(age < 30, "young",
         ifelse(age <= 59,"middle", "old")))

table(welfare$ageg)
qplot(welfare$ageg)

ageg_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg) %>%
  summarise(mean_income= mean(income)) 
ageg_income
ggplot(data = ageg_income, aes(x=ageg, y=mean_income,fill=ageg)) +
  geom_col() + scale_alpha_discrete()

welfare <- welfare %>%
  mutate(연령대 = ifelse(age < 30, "20대 이하",
            ifelse(age < 40, "30대",
            ifelse(age < 50, "40대",
            ifelse(age < 60, "50대",
            ifelse(age < 70, "60대","그 이상"))))))

연령대별_임금 <-  welfare %>%
  filter(!is.na(income)) %>%
  group_by(연령대) %>%
  summarise(mean_income= mean(income)) 
ggplot(data = 연령대별_임금, aes(x=연령대, y=mean_income, fill=연령대)) + geom_col()
            
sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg,sex) %>%
  summarise(mean_income= mean(income)) 
sex_income

ggplot(data = sex_income,aes(x=ageg, y=mean_income,fill=sex)) +
  geom_col(position = "dodge") + scale_x_discrete(limits = c("young", "middle","old"))

sex_age <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age,sex) %>%
  summarise(mean_income= mean(income))
head(sex_age)

ggplot(data = sex_age,aes(x=age, y=mean_income,fill=sex,col=sex)) +
  geom_line() 

class(welfare$code_job)

list_job <- read_excel("data/09-1.Koweps_Codebook.xlsx",col_names = T,sheet = 2)
head(list_job)
dim(list_job)

welfare <- left_join(welfare,list_job,id= "code_job")
welfare %>%
  filter(!is.na(code_job)) %>%
  select(code_job,job) %>%
  head(10)

job_income <- welfare %>%
  filter(!is.na(job) & !is.na(income)) %>%
  group_by(job) %>%
  summarise(mean_income =mean(income)) 

top10 <- job_income %>%
  arrange(desc(mean_income)) %>%
  head(10)
top10

ggplot(top10, aes(x=reorder(job,mean_income), y=mean_income, fill=job)) +geom_col() + coord_flip()

bottom10 <- job_income %>%
  arrange(desc(mean_income)) %>%
  head(10)

ggplot(bottom10, aes(x=reorder(job,-mean_income), y=mean_income, fill=job)) +geom_col() + coord_flip()

job_male <- welfare %>%
  filter(!is.na(job) & sex =="male") %>% 
  group_by(job) %>%
  summarise(n =n()) %>%
  arrange(desc(n)) %>%
  head(10)
job_male

ggplot(job_male, aes(x=reorder(job,n), y=n, fill=job)) +geom_col() + coord_flip()

job_female <- welfare %>%
  filter(!is.na(job) & sex =="female") %>% 
  group_by(job) %>%
  summarise(n =n()) %>%
  arrange(desc(n)) %>%
  head(10)
job_female

ggplot(job_female, aes(x=reorder(job,n), y=n, fill=job)) +geom_col() + coord_flip()

welfare$religion <- ifelse(welfare$religion ==1, "yes", "no")
qplot(welfare$religion)


welfare$group_marriage <- ifelse(welfare$marriage ==1, "marriage",
                                 ifelse(welfare$marriage ==3, "divorce",NA))
table(welfare$group_marriage)
table(is.na(welfare$group_marriage))
qplot(welfare$group_marriage)

religion_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(religion,group_marriage) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
    mutate(pct = round(n/tot_group*100,1))
religion_marriage

divorce <- religion_marriage %>%
  filter(group_marriage == "divorce") %>%
  select(religion, pct)
divorce

ggplot(divorce, aes(x = religion, y=pct, fill=religion)) +
  geom_col()


ageg_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(ageg,group_marriage) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100,1))
ageg_marriage


ageg_divorce <- ageg_marriage %>%
  filter(ageg != "young" & group_marriage=="divorce") %>%
  select(ageg,pct)
ageg_divorce


ageg_religion_marriage <- welfare %>%
  filter(!is.na(group_marriage) & ageg != "young") %>%
  group_by(ageg,religion, group_marriage) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100,1))
ageg_religion_marriage


df_divorce <- ageg_religion_marriage %>%
  filter(group_marriage == "divorce") %>%
  select(ageg, religion, pct)

ggplot(df_divorce, aes(x = ageg, y=pct, fill=religion)) +
  geom_col(position = "dodge")


table(welfare$code_region)

list_region <- data.frame(code_region = c(1:7),
                          region = c("서울",
                                     "수도권",
                                     "부산",
                                     "대구",
                                     "대전",
                                     "강원",
                                     "호남제주"))
welfare <- left_join(welfare, list_region, id="code_region")

welfare %>%
  select(code_region, region) %>%
  head

region_ageg <- welfare %>%
  group_by(region,ageg) %>%
  summarise(n=n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100,1))
head(region_ageg)

ggplot(region_ageg, aes(x=region, y=pct, fill=ageg)) + geom_col() + coord_flip()  ## + scale_x_discrete()  영 미들 올드 순으로 보여줄수 있는 방법은 뭔가
  

region_ageg$ageg <- factor(region_ageg$ageg, levels = c("old","middle","young")) # factor 처리를 해주면 순서를 바꿀수 있음


list_order_old <- region_ageg %>%
  filter(ageg == "old") %>%
  arrange(pct)
list_order_old


ggplot(data = list_order_old, aes(x = region, y= pct, fill=region)) + geom_col() + coord_flip() + scale_x_discrete(limits=list_order_old$region)
