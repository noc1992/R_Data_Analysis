library(readxl)
library(ggplot2)
library(MASS)
library(dplyr)
library(foreign)
library(reshape)

wf = read.spss("data/09-1.Koweps_hpc10_2015_beta1.sav", to.data.frame=TRUE)
View(wf)
welfare <- wf

head(welfare)
summary(welfare)

welfare <- rename(welfare,
                  sex = h10_g3,
                  year = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region = h10_reg7)

str(welfare$sex)
summary(welfare$sex)
table(welfare$sex)
dim(welfare$sex)

class(welfare$sex)
