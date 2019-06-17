install.packages("devtools")
install.packages("stringi")
devtools::install_github("cardiomoon/kormaps2014")
library(kormaps2014)
library(dplyr)
library(ggplot2)
library(ggiraphExtra)
korpop1
kormap1
korpop1 <- rename(korpop1,
                  pop=총인구_명,
                  name=행정구역별_읍면동)

korpop1 <- ggChoropleth(data = korpop1,
                        aes(fill=총인구_명,
                      map_id = code,
                      tooltip=행정구역별_읍면동),
                  map=kormap1                  )
ggChoropleth(data = tbc,
             aes(fill=NewPts,
                 map_id=code,
                 tooltip=name),
             map = kormap1,
             interactive = T)
