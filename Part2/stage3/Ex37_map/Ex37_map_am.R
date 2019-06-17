install.packages("ggiraphExtra")
install.packages("mapdata")
install.packages("mapproj")
install.packages("gridExtra")
library(gridExtra)
library(mapproj)
library(ggiraphExtra)
library(maps)
library(mapdata)
library(tibble)
library(reshape2)
library(plotly)
library(extrafont)
windowsFonts()
windowsFonts(malgun = "맑은 고딕")
library(RColorBrewer)
theme_update(text = element_text(family = "malgun"))
library(KoNLP)


str(USArrests)
head(USArrests)
summary(USArrests)

ud <- USArrests

ud <- rownames_to_column(ud, var = "state")
ud
ud$state <- tolower(ud$state)
head(ud)

states_map <- map_data("state")
head(states_map)

ggChoropleth(data = ud,
             aes(fill=Murder,
                 map_id=state),
             map=states_map)

ggChoropleth(data = ud,
             aes(fill=Rape,
                 map_id= state),
             map = states_map,
             interactive = T)

M <- ggChoropleth(data = ud,
             aes(fill=Murder,
                 map_id= state),
             map = states_map) +
  xlab("경도") + ylab("위도") +
  ggtitle(("미국의 주별 살인범죄 분포")) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))

R <- ggChoropleth(data = ud,
             aes(fill=Rape,
                 map_id= state),
             map = states_map) +
  xlab("경도") + ylab("위도") +
  ggtitle(("미국의 주별 강간범죄 분포")) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))

P <- ggChoropleth(data = ud,
             aes(fill=UrbanPop,
                 map_id= state),
             map = states_map) +
  xlab("경도") + ylab("위도") +
  ggtitle(("미국의 주별 인구 분포")) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))

A <- ggChoropleth(data = ud,
             aes(fill=Assault,
                 map_id= state),
             map = states_map) +
  xlab("경도") + ylab("위도") +
  ggtitle(("미국의 주별 강도범죄 분포")) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))

grid.arrange(M,A,P,R, ncol=2, top = "미국 주별 강력범죄 분포") 
