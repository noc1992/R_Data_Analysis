install.packages("igraph")
library(igraph)
g1 <- graph(c(1,2, 2,3, 2,4, 1,5, 5,5, 3,6))
plot(g1)
str(g1)
name <- c('seo','il','kim','son','no','li','yu','sin','kang','kwang','jung')
pemp <- c('seo','seo','il','kim','kim','seo','li','yu','seo','kang','kwang')
emp <- data.frame(na = name, suna = pemp)
emp
g <- graph.data.frame(emp,directed=T)
plot(g, layout=layout.fruchterman.reingold,vertex.size=8,edge.arrow.size=0.5)
dev.off()

install.packages("devtools")
install.packages("d3Network")
library(d3Network)
install.packages("devtools")
install.packages('d3Network')
install.packages("RCurl")
library(d3Network)
library(RCurl)
library(devtools)
library(igraph)
name <- c('Angela Bassett','Jessica Lange','Winona Ryder','Michelle Pfeiffer',
          'Whoopi Goldberg','Emma Thompson','Julia Roberts','Sharon Stone','Meryl Streep',
          'Susan Sarandon','Nicole Kidman')
pemp <- c('Angela Bassett','Angela Bassett','Jessica Lange','Winona Ryder','Winona Ryder',
          'Angela Bassett','Emma Thompson', 'Julia Roberts','Angela Bassett',
          'Meryl Streep','Susan Sarandon')
emp <- data.frame(이름=name,상사이름=pemp)
d3SimpleNetwork(emp, width=600, height=600, file="graph/d3.html")

g <- read.csv("data/군집분석.csv", head= T )
graph <- data.frame(학생=g$학생,교수=g$교수)
g <- graph.data.frame(graph,directed = T)
plot(g, layout=layout.fruchterman.reingold, vertex.size=2 , edge.arrow.size=0.5,
     vertex.color = "green",vertex.label=NA)
plot(g, layout=layout.kamada.kawai, vertex.size=2 , edge.arrow.size=0.5,
     vertex.color = "green",vertex.label=NA)


library(stringr)
g <- read.csv("data/군집분석.csv", head= T )
graph <- data.frame(학생=g$학생,교수=g$교수)

g <- graph.data.frame(graph,directed = T)

V(g)$name
gubun1 <- V(g)$name
gubun1
gubun <- str_sub(gubun1,start=1,end=1)
gubun

colors <- c()
for  ( i in length(gubun)){
  if (gubun[i] =='S') {
    colors <- c(colors,'red')
  }
  else {
    colors <- c(colors,'green')
  }
}

sizes <- c()
for (i in 1:length(gubun)){
  if (gubun[i] =='S') {
    sizes <- c(sizes,2)
  }
  else {
    sizes <- c(sizes,6)
  }
}

plot(g, layout=layout.fruchterman.reingold,vertex.size=sizes,edge.arrow.size=0.1,vertex.color=colors,vertex.label=NA)
savePlot("graph/군집_색상크기조절_1.png", type="png")

shapes <- c()
for  ( i in length(gubun)){
  if (gubun[i] =='S') {
    shapes <- c(shapes,'circle')
  }
  else {
    shapes <- c(shapes,'square')
  }
}

plot(g, layout=layout.kamada.kawai,vertex.size=sizes,edge.arrow.size=0,vertex.color=colors,vertex.label=NA,vertex.shape= shapes)



virus1 <- read.csv("data/메르스전염현황.csv",header=T)
d3SimpleNetwork(virus1,width = 1000,height = 1000,file="graph/mers.html")
