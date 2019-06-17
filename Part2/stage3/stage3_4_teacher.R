v <- array(dim=c(10, 10))
for (i in 1:10) {
  v[i,] <- count[, i+1] / 100000
}
v[1,]
colors <- c("violet", "blue", "red", "black", "orange",
            "green", "cyan", "yellow", "brown", "pink")
plot(v[1,], xlab="", ylab="", ylim=c(0,6000), axes=FALSE,
     col="violet", type="o", lwd=2,
     main=paste("연도별 요양 기관별 보험 청구 건수(단위:십만건)","\n", 
                "출처:건강보험심사평가원"))
axis(1, at=1:10, label=colname, las=2)
axis(2, las=1)
for (i in 2:10) {
  lines(v[i,], col=colors[i], type="o", lwd=2)
}

# ggplot으로 변환하기
count2 <- melt(count, id=c('년도'),
               variable.name='병원종류', value.name='건수')
count2$건수 <- count2$건수/100000
options(digits = 2)

count3 <- ggplot(count2, aes(x=년도, y=건수, fill=병원종류, color=병원종류))+
  geom_line(linetype=1, size=1) +
  geom_point(size=3) +
  geom_hline(yintercept=seq(0,5000,500), lty='dashed', size=0.1) +
  theme_bw(base_family="malgun", base_size = 12) +
  ggtitle('연도별 요양기관별 보험청구건수 2001~2013') +
  theme(plot.title = element_text(family="malgun",
                                  face = "bold",
                                  hjust = 0.5, 
                                  size = 15, 
                                  color = "black")) 
count3
ggplotly(count3)