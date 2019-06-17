anscombe
ans_x_mean <- anscombe %>%
  select(x1,x2,x3,x4) %>%
  summarise_each(list(mean),x1,x2,x3,x4) 
ans_x_mean

ans_y_mean <- anscombe %>%
  select(y1,y2,y3,y4) %>%
  summarise_each(list(mean),y1,y2,y3,y4) 
ans_y_mean

ans_y_var <- anscombe %>%
  select(y1,y2,y3,y4) %>%
  summarise_each(list(var),y1,y2,y3,y4) 
ans_y_var

ans_x_var <- anscombe %>%
  select(x1,x2,x3,x4) %>%
  summarise_each(list(var),x1,x2,x3,x4) 
ans_x_var
