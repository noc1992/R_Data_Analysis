mpg

mpg_a = filter(mpg, manufacturer== 'audi')
mpg_t = filter(mpg, manufacturer== 'toyota')
mean(mpg_a$cty); mean(mpg_t$cty)

mpg %>%
  filter(manufacturer %in% c("audi", "toyota")) %>%
  group_by(manufacturer) %>%
  summarise(aver = mean(cty))

mpg %>%
  filter(manufacturer == "audi") %>%
  select(manufacturer, model, class,hwy) %>%
  arrange(desc(hwy)) %>%
  head(5)
