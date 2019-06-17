install.packages("reshape2")
install.packages("stringr")
library(stringr)
library(reshape2)

Fruits
melt(Fruits,id='Year')

dcast(mtest, name~var_name,sum)

fruits <- c('apple', 'Apple', 'banana')
str_detect(fruits, 'A')
str_detect(fruits, '^a')               # ^: head of the word $ : tail of the word
str_detect(fruits, 'a$')
str_detect(fruits, '^[aA]')           # a or A
str_detect(fruits, '[aA]')            # detect all of the a or A
str_detect(fruits, regex('a', ignore_case = F))
str_count(fruits, 'a')
str_c('apple', 'banana')
str_c("Fruits:", fruits)
str_c(fruits, "name is ", fruits)
str_c(fruits, collapes="")
str_c(fruits, collapes=".")

str_dup(fruits, 3)
str_length('apple')
str_length('과일일')
str_locate('allocate', 'a')
str_replace('apple', 'p', '*')
str_replace_all('apple', 'p', '*')

fruits <- str_c('apple','/', 'orange', '/', 'banana')
fruits
str_split(fruits,"/")
str_sub(fruits, start = 1, end = 3)
str_trim(" balnk  ")




