#LOAD DATA

list.files()
list.files(recursive = T)

scan1 <- scan('Data/scan_1.txt')
scan2 <- scan('Data/scan_2.txt')
scan2
scan2 <- scan('Data/scan_2.txt', what = "")             ##read as string 
scan3 <- scan('Data/scan_3.txt',what = "")                        ##it dones't readed when it doesn't have "what="
scan3
input <- scan(what = "")
input

input3 <- readline()                      #read a line of words
input3

input4 <- readLines("Data/scan_4.txt")
input4

read.table('Data/fruits.txt', header = T)   # header -> name on the list
read.table('Data/fruits_2.txt', header = F)
read.table('Data/fruits_2.txt', skip = 2)
read.table('Data/fruits_2.txt', nrows = 2)

read.csv('Data/fruits_4.csv', header = F)
read.table('Data/fruits_4.csv')

label <- c("NO", "name","price", "qty")
furit4 <- read.csv('Data/fruits_4.csv', header = F,col.names = label)
furit4
install.packages("googleVis")
install.packages("sqldf")
library(googleVis)
library(sqldf)
Fruits
write.csv(Fruits, "Fruits_sql.csv", quote = F, row.names = F)
fruits_2 <- read.csv.sql("Data/Fruits_sql.csv", sql = "SELECT * FROM file WHERE year = 2008")
fruits_2

install.packages("XLConnect")
library(XLConnect)

data1 <- loadWorkbook("Data/fruits_6.xls", create=T)
data2 <- readWorksheet(data1, sheet = "sheet1",
                       startRow = 1, endRow = 8, startCol = 1, endCol = 5)
data2

fruits6 <- read.delim("clipboard", header = T)

install.packages("readxl")
library(readxl)

cust_profile <- read_excel("Data/fruits_6.xls", # path
                           
                                                      sheet = "Sheet1", # sheet name to read from
                           
                                                      range = "A2:D6", # cell range to read from
                          
                                                      col_names = TRUE, # TRUE to use the first row as column names
                           
                                                    col_types = "guess", # guess the types of columns
                           
                                                    na = "NA") # Character vector of strings to use for missing values

cust_profile
  

  
  









