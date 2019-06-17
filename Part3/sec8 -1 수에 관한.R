mat1 <- matrix(c(1:6), nrow = 2, byrow = T)
mat1
a <- rep(1,3)
sweep(mat1, 2, a)
b <- matrix(c(2,2))
b            
sweep(mat1, 1, b)

a <- c(1:5)
length(a)
v2 <- c(1, -2, 3, -5)
abs(v2)
ceiling(2.3)
floor(2.4)
choose(6,2)
func_test1 <- function(a, b) {
     a - b  
}
func_test1(2,3)
func_test2 <- function(a,b) {
  abs(func_test1(a,b))
}
func_test2(2,3)


