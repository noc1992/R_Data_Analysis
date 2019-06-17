#조건식 
myAbs <- function(x) {
  if (x >= 0) {
    return(x) 
  }
  else {
    return(-x)
  }
}
myAbs(4.5)

myAbs2 <- function(x) {
  if (x >= 0) {
    return(x*x) 
  }
    return(x*(-2))
}
myAbs2(-2)

number <- scan()
result <- ifelse(number%%2==0, '짝수','홀수')

#연습문제
exFunc <- function(x) {
  if (x > 5){
    return(1)
  }
  else {
    return(0)
  }
}
exFunc(5)

myf2 <- function(x) {
  if (x > 0){
    return(1)
  }
  else {
    return(0)
  }
}
myf2(-1)

myf3 <- function(x, y) {
  if (x>y) {
    return(x - y)
  }
  else {
    return(y - x)
  }
}
myf3(1,3)

myf4 <- function(x) {
  if (x < 0) {
    return(0)
  }
  else if (5 > x) {
    return(1)
    }
  else {
    return(10)
  }
}
myf4(5)

myf5 <- function(input) {
  if (input =='y' || input == 'Y'){
    return("Yes")
  }
  else {
    return("Not Yes")
  }
}
ip <- scan(what = "")           #scan으로 문자를 받을때는 what=""이 있어야 문자를 받을수 있음
myf5(ip)

myIntD <- function(a, b, c) {
  D = b*b -4 * a * c
  if (D > 0) {
    return(2)
  } else if (D == 0) {
    return(1)
  } else {
    return(0)
  }
}

myRealD <- function(a, b, c) {
  D = b*b - 4*a*c
  if (abs(D) < 1e-10) {
    return(1)
  } else if (D > 0) {
    return(2)
  } else {
    return(0)
  }
}


