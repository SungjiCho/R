# R 기초행렬연산
mx <- matrix(c(2, 5, 8, 7, 6, 7, 1, 2), ncol=4)
my <- matrix(c(10, 12, 27, 68, 35, 21, 14, 50), ncol = 2)
mx
my

m <- mx%*%my
m
solve(m)
t(m)
solve(m)%*%m
round(solve(m), digits=2)
diagM1 <- diag(2, 4)
diagM1
diagM2 <- diag(1, ncol=3, nrow=2)
diagM2

diag(diagM1) <- c(6, 2, 7, 1)
diagM1

x1 <- c(2, 4, 6, 2, 1)
x2 <- c(15, 10, 21, 17, 19)
x3 <- c(5, 7, 8, 6, 4)
x4 <- c(10, 17, 25, 20, 18)
x5 <- c(25, 39, 20, 31, 28)
data <- data.frame(x1, x2, x3, x4, x5)
data

mean(data[,3])
range(data$x5)
median(data[,3])
var(data[,4])
cov(data[,4], data[,5])  #x4, x5 공분산
cov(data[,4:5])  #x4, x5 공분산 행렬
cov(data[,c(3, 4)])  #x4, x5 상관계수 행렬

data$x5
summary(data$x5)
quantile(data$x5)



#2X3 행렬
A <- matrix(c(3, -1, 2, 1, 5, 4), ncol=3, byrow=TRUE)
A
#전치행렬
tA <- t(A)
tA

#벡터 연산
a <- c(1, -2, 3)
b <- c(2, 2, 4)

a+b
t(a)%*%b  #a'b
3*a

#벡터 길이
La <- sqrt(t(a)%*%a)
La
Lb <- sqrt(t(b)%*%b)
Lb

#cos theta
cos.theta <- (t(a)%*%b)/(La*Lb)
cos.theta

#theta
(acos(cos.theta)/pi)*180  #라디안에서 도단위로 변환
   
#2차 형식
A <- matrix(c(3, 1, -1, 5, 2, 4), ncol=3)
B <- matrix(c(1, -1, 2, 3, 3, 5), ncol=3)
A
B
A%*%t(B)  #AB'

#결정식
A <- matrix(c(1, -3, 2, 5), ncol=2)
A
det(A)

A <- matrix(c(1, 0, 1, 2, 3, 5, 1, 4, 6), ncol=3)
A
det(A)

#역행렬
solve(A)

library(MASS)
ginv(A)
detach("package:MASS", unload=TRUE)

#트레이스
A <- matrix(c(3, -sqrt(2), -sqrt(2), 2), ncol=2)
A
sum(diag(A))

#직교행렬
A <- matrix(c(1/sqrt(2), -1/sqrt(2), 1/sqrt(2), 1/sqrt(2)),
            ncol=2)
A
t(A)%*%A

#회전행렬
theta <- 60/180*pi  #라디안 단위 60
A <- matrix(c(cos(theta), sin(theta),
              -sin(theta), cos(theta)), ncol=2)
A
t(A)%*%A


theta <- 45/180*pi  #라디안 단위 45
A <- matrix(c(cos(theta), sin(theta), 
              -sin(theta), cos(theta)), ncol=2)

A
t(A)%*%A


#고유값과 고유벡터
A <- matrix(c(4, 2, -5, -3), ncol=2)
eg <- eigen(A)
eg

eg$values[1]   #행렬 A의 고유값 람다1
eg$vectors[,1] #행렬 A의 고유벡터 e1
eg$values[2]   #행렬 A의 고유값 람다2
eg$vectors[,2] #행렬 A의 고유벡터 e2

A <- matrix(c(3, 1, 1, 3), ncol=2)
eg <- eigen(A)
eg

sum(diag(A))
sum(eg$values)
det(A)
prod(eg$values)  #고유값 곱

#고유값과 고유벡터
P <- eg$vectors
LAMBDA <- diag(eg$values)
P
LAMBDA
P%*%LAMBDA%*%t(P)


install.packages("BWidget")
install.packages("rpanel", dependencies = TRUE)
install.packages("Rcpp", dependencies = TRUE)
install.packages("classInt", dependencies = TRUE)
install.packages("SpatialEpi", dependencies = TRUE)
install.packages("biotools", dependencies = TRUE)
 

