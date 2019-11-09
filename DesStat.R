#외부 엑셀 파일 불러오기
install.packages("readxl")
library(readxl)

HBAT <- read_excel("HBAT.xlsx", col_names = TRUE, , col_types="guess", na="NA")
str(HBAT)
colSums(is.na(HBAT))

attach(HBAT)
s.table <- table(sex)
s.table
barplot(s.table)
pie(s.table)
boxplot(AGE, main="AGE")
boxplot(JS1, JS2, JS3, JS4, JS5)
boxplot(JP~sex, horizontal=TRUE)

hist(JS5)
summary(JS5)
plot(JS5, AGE)


#다변량 데이터의 기술통계량을 구하고 그 의미 파악하기
str(HBAT)
colSums(is.na(HBAT))

Mat <- HBAT[,c(2, 4, 14:18)]
str(Mat)

table(sex)
table(JP)
table(sex, JP)




#개인적 확인용
summary(EXP)
boxplot(EXP)
summary(JP)
boxplot(JP)
summary(JS1)
boxplot(JS1)
summary(JS2)
boxplot(JS2)
summary(JS3)
boxplot(JS3)
summary(JS4)
boxplot(JS4)
summary(JS5)
boxplot(JS5)
summary(OC1)
boxplot(OC1)
summary(OC2)
boxplot(OC2)
summary(OC3)
boxplot(OC3)
summary(OC4)
boxplot(OC4)
summary(SI1)
boxplot(SI1)
summary(SI2)
boxplot(SI2)
summary(SI3)
boxplot(SI3)
summary(SI4)
boxplot(SI4)

#데이터 불러오기
HBAT <- read_excel("HBAT.xlsx", col_names = TRUE, , col_types="guess", na="NA")
str(HBAT)
colSums(is.na(HBAT))

attach(HBAT)

s.table <- table(sex)
s.table

#표본평균벡터
mu <- c(mean(JP), mean(JS1), mean(JS2), mean(JS3), mean(JS4), mean(JS5))
mu
#표본공분산행렬
s <- cov(HBAT[,c(4, 14:18)])
s

s_JS <- cov(HBAT[,14:18])
s_JS

#표본상관행렬
r <- cor(HBAT[,c(4, 14:18)])
r

r_JS <- cor(HBAT[,c(14:18)])
r_JS



#일변량 변수에 대한 정규성 평가
qqnorm(JS1, pch=19, main=c("JS1", "Normal Q-Q Plot"))
qqnorm(JS1)
qqline(JS1)
qqnorm(JS2)
qqline(JS2)
qqnorm(JS3)
qqline(JS3)
qqnorm(JS4)
qqline(JS4)
qqnorm(JS5)
qqline(JS5)

n <- length(JS1)  #표본수
n
i <- 1:n  #순서
JS1.S <- sort(JS1)  #JS1 순서통계량
JS2.S <- sort(JS2)
JS3.S <- sort(JS3)
JS4.S <- sort(JS4)
JS5.S <- sort(JS5)

edf <- (i-0.5)/n  #경험적 분포
q <- qnorm(edf, mean=0, sd=1)  #N(0,1) 분위수

#JS1 순서통계량과 정규분포 분위수
JS1Q.table <- cbind(i, JS1.S, edf, q)
#JS1 순서통계량과 정규분포 분위수의 공분산행렬
JS1Q.S <- cov(JS1Q.table[,c(2, 4)])
#JS1 순서통계량과 정규분포 분위수의 상관계수
JS1Q.S[1,2]/sqrt(JS1Q.S[1,1]*JS1Q.S[2,2])
cor(JS1Q.table[,c(2,4)])

#JS2 순서통계량과 정규분포 분위수
JS2Q.table <- cbind(i, JS2.S, edf, q)
#JS2 순서통계량과 정규분포 분위수의 공분산행렬
JS2Q.S <- cov(JS2Q.table[,c(2, 4)])
#JS2 순서통계량과 정규분포 분위수의 상관계수
JS2Q.S[1,2]/sqrt(JS2Q.S[1,1]*JS2Q.S[2,2])
cor(JS2Q.table[,c(2,4)])

#JS3 순서통계량과 정규분포 분위수
JS3Q.table <- cbind(i, JS3.S, edf, q)
#JS3 순서통계량과 정규분포 분위수의 공분산행렬
JS3Q.S <- cov(JS3Q.table[,c(2, 4)])
#JS3 순서통계량과 정규분포 분위수의 상관계수
JS3Q.S[1,2]/sqrt(JS3Q.S[1,1]*JS3Q.S[2,2])
cor(JS3Q.table[,c(2,4)])

#JS4 순서통계량과 정규분포 분위수
JS4Q.table <- cbind(i, JS4.S, edf, q)
#JS4 순서통계량과 정규분포 분위수의 공분산행렬
JS4Q.S <- cov(JS4Q.table[,c(2, 4)])
#JS4 순서통계량과 정규분포 분위수의 상관계수
JS4Q.S[1,2]/sqrt(JS4Q.S[1,1]*JS4Q.S[2,2])
cor(JS4Q.table[,c(2,4)])

#JS5 순서통계량과 정규분포 분위수
JS5Q.table <- cbind(i, JS5.S, edf, q)
#JS5 순서통계량과 정규분포 분위수의 공분산행렬
JS5Q.S <- cov(JS5Q.table[,c(2, 4)])
#JS5 순서통계량과 정규분포 분위수의 상관계수
JS5Q.S[1,2]/sqrt(JS5Q.S[1,1]*JS5Q.S[2,2])
cor(JS5Q.table[,c(2,4)])


#흠...
shapiro.test(JS1)


#다변량 정규성 평가
JS <- HBAT[,14:18] 
n <- dim(JS)[1]  #표본수
p <- dim(JS)[2]  #변수의 수

m <- colMeans(JS) #평균벡터
s <- cov(JS) #공분산행렬
solve(s)  #공분산행렬의 역행렬
d2 <- mahalanobis(JS, m, s)  #마할라노비스 거리
d2 <- sort(d2)
i <- 1:n  #순서
edf <- (i-0.5)/n  #경험적 분포
q <- qchisq(edf, p)  #x2(p) 분위수

#마할라노비스 거리 순서통계량과 카이제곱분포
cbind(i, d2, edf, q)

plot(q, d2, pch=19,
     main="Chisquare plot for multivariate normality",
     xlab = "x2 quantile",
     ylab = "ordered Mahalanobis d2") #카이제곱그림
abline(0, 1)


#다변량 왜도와 첨도 함수
sk <- function(x){
  n <- dim(x)[1]    #표본수
  p <- dim(x)[2]    #변수의 수
  
  m <- colMeans(x)  #평균벡터
  
  s <- cov(x)
  s <- (n-1)/n*s    #시그마 최대우도 추정량
  s.Inv <- solve(s) #시그마 최대우도 추정량 역행렬
  
  g <- matrix(rep(NA, n*n), ncol=n)  #gij값 (행렬)
  
  for(i in 1:n){
    for(j in 1:n){
      g[i, j] <- t(x[i,]-m)%*%s.Inv%*%(x[j,]-m)
    }
  }
  
  b1 <- sum(g^3)/n^2     #왜도
  b2 <- sum(diag(g)^2)/n #첨도
  result <- c(b1, b2)
  names(result) <- c("왜도", "첨도")
  return(result)
}

mat <- as.matrix(JS)
sk(mat)

qnorm(0.95)

install.packages("MVN")
library(MVN)
mvn(JS)

summary(JP)


#일반화표본분산
dim(HBAT)[2]  #데이터 전체 변수 갯수
p <- 5

det(s_JS)

#총표본분산
sum(diag(s_JS))
eg_JS <- eigen(r_JS) #직업만족도 행렬의 고유값과 고유벡터
lambda_JS <- eg_JS$values  #직업만족도 행렬의 고유값 람다들
lambda_JS

#변수들 상호상관 - 조건수(1보다 클수록 관련 높음)
inter.corr1 <- lambda_JS[1]/lambda_JS[5]
inter.corr1
inter.corr2 <- 1/lambda_JS[1] + 1/lambda_JS[2] + 1/lambda_JS[3] + 1/lambda_JS[4] + 1/lambda_JS[5]
inter.corr2
inter.corr3 <- (1/p)*(1/lambda_JS[1] + 1/lambda_JS[2] + 1/lambda_JS[3] + 1/lambda_JS[4] + 1/lambda_JS[5])
inter.corr3

#성별에 따른 기술통계 구하기
male <- subset(HBAT, sex==0)
female <- subset(HBAT, sex==1)

summary(male)
summary(female)

#남녀 표본평균벡터
mu_male <- c(mean(male$JP), mean(male$JS1), mean(male$JS2), 
             mean(male$JS3), mean(male$JS4), mean(male$JS5))
mu_female <- c(mean(female$JP), mean(female$JS1), mean(female$JS2), 
              mean(female$JS3), mean(female$JS4), mean(female$JS5))
mu_male
mu_female

mu_male_JS <- colMeans(male[, 14:18])
mu_female_JS <- colMeans(female[, 14:18])
mu_male_JS
mu_female_JS

#산점도 행렬
plot(male[,14:18], pch=19) #양의 선형 관계
plot(female[,14:18], pch=19)

#평행그림
install.packages("lattice")
library(lattice)
parallelplot(male[, 14:18], main="JS of male")
detach("package:shiny", unload = TRUE)
detach("package:lattice", unload = TRUE)

stars(male[, 14:18], labels=male[,1], main = "Star Graph of male")

install.packages("aplpack")
install.packages("XQuartz")
library(aplpack)

#남녀 표본공분산행렬
s_male <- cov(male[,c(4, 14:18)])
s_female <- cov(female[,c(4, 14:18)])

s_male
s_femal

#남녀 표본상관행렬
r_male <- cor(male[,c(4, 14:18)])
r_female <- cor(female[,c(4, 14:18)])

r_male
r_female








