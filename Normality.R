#데이터 불러오기
HBAT <- read_excel("HBAT.xlsx", col_names = TRUE, , col_types="guess", na="NA")
str(HBAT)

JS <- HBAT[,14:18] 
n <- dim(JS)[1]  #표본수
p <- dim(JS)[2]  #변수의 수


#기술통계량#
summary(JS)
stem(JS$JS1, scale=0.5)  #JS1 줄기-잎그림
stem(JS$JS2, scale=0.5)  #JS2 줄기-잎그림
stem(JS$JS3, scale=0.5)  #JS3 줄기-잎그림
stem(JS$JS4, scale=0.25) #JS4 줄기-잎그림
stem(JS$JS5, scale=2)    #JS5 줄기-잎그림

boxplot(JS$JS1, JS$JS2, JS$JS3, JS$JS4, names=c("JS1", "JS2", "JS3", "JS4"))
boxplot(JS$JS5, names=c("JS5"))



#일변량 정규성 진단#
i <- 1:n  #순서
JS1.S <- sort(JS1)  #JS1 순서통계량
JS2.S <- sort(JS2)  #JS2 순서통계량
JS3.S <- sort(JS3)  #JS3 순서통계량
JS4.S <- sort(JS4)  #JS4 순서통계량
JS5.S <- sort(JS5)  #JS5 순서통계량
edf <- (i-0.5)/n  #경험적 분포
q <- qnorm(edf, mean=0, sd=1)  #N(0,1) 분위수

#순서통계량과 정규분포 분위수
options(max.print=100000)
cbind(i, JS1.S, JS2.S, JS3.S, JS4.S, JS5.S, edf, q)

#Q-Q plot
layout(matrix(1:4), ncol=2))
qqnorm(JS1, pch=19,
       main = c("JS1", "Q-Q Plot"))
qqline(JS1, lwd=2)

qqnorm(JS2, pch=19,
       main = c("JS2", "Q-Q Plot"))
qqline(JS2, lwd=2)

qqnorm(JS3, pch=19,
       main = c("JS3", "Q-Q Plot"))
qqline(JS3, lwd=2)

qqnorm(JS4, pch=19,
       main = c("JS4", "Q-Q Plot"))
qqline(JS4, lwd=2)

qqnorm(JS5, pch=19,
       main = c("JS5", "Q-Q Plot"))
qqline(JS5, lwd=2)



#다변량 정규성 평가
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

#카이제곱그림
plot(q, d2, pch=19,
     main="Chisquare Plot for Multivariate Normality",
     xlab = "x2 quantile",
     ylab = "ordered Mahalanobis d2")
abline(0, 1)

#다변량 왜도와 첨도
install.packages("MVN")
library(MVN)
mvn(JS)



