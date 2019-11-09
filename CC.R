
#정준상관분석
install.packages("CCA")
library(CCA)

str(HBAT)
ccMat <- HBAT[,19:26]
str(ccMat)
summary(ccMat)

SIY <- ccMat[,5:8]
OCX <- ccMat[,1:4]



print(matcor(OCX, SIY), digits=3)  #상관계수행렬(CCA패키지)
#독립변수군의 상관계수행렬, 종속변수군의 상관계수행렬, 전체상관계수행렬

cc1 <- cc(OCX, SIY)  #정준상관분석
cc1[1]  #정준상관계수
 #제1, 제2, 제3, 제4 변수군의 관계가 양의 관계
cc1[3:4]  #정준변수계수  u1 u2 v1 v2

cc2 <- comput(OCX, SIY, cc1)  #정준상관분석 추가계산
cc2[3:6]  #변수와 정준변수 간의 상관계수

cc3 <- comput(SIY, OCX, cc1)
cc3[3:6]


# 표준화 정준변수
cov.OCX <- cov(OCX)  #OC의 공분산행렬
sd.OCX <- sqrt(diag(cov.OCX))  #OC1, OC2, OC3, OC4의 표준편차
S.X <- diag(sd.OCX)  #OC1, OC2, OC3, OC4의 표준편차 대각행렬
S.X

S.X%*%cc1$xcoef  #OCX의 표준화 정준변수계수 = 원변수들의 표준편차 * 원변수의 정준변수계수

cov.SIY <- cov(SIY) #SI의 공분산행렬
sd.SIY <- sqrt(diag(cov.SIY))  #SI1, SI2, SI3, SI4의 표준편차
S.Y <- diag(sd.SIY)  #SI1, SI2, SI3, SI4의 표준편차 대각행렬
S.Y
S.Y%*%cc1$ycoef  #SIY의 표준화 정준변수계수


par(mfrow=c(2, 2))
U1 <- cc1$scores$xscores[,1]  #첫 번째 정준변수(U1, V1)
V1 <- cc1$scores$yscores[,1]

plot(U1, V1, pch=18, main="First Canonical Plot")

U2 <- cc1$scores$xscores[,2]  #두 번째 정준변수(U2, V2)
V2 <- cc1$scores$yscores[,2]

plot(U2, V2, pch=18, main="Second Canonical Plot")

U3 <- cc1$scores$xscores[,3]  #세 번째 정준변수(U3, V3)
V3 <- cc1$scores$yscores[,3]

plot(U3, V3, pch=18, main="Third Canonical Plot")

U4 <- cc1$scores$xscores[,4]  #네 번째 정준변수(U4, V4)
V4 <- cc1$scores$yscores[,4]

plot(U4, V4, pch=18, main="Fourth Canonical Plot")

par(mfrow=c(1, 1))
plt.cc(cc1, type="v", var.label = TRUE)
plt.cc(cc1, type="i", var.label = TRUE)

mtc <- matcor(OCX, SIY)
print(mtc, digits=3)

img.matcor(mtc, type=1)  #상관계수행렬 그래프
img.matcor(mtc, type = 2)  #상관계수행렬 그래프(변수군별, 변수군간)

# 상관성에 대한 검정
alpha <- 0.05  #유의수준
n <- dim(ccMat)[1]  #표본수
p <- dim(OCX)[2]   #확률변수군 OC의 변수 수  
q <- dim(SIY)[2]   #확률변수군 SI의 변수 수
lambda1 <- prod(1-cc1[[1]]^2)  #람다1 검정통계량
chi2 <- (-(n-(p+q+3)/2))*log(lambda1)  #카이스퀘어 검정통계량
lambda1; chi2

1-pchisq(chi2, 16) #p-value

qchisq(1-alpha, p*q)  #임계값



alpha <- 0.05  #유의수준
n <- dim(ccMat)[1]  #표본수
p <- dim(OCX)[2]   #확률변수군 OC의 변수 수  
q <- dim(SIY)[2]   #확률변수군 SI의 변수 수

#정준상관계수의 유의검정
det(cor(ccMat))/det(cor(OCX))*det(cor(SIY))
(-396-0.5*(8+3))*log(0.007142162)
1-pchisq(1984.109, 16)


#정준상관계수의 개수에 관한 검정
cca <- cancor(OCX, SIY)
-(n-0.5*(p+3))*sum(log(1-(cca$cor[1:4])^2))  #검정통계량값
1-pchisq(131.574, 16)  #p-value
#귀무가설 기각, 정준상관변수 유의미하다.

-(n-0.5*(p+3))*sum(log(1-(cca$cor[2:4])^2))  #검정통계량값
1-pchisq(5.349644, 16)  #p-value



#다변량 정규성 평가
n <- dim(SIY)[1]  #표본수
p <- dim(SIY)[2]  #변수의 수

m <- colMeans(SIY) #평균벡터
s <- cov(SIY) #공분산행렬
solve(s)  #공분산행렬의 역행렬
d2 <- mahalanobis(SIY, m, s)  #마할라노비스 거리
d2 <- sort(d2)
i <- 1:n  #순서
edf <- (i-0.5)/n  #경험적 분포
q <- qchisq(edf, p)  #x2(p) 분위수

#마할라노비스 거리 순서통계량과 카이제곱분포
cbind(i, d2, edf, q)

plot(q, d2, pch=19,
     main="Chisquare plot for Staying Intentions",
     xlab = "x2 quantile",
     ylab = "ordered Mahalanobis d2") #카이제곱그림
abline(0, 1)

#다변량 정규성 평가
n <- dim(OCX)[1]  #표본수
p <- dim(OCX)[2]  #변수의 수

m <- colMeans(OCX) #평균벡터
s <- cov(OCX) #공분산행렬
solve(s)  #공분산행렬의 역행렬
d2 <- mahalanobis(OCX, m, s)  #마할라노비스 거리
d2 <- sort(d2)
i <- 1:n  #순서
edf <- (i-0.5)/n  #경험적 분포
q <- qchisq(edf, p)  #x2(p) 분위수

#마할라노비스 거리 순서통계량과 카이제곱분포
cbind(i, d2, edf, q)

plot(q, d2, pch=19,
     main="Chisquare plot for Organizational Commitment",
     xlab = "x2 quantile",
     ylab = "ordered Mahalanobis d2") #카이제곱그림
abline(0, 1)

install.packages("MVN")
library(MVN)
mvn(SIY)
mvn(OCX)


boxplot(SIY)
boxplot(OCX)
