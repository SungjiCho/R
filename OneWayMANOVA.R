
#일원배치 다변량 분석
str(HBAT)
n <- table(HBAT$sex)  #처리별 표본의 수
N <- sum(n)  # 전체 자료의 수
g <- length(n)  #처리 수
p <- dim(JS])[2]  #변수의 수

#처리별 평균벡터
JS_sex <- HBAT[,c(2, 14:18)]
m <- aggregate(.~sex, data=JS_sex, mean)[,-1]
m <- as.matrix(m)
m

#전체 평균벡터
tm <- colMeans(JS)
tm

B <- diag(0, p)  #초기화
for(i in 1:g){
  B <- B + n[i]*(m[i,]-tm)%*%(t(m[i,]-tm))
} #처리제곱합행렬 cov(m)*(g-1)*(N/g) [관측치동일]
B

E <- diag(0, p)
for(i in 1:g){
  E <- E + (n[i]-1)*
    cov(JS_sex[JS_sex$sex==(i-1),-1])
} #오차제곱합행렬
E

T <- B + E  #총제곱합행렬
T

df.B <- g-1  #처리 자유도
df.E <- N-g  #오차 자유도

#검정통계량 람다
Lambda <- det(E)/det(B+E)  
Lambda
#근사 검정통계량 람다*
Lambda.a <- (-(N-1-p-g)/2)*log(Lambda)
Lambda.a

alpha <- 0.05
df <- p*(g-1)
qchisq(alpha, df, lower.tail = FALSE)  #기각값


#Pillai 검정툥계량 ver.1
Pillai.Stat <- function(E, B){
  Inv.E <- solve(E)  #오차제곱합행렬의 역행렬
  #Inv(E)B의 고유값
  lambda <- eigen(Inv.E%*%B)$values
  s <- length(lambda)
  V <- 0
  for(i in 1:s){
    V <- V+lambda[i]/(1+lambda[i])
  } #Pillai 검정통계량
  
  return(V)
}

Pillai.Stat(E, B)
sum(diag(solve(E+B)%*%B))  #Pillai 검정통계량 ver.2


#Lawley-Hotelling 검정통계량
Inv.E <- solve(E)  #오차제곱합행렬의 역행렬
Inv.E
#Inv(E)B의 고유값
lambda <- eigen(Inv.E%*%B)$values
lambda
U <- sum(lambda) #Lawley-Hotelling 검정통계량
U


#Roy 검정통계량
theta <- lambda[1]/(1+lambda[1])  #Roy 검정통계량
theta



#공분산행렬 동일성 검정
n  #처리별 표본의 수
v <- n-1
v 
N  #전체 자료의 수
g  #처리수
p  #변수의 수

#그룹1 자료
g1.rs <- JS_sex[JS_sex$sex==0, -1]
#그룹2 자료
g2.rs <- JS_sex[JS_sex$sex==1, -1]
#그룹1 공분산행렬
S1 <- cov(g1.rs)
#그룹2 공분산행렬
S2 <- cov(g2.rs)
#합동공분산행렬
Spl <- (v[1]*S1+v[2]*S2)/sum(v)
Spl

S1;S2;Spl

detS1 <- det(S1)
detS2 <- det(S2)
detSpl <- det(Spl)
detS1;detS2;detSpl

M <- ((detS1/detSpl)^(v[1]/2))*
     ((detS2/detSpl)^(v[2]/2))  #M값
M

c1 <- (sum(1/v)-(1/sum(v)))*
      ((2*p^2+3*p-1)/(6*(p+1)*(g-1)))  #c1값
c1

u <- (-2*(1-c1)*log(M))  #통계량u
u

alpha <- 0.05  #유의수준
df <- (g-1)*p*(p+1)/2  #자유도
critical.value <- qchisq(alpha, df, lower.tail = FALSE)
critical.value  #임계값
pvalue <- pchisq(u, df, lower.tail = FALSE)  #p-값
pvalue







