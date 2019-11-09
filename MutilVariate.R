
# 2x3 행렬 #
A <- matrix(c(3, -1, 2, 1, 5, 4), ncol = 3, byrow = TRUE)
A

tA <- t(A)  #전치행렬
tA


L <- matrix(c(0.7, 0.2, 0.9, 0, 0.5, 0, 0, 0.5, 0.1, 0.7), ncol=2, byrow=TRUE)
L

U <- matrix(c(0.47, 0, 0, 0, 0, 0, 0.19, 0, 0, 0, 0, 0, 0.75, 0, 0, 0, 0, 0, 0.75, 0, 0, 0, 0, 0, 0.5), ncol=5, byrow=TRUE)
U

L%*%t(L) + U

# 벡터 연산 #
a <- c(1, -2, 3)  #벡터 a
b <- c(2, 2, 4)  #벡터 b

a + b

t(a)%*%b  #a'b

3*a

La <- sqrt(t(a)%*%a)  #벡터 a의 길이
La

Lb <- sqrt(t(b)%*%b)  #벡터 b의 길이
Lb

cos.theta <- (t(a)%*%b)/(La*Lb)  #cos theta
cos.theta

(acos(cos.theta)/pi)*180  #사이각 theta

# 직교행렬 예시#
a <- 1/sqrt(2)
A <- matrix(c(a, -a, a, a), ncol=2)
A

t(A)%*%A


# 직교행렬 예시: 회전행렬 #
theta <- 60/180*pi
A <- matrix(c(cos(theta), sin(theta),
              -sin(theta), cos(theta)), ncol=2)
A

t(A)%*%A


install.packages("psych")
library(psych)

data <- data(Thurstone)

thur.om <- omega(Thurstone,title="9 variables from Thurstone") #compare with
thur.bf   <- fa(Thurstone),3,rotate="biquartimin")

a <- 0.9*0.9 + 0.12*0.12
a
1-a



# 주성분 분석 #
S <- matrix(c(7, 1, 1, 7), ncol=2)
S

e.value <- eigen(S)$values
e.vector <- eigen(S)$vectors
e.value; e.vector

e.value[1] / sum(e.value)
e.value[2] / sum(e.value)



# 공분산행렬과 상관행렬을 이용한 주성분분석 비교 #
S <- matrix(c(1, 6, 6, 100), ncol=2)
S

Se.value <- eigen(S)$values
Se.vector <- eigen(S)$vectors

Se.value; Se.vector

Se.value[1] / sum(Se.value)


R <- matrix(c(1, 0.6, 0.6, 1), ncol=2)
R

Re.value <- eigen(R)$values
Re.vector <- eigen(R)$vectors

Re.value; Re.vector

Re.value[1] / sum(Re.value)


library(psych)
library(GPArotation)
data("Thurstone")

fit1 <- fa(Thurstone, nfactors=2, fm="ml")
print(fit1)

fit2 <- fa(Thurstone, nfactors=2, rotate="none", fm="ml")
print(fit2)

fit3 <- fa(Thurstone, nfactors=2, rotate="none", fm="ols")
print(fit3)

fit4 <- fa(Thurstone, nfactors=2, rotate="none", fm="gls")
print(fit4)

fa.parallel(Thurstone, n.obs=213)


library(EFAutilities)

p = 9; m = 3
MT <- matrix(0, p, m)
MT[c(1:3), 1] <- 9
MT[4:6, 2] <- 9
MT[7:9, 3] <- 9
MT

MW <- matrix(0, p, m)
MW[MT == 0] <- 1
MW

tgRes1 <- efa(x = NULL, covmat = Thurstone, dist = "normal", factors = m, n.obs = 213, fm = 'ml', useorder = T,
            rtype = 'oblique', rotation = 'target', MTarget = MT, MWeight = MW)
tgRes1

res2 <- efa(covmat = Thurstone, factors = 3, n.obs = 213, 
            rotation = 'target', fm = 'ml', useorder = T,
            MTarget = MT, MWeight = MW)
tgRes1$ModelF
tgRes1$rotatedse






