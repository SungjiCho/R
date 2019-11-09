str(HBAT)
designMat <- HBAT[,c(2, 4, 14:18)]
str(designMat)
attach(designMat)

sex = factor(sex)  #factor 지정
JP = factor(JP)

#일변량 이원배치분산분석
summary(aov(JS1 ~ sex+JP+sex:JP, data=designMat))
summary(aov(JS2 ~ sex+JP+sex:JP, data=designMat))
summary(aov(JS3 ~ sex+JP+sex:JP, data=designMat))
summary(aov(JS4 ~ sex+JP+sex:JP, data=designMat))
summary(aov(JS5 ~ sex+JP+sex:JP, data=designMat))


# 상호작용 그림
par(mfrow=c(1,2))
interaction.plot(sex, JP, JS1, type="b", col=c(1:3),
                 leg.bty = "o", leg.bg = "beige", lwd=2, pch = c(18, 24, 22),
                 xlab = "Sex", ylab = "Job Performance", main = "Interaction Plot : JS1")
interaction.plot(JP.f, SEX.f, JS1, type="b", col=c(1:3),
                 leg.bty = "o", leg.bg = "beige", lwd=2, pch = c(18, 24, 22),
                 ylab = "Sex", xlab = "Job Performance", main = "Interaction Plot : JS1")

interaction.plot(SEX.f, JP.f, JS2, type="b", col=c(1:3),
                 leg.bty = "o", leg.bg = "beige", lwd=2, pch = c(18, 24, 22),
                 xlab = "Sex", ylab = "Job Performance", main = "Interaction Plot : JS2")
interaction.plot(SEX.f, JP.f, JS3, type="b", col=c(1:3),
                 leg.bty = "o", leg.bg = "beige", lwd=2, pch = c(18, 24, 22),
                 xlab = "Sex", ylab = "Job Performance", main = "Interaction Plot : JS3")
interaction.plot(SEX.f, JP.f, JS4, type="b", col=c(1:3),
                 leg.bty = "o", leg.bg = "beige", lwd=2, pch = c(18, 24, 22),
                 xlab = "Sex", ylab = "Job Performance", main = "Interaction Plot : JS4")
interaction.plot(SEX.f, JP.f, JS5, type="b", col=c(1:3),
                 leg.bty = "o", leg.bg = "beige", lwd=2, pch = c(18, 24, 22),
                 xlab = "Sex", ylab = "Job Performance", main = "Interaction Plot : JS5")


# 다중비교 실시
TukeyHSD(aov(JS2 ~ factor(SEX.f)+factor(JP.f)+factor(SEX.f)*factor(JP.f), data=designMat))

# 다변량 이원배치분산분석
JS = cbind(JS1, JS2, JS3, JS4, JS5)  #응답벡터
fit = manova(JS ~ SEX.f + JP.f + SEX.f:JP.f)
summary(fit, test="Wilks")  # Wilks' lambda
summary(fit, test="Pillai")  # Pillai's trace
summary(fit, test="Roy")  # Roy's grestest root
summary(fit, test="Hotelling")  # Hotelling-Lawley trace


#공분산행렬의 동질성 검정
install.packages("biotools")
install.packages("rpanel")
install.packages("tcltk")
install.packages("BWidget")
library(biotools)
.libPaths()

install.packages("psych")
install.packages("GPArotation")

library(psych)
library(GPArotation)
data(Thurstone)

fit1 <- fa(Thurstone, nfactors = 3, rotate = "oblimin", fm="ml")
print(fit1)

install.packages("EFAutilities")
library(EFAutilities)
Thurstone
res1 <- efa(x=NULL, covmat=Thurstone, dist="normal", factors=3, n.obs=213, fm="ml",
            rtype = 'oblique', rotation = 'CF-varimax', merror="YES", mnames=row.names(Thurstone))
print(res1)

res1$ModelF
res1$rotatedlow
res1$rotatedupper
res1$Residual


Target1 <- matrix(c(9, 0, 0,
                    9, 0, 0,
                    9, 0, 0,
                    0, 9, 0,
                    0, 9, 0,
                    0, 9, 0,
                    0, 0, 9,
                    0, 0, 9,
                    0, 0, 9), ncol=3, byrow=TURE)

MWeight1 <- matrix(0, ncol=3, nrow=9)
MWeight1[Target1==0] <- 1

