
str(HBAT)
colSums(is.na(HBAT))

Mat <- HBAT[,c(2, 4, 14:18)]
str(Mat)
detach(HBAT)
attach(Mat)

table(sex)
table(JP)
table(sex, JP)

x0 <- subset(Mat, sex==0)
x1 <- subset(Mat, sex==1)

x2 <- subset(Mat, JP==2)
x3 <- subset(Mat, JP==3)
x4 <- subset(Mat, JP==4)
x5 <- subset(Mat, JP==5)

x02 <- subset(Mat, sex==0&JP==2)
x03 <- subset(Mat, sex==0&JP==3)
x04 <- subset(Mat, sex==0&JP==4)
x05 <- subset(Mat, sex==0&JP==5)

x12 <- subset(Mat, sex==1&JP==2)
x13 <- subset(Mat, sex==1&JP==3)
x14 <- subset(Mat, sex==1&JP==4)
x15 <- subset(Mat, sex==1&JP==5)

#JS1#
mean(x02$JS1); sd(x02$JS1)
mean(x03$JS1); sd(x03$JS1)
mean(x04$JS1); sd(x04$JS1)
mean(x05$JS1); sd(x05$JS1)

mean(x12$JS1); sd(x12$JS1)
mean(x13$JS1); sd(x13$JS1)
mean(x14$JS1); sd(x14$JS1)
mean(x15$JS1); sd(x15$JS1)

mean(x0$JS1); sd(x0$JS1)
mean(x1$JS1); sd(x1$JS1)
mean(x2$JS1); sd(x2$JS1)
mean(x3$JS1); sd(x3$JS1)
mean(x4$JS1); sd(x4$JS1)
mean(x5$JS1); sd(x5$JS1)

mean(Mat$JS1); sd(Mat$JS1)

#JS2#
mean(x02$JS2); sd(x02$JS2)
mean(x03$JS2); sd(x03$JS2)
mean(x04$JS2); sd(x04$JS2)
mean(x05$JS2); sd(x05$JS2)

mean(x12$JS2); sd(x12$JS2)
mean(x13$JS2); sd(x13$JS2)
mean(x14$JS2); sd(x14$JS2)
mean(x15$JS2); sd(x15$JS2)

mean(x0$JS2); sd(x0$JS2)
mean(x1$JS2); sd(x1$JS2)
mean(x2$JS2); sd(x2$JS2)
mean(x3$JS2); sd(x3$JS2)
mean(x4$JS2); sd(x4$JS2)
mean(x5$JS2); sd(x5$JS2)

mean(Mat$JS2); sd(Mat$JS2)

#JS3#
mean(x02$JS3); sd(x02$JS3)
mean(x03$JS3); sd(x03$JS3)
mean(x04$JS3); sd(x04$JS3)
mean(x05$JS3); sd(x05$JS3)

mean(x12$JS3); sd(x12$JS3)
mean(x13$JS3); sd(x13$JS3)
mean(x14$JS3); sd(x14$JS3)
mean(x15$JS3); sd(x15$JS3)

mean(x0$JS3); sd(x0$JS3)
mean(x1$JS3); sd(x1$JS3)
mean(x2$JS3); sd(x2$JS3)
mean(x3$JS3); sd(x3$JS3)
mean(x4$JS3); sd(x4$JS3)
mean(x5$JS3); sd(x5$JS3)

mean(Mat$JS3); sd(Mat$JS3)


#JS4#
mean(x02$JS4); sd(x02$JS4)
mean(x03$JS4); sd(x03$JS4)
mean(x04$JS4); sd(x04$JS4)
mean(x05$JS4); sd(x05$JS4)

mean(x12$JS4); sd(x12$JS4)
mean(x13$JS4); sd(x13$JS4)
mean(x14$JS4); sd(x14$JS4)
mean(x15$JS4); sd(x15$JS4)

mean(x0$JS4); sd(x0$JS4)
mean(x1$JS4); sd(x1$JS4)
mean(x2$JS4); sd(x2$JS4)
mean(x3$JS4); sd(x3$JS4)
mean(x4$JS4); sd(x4$JS4)
mean(x5$JS4); sd(x5$JS4)

mean(Mat$JS4); sd(Mat$JS4)


#JS5#
mean(x02$JS5); sd(x02$JS5)
mean(x03$JS5); sd(x03$JS5)
mean(x04$JS5); sd(x04$JS5)
mean(x05$JS5); sd(x05$JS5)

mean(x12$JS5); sd(x12$JS5)
mean(x13$JS5); sd(x13$JS5)
mean(x14$JS5); sd(x14$JS5)
mean(x15$JS5); sd(x15$JS5)

mean(x0$JS5); sd(x0$JS5)
mean(x1$JS5); sd(x1$JS5)
mean(x2$JS5); sd(x2$JS5)
mean(x3$JS5); sd(x3$JS5)
mean(x4$JS5); sd(x4$JS5)
mean(x5$JS5); sd(x5$JS5)

mean(Mat$JS5); sd(Mat$JS5)
