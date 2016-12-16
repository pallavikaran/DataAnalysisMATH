#Author : Pallavi Karan
#Date: 10/10/2016
#Purpose: Assignmnet no 4 

rm(list = ls()) #Clear the lists
#Ans 4
library(sqldf)

mydata <- read.table("D://MATH-DA//bodyfat.txt", sep = "")
attach(mydata)
cor(mydata[,1:3])

lm.fit <- lm(V4 ~ V1)
summary(lm.fit)
plot(lm.fit)
abline(lm.fit)
anova(lm.fit)
confint(lm.fit)
new.data<-data.frame(V1=25)
predict.lm(lm.fit, new.data, interval ="confidence")

lm.fit1 <- lm(V4 ~ V1 +V2)
summary(lm.fit1)
anova(lm.fit1)
#sqldf("SELECT V2 FROM mydata WHERE V1='25.5'")
newdata <- mydata[ which(mydata$V1>24 & mydata$V1<26), ]
predict.lm(lm.fit1, newdata, interval ="confidence")
predict.lm(lm.fit1, newdata, interval ="prediction")
new.data1<-newdata$V2

#Ans 5

