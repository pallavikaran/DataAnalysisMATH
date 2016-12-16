#Author : Pallavi Karan
#Date: 11/9/2016
#Purpose: Assignment no 5 
#ques: 1
rm(list = ls()) #Clear the lists
#Ans 1 a) combining the datasets

mydata <- read.table("C://Users//pallavikaran//Desktop//GPAData.txt", sep = "", header = TRUE)
attach(mydata)
mydata.1 <- read.table("C://Users//pallavikaran//Desktop//GPAMajorData.txt", sep = "", header = TRUE)
attach(mydata.1)
# merge two data frames by ID
mydata["Major"] <- NA
mydata$Major<- mydata.1$Major
mydata
#scatterplot
plot(mydata)
#ggplot
require(ggplot2)
plotter<-ggplot(mydata, aes(x=mydata$ACT, y=mydata$GPA))+geom_point(aes(color=factor(mydata$Major),shape=factor(mydata$Major)))
plot(plotter)

#1b
lm.fit <- lm(mydata$GPA ~ mydata$ACT)
summary(lm.fit)
plot(mydata$GPA ~ mydata$ACT, main="Scatterplot")
abline(lm.fit)
par(mfrow=c(1,2))
plot(lm.fit$fitted.values, lm.fit$residuals, xlab="Fit",ylab="Residual", pch=20)
abline(h=0)
qqnorm(lm.fit$residuals, pch=20)
qqline(lm.fit$residuals)
major.0<-which(mydata$Major==0)
major.1<-which(mydata$Major==1)
par(mfrow=c(1,2))
plot(lm.fit$fit[major.0], lm.fit$residuals[major.0], xlab="Major=0", ylab="Residuals")
abline(h=0)
plot(lm.fit$fit[major.1], lm.fit$residuals[major.1], xlab="Major=1", ylab="Residuals")
abline(h=0)

#1 d)
lm.fit.1 <- lm(mydata$GPA ~ mydata$ACT + factor(mydata$Major))
summary(lm.fit.1)

#1 e)
mydata$x.i.1.x.i.2<-(mydata$ACT*mydata$Major)
lm.fit.2 <- lm(mydata$GPA ~ mydata$ACT + factor(mydata$Major)+mydata$x.i.1.x.i.2)
summary(lm.fit.2)
plotter1<-ggplot(mydata, aes(x=mydata$ACT, y=mydata$GPA))+geom_point(aes(color=factor(mydata$Major),shape=factor(mydata$Major)))+geom_abline(intercept=lm.fit.2$coefficients[1]+lm.fit.2$coefficients[3],slope=lm.fit.2$coefficients[2]+lm.fit.2$coefficients[4],color="blue",size=1)
plotter2<-plotter1+geom_abline(intercept=lm.fit.2$coefficients[1],slope=lm.fit.2$coefficients[2],color="red",size=1)
print(plotter2)

#1 g)
new.data.0<-subset(mydata,Major==0)
new.data.1<-subset(mydata,Major==1)
lm.fit.new.data.0 <- lm(new.data.0$GPA ~ new.data.0$ACT)
summary(lm.fit.new.data.0)
plot(mydata$ACT, mydata$GPA)
abline(lm.fit.new.data.0)

lm.fit.new.data.1 <- lm(new.data.1$GPA ~ new.data.1$ACT)
summary(lm.fit.new.data.1)
plot(mydata$ACT, mydata$GPA)
abline(lm.fit.new.data.1)

