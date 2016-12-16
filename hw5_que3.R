#Author : Pallavi Karan
#Date: 11/9/2016
#Purpose: Assignment no 5 
#ques: 3 & 4
rm(list = ls()) #Clear the lists
#Ans 3 a)

mydata <- read.table("C://Users//pallavikaran//Desktop//Ch6GroceryData.txt", sep = "", header = TRUE)
mydata["x.i.1"] <- NA
mydata["x2.i.1"] <- NA
mydata["x.i.2"] <- NA
mydata["x.i.1.x.i.2"] <- NA
mydata["x2.i.1.x.i.2"] <- NA

mydata$lab.cen<-(mydata$labor-mean(mydata$labor))/sd(mydata$labor)
mydata$case.cen<-(mydata$cases-mean(mydata$cases))/sd(mydata$cases)

mydata$x.i.1<-mydata$case.cen
mydata$x2.i.1<-(mydata$lab.cen*mydata$lab.cen)
mydata$x.i.2<-mydata$holiday
mydata$x.i.1.x.i.2<-(mydata$x.i.1*mydata$x.i.2)
mydata$x2.i.1.x.i.2<-(mydata$x2.i.1*mydata$x.i.2)

lm.fit <- lm(mydata$lab.cen ~ mydata$x.i.1 + mydata$x2.i.1 + factor(mydata$x.i.2) + (mydata$x.i.1*factor(mydata$x.i.2)) +(mydata$x2.i.1*factor(mydata$x.i.2)))
summary(lm.fit)

# 3 c)
lm.fit.red <- lm(mydata$lab.cen ~ mydata$x.i.1 + factor(mydata$x.i.2))
summary(lm.fit.red)
require(ggplot2)
plotter<-ggplot(mydata, aes(x=mydata$cases, y=mydata$labor))+geom_point(aes(color=mydata$holiday))
plot(plotter)
plotter1<-ggplot(mydata, aes(x=mydata$cases, y=mydata$labor))+geom_point(aes(color=factor(mydata$holiday),shape=factor(mydata$holiday)))+geom_abline(intercept=lm.fit.red$coefficients[1]+lm.fit.red$coefficients[3],slope=lm.fit.red$coefficients[2],color="blue",size=1)
plotter2<-plotter1+geom_abline(intercept=lm.fit.red$coefficients[1],slope=lm.fit.red$coefficients[2],color="red",size=1)
print(plotter2)

# 4 a)
lm.fit.firtorder <- lm(mydata$labor ~ mydata$cases + mydata$costs + factor(mydata$holiday))
summary(lm.fit.firtorder)
(ex.std.res<-rstudent(lm.fit.firtorder))
(ti.max<-ex.std.res[abs(ex.std.res)==max(abs(ex.std.res))])
p.val<-2*(1-pt(abs(ti.max),df=52-3-1))
p.val<.05/52
p.val

# 4 b)
x.mat<-model.matrix(lm.fit.firtorder)
lev<-hat(x.mat)
#plot(lev, ylab="Leverages", main="Index plot of Levrages")
plot(lev, ylab="Leverages", main="Index plot of Levrages", pch=20)
abline(h=2*3/52)
identify(1:52, lev, 1:52)

#4 c)
new.data<-data.frame(labor = 300000, costs=7.2, holiday=0)
plot(mydata$cases, mydata$costs, xlab="Cases", ylab="Costs")
points(new.data, pch=20)

#4 d)
plot(costs~cases, data=subset(data, holiday==0))
new_pt <- matrix(c(1, 300000, 7.2, 0), nrow =1, ncol=4)
lev_pt <- new_pt%*%solve(t(x.mat)%*%x.mat)%*%t(new_pt)
cook <- cooks.distance(lm.fit.firtorder)
plot(cook, xlab= "Case", ylab = "Cook's Distance", pch=20)
segments(1:52,0,1:52,cook)
identify(1:52,cook,1:52)
cook[c(16, 22, 43, 48, 10, 32, 38,40)]
