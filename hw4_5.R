#Author : Pallavi Karan
#Date: 10/10/2016
#Purpose: Assignmnet no 4 

rm(list = ls()) #Clear the lists

my_data <- read.table("D://MATH-DA//SteroidLevels.txt", sep = "", header = TRUE)
attach(my_data)
View(my_data)
mean.var<-mean(my_data$Age) 
#new.data.age<-sd(my_data$Age)
#merge(my_data,diff_data,diff_data_sq,by=c(my_data$Level,my_data$Age,diff_data,diff_data_sq) )
my_data["diff.mean"] <- NA
my_data$diff.mean<- diff_data<-(my_data$Age-mean.var)
my_data["diff.mean.sq"] <- NA
my_data$diff.mean.sq<- diff_data_sq<-diff_data^2
lm.fit1 <- lm(my_data$Level ~ my_data$diff.mean +my_data$diff.mean.sq)
summary(lm.fit1)
lm.fit1 <- lm(my_data$Level ~ my_data$diff.mean +I(my_data$diff.mean^2))
new.data<-data.frame(Age=15)
predict.lm(lm.fit1, new.data=list(Age=15), interval ="confidence", level=0.99, type="response")
predict.lm(lm.fit1, new.data=list(Age=15), interval ="prediction",level=0.99)


