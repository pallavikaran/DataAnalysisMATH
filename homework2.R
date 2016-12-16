#Author: Pallavi Karan
#Date: 9/6/2016
#Data Analysis-8050 HW-2

#Loading the dataset
rm(list = ls()) #clearing the workspace
getwd()
setwd("D:\\MATH-DA\\HW 2\\")
library(XLConnect)
my.data <- readWorksheet(loadWorkbook("SaltConc.xlsx"),sheet=1)
# 1 (a)
mean(my.data$Salt)
mean(my.data$Area)
median(my.data$Salt)
median(my.data$Area)
sd(my.data$Salt)
sd(my.data$Area)
max(my.data$Salt)
min(my.data$Salt)
max(my.data$Area)
min(my.data$Area)
summary(my.data)

# 1(B)
boxplot(my.data$Salt,my.data$Area, main="Boxplot for SaltConc" ,ylab="Distribution", names=c("Salt Conc","Roadway Area"))

# 1(C)
hist(my.data$Area, main="Area Distribution", xlab="Area")

# 1 (D)
plot(my.data$Salt ~ my.data$Area , main="Scatterplot", xlab="Roadway Area",ylab="Salt Concentration")

# 1 (E)
myfunction <- function(args.dataset){ 
  my.subset <- args.dataset[2:2,]
  my.sum<-sum(my.subset)
  return(my.sum)
}
my.sum.from.function <- myfunction(my.data)
my.sum.from.function

# 1 (F)
t.test( x = my.data$Salt, alternative = "two.sided",
        mu = 20, conf.level = 0.90)

#########################Assignmet no 2 ##############################
# 3
mod3 <- lm(my.data) #linear regression fit 
summary(mod3)
plot(mod3)
plot(my.data$Salt ~ my.data$Area , main="Scatterplot", xlab="Roadway Area",ylab="Salt Concentration")
abline(mod3)#best fit line
confint(mod3)#conf interval
new.data<-data.frame(Area=1)#x=1
predict.lm(mod3 ,new.data,interval ="confidence")#Estimate cofidence interval
predict.lm(mod3 ,new.data,interval ="prediction")#Predict interval
