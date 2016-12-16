#Author : Pallavi Karan
#Date: 11/9/2016
#Purpose: Assignment no 6

rm(list = ls()) #Clear the lists
#Ans 1) a)

library(XLConnect)
dataframe <- readWorksheet(loadWorkbook("C://Users//pallavikaran//Desktop//RealEstateData.xlsx"),sheet=1)

sampled.data<-dataframe[sample(nrow(dataframe), 300), ]
sampled.data<-sampled.data[ , -which(names(sampled.data) %in% c("ID"))]
attach(sampled.data)
#Step 1: EDA
### From domain knowledge, ID doesn't play an important role. Hence drop it.
### Categorical variables are: AC, Pool, Year, Qulaity, style, hwy
########################################################################
#full model 
lm.fit <- lm(Price ~ SqFt+ Bed +Bath +factor(AC)+ GSize +factor(Pool)+ factor(Year)+factor(Quality)+factor(Style)+LotSize+factor(Hwy), data=sampled.data)
summary(lm.fit)
plot(lm.fit)
anova(lm.fit)
#par(mfrow=c(1,2))
plot(lm.fit$fitted.values, lm.fit$residuals, xlab="Fit",ylab="Residual", pch=20)
abline(h=0)
#######################################################################################
updated.factor.year<-factor(Year,exclude=c(1921,1938,1939,1947,1950,1955,1957,1958,1959,1960,1963,1966,1973,1977,1983))
lm.fit.1 <- lm(Price ~ SqFt+Bath + GSize +factor(Pool)+updated.factor.year +factor(Quality)+factor(Style)+LotSize+factor(Hwy), data=sampled.data)
summary(lm.fit.1)
plot(lm.fit.1)
anova(lm.fit.1)
plot(lm.fit.1$fitted.values, lm.fit.1$residuals, xlab="Fit",ylab="Residual", pch=20)
abline(h=0)
#######################################################################################
updated.factor.year1<-factor(Year,exclude=c(1921,1938,1939,1947,1950,1955,1957,1958,1959,1960,1963,1966,1973,1977,1983,1969,1980,1981,1992,1994,1995,1998))
updated.factor.style<-factor(Style,exclude=c(4))
lm.fit.2 <- lm(Price ~ SqFt+Bath + GSize +updated.factor.year1 +factor(Quality)+updated.factor.style+LotSize+factor(Hwy), data=sampled.data)
summary(lm.fit.2)
anova(lm.fit.2)
plot(lm.fit.2)
plot(lm.fit.2$fitted.values, lm.fit.2$residuals, xlab="Fit",ylab="Residual", pch=20)
abline(h=0)
####################################################################################
updated.factor.year3<-factor(Year,exclude=c(1921,1938,1939,1947,1950,1955,1957,1958,1959,1960,1963,1966,1973,1977,1983,1969,1980,1981,1992,1994,1995,1998, 1990,1997,1941,1991))
lm.fit.4 <- lm(Price ~ SqFt+Bath + GSize +updated.factor.year3 +factor(Quality)+updated.factor.style+LotSize+factor(Hwy), data=sampled.data)
summary(lm.fit.4)
anova(lm.fit.4)
plot(lm.fit.4)
plot(lm.fit.4$fitted.values, lm.fit.4$residuals, xlab="Fit",ylab="Residual", pch=20)
abline(h=0)
####################################################################################
updated.factor.year4<-factor(Year,exclude=c(1921,1938,1939,1947,1950,1955,1957,1958,1959,1960,1963,1966,1973,1977,1983,1969,1980,1981,1992,1994,1995,1998, 1990,1997,1941,1991,1922,1989))
lm.fit.5 <- lm(Price ~ SqFt+Bath + GSize +updated.factor.year4 +factor(Quality)+updated.factor.style+LotSize+factor(Hwy), data=sampled.data)
summary(lm.fit.5)
anova(lm.fit.5)
plot(lm.fit.5)
plot(lm.fit.5$fitted.values, lm.fit.5$residuals, xlab="Fit",ylab="Residual", pch=20)
abline(h=0)
######################################################################################
lm.fit.7 <- lm(Price ~ SqFt+Bath +updated.factor.year4 +factor(Quality)+updated.factor.style1+LotSize+factor(Hwy), data=sampled.data)
summary(lm.fit.7)
anova(lm.fit.7)
plot(lm.fit.7)
plot(lm.fit.7$fitted.values, lm.fit.7$residuals, xlab="Fit",ylab="Residual", pch=20)
abline(h=0)
######################################################################################
updated.factor.year5<-factor(Year,exclude=c(1921,1938,1939,1947,1950,1955,1957,1958,1959,1960,1963,1966,1973,1977,1983,1969,1980,1981,1992,1994,1995,1998, 1990,1997,1941,1991,1922,1989,1982,1986))
lm.fit.8 <- lm(Price ~ SqFt +updated.factor.year5 +factor(Quality)+updated.factor.style1+LotSize+factor(Hwy), data=sampled.data)
summary(lm.fit.8)
anova(lm.fit.8)
plot(lm.fit.8)
plot(lm.fit.8$fitted.values, lm.fit.8$residuals, xlab="Fit",ylab="Residual", pch=20)
abline(h=0)
######################################################################################
updated.factor.year6<-factor(Year,exclude=c(1921,1938,1939,1947,1950,1955,1957,1958,1959,1960,1963,1966,1973,1977,1983,1969,1980,1981,1992,1994,1995,1998, 1990,1997,1941,1991,1922,1989,1982,1986,1908,1918,1940,1948))
lm.fit.9 <- lm(Price ~ SqFt +updated.factor.year6 +factor(Quality)+updated.factor.style1+LotSize+factor(Hwy), data=sampled.data)
summary(lm.fit.9)
anova(lm.fit.9)
plot(lm.fit.9)
plot(lm.fit.9$fitted.values, lm.fit.9$residuals, xlab="Fit",ylab="Residual", pch=20)
abline(h=0)
###############################################################################################
updated.factor.year7<-factor(Year,exclude=c(1921,1938,1939,1947,1950,1955,1957,1958,1959,1960,1963,1966,1973,1977,1983,1969,1980,1981,1992,1994,1995,1998, 1990,1997,1941,1991,1922,1989,1982,1986,1908,1918,1940,1948,1949,1951,1964,1965,1971))
lm.fit.10 <- lm(Price ~ SqFt +updated.factor.year7 +factor(Quality)+updated.factor.style1+LotSize+factor(Hwy), data=sampled.data)
summary(lm.fit.10)
anova(lm.fit.10)
plot(lm.fit.10)
plot(lm.fit.10$fitted.values, lm.fit.10$residuals, xlab="Fit",ylab="Residual", pch=20)
abline(h=0)
###############################################################################################
###############################################################################################
lm.fullfit.1 <- lm(Price ~ SqFt+ Bed +Bath +factor(AC)+ GSize + factor(Year)+factor(Quality)+factor(Style)+LotSize+factor(Hwy), data=sampled.data)
summary(lm.fullfit.1)
anova(lm.fullfit.1)
x.mat<-model.matrix(lm.fullfit.1)[,-1]
y<-sampled.data$Price
checks<-leaps(x.mat,y,method="Cp")
plot(lm.fit.11)
plot(lm.fit.11$fitted.values, lm.fit.11$residuals, xlab="Fit",ylab="Residual", pch=20)
abline(h=0)
###############################################################################################
###############################################################################################
lm.fullfit.2 <- lm(Price ~ SqFt+ Bed +Bath + GSize + factor(Year)+factor(Quality)+factor(Style)+LotSize+factor(Hwy), data=sampled.data)
summary(lm.fullfit.2)
anova(lm.fullfit.2)
library(leaps)
x.mat<-model.matrix(lm.fullfit.2)
stepAIC(lm.fullfit.1, scope=list(lower=lm.fullfit.2, upper=lm.fullfit.1), direction = "backward", trace=F)
stepAIC(lm.fullfit.1, scope=list(lower=lm.fullfit.2, upper=lm.fullfit.1), direction = "both", trace=F)
subset<-regsubsets(Price ~ SqFt+ Bed +Bath + GSize + factor(Year)+factor(Quality)+factor(Style)+LotSize+factor(Hwy), data=sampled.data, really.big=T)
summary(subset)
summary(subset)$bic

###############################################################################################
###############################################################################################
lm.fullfit.3 <- lm(Price ~ SqFt+ Bed +Bath + GSize +factor(Quality)+factor(Style)+LotSize+factor(Hwy), data=sampled.data)
summary(lm.fullfit.3)
anova(lm.fullfit.3)
###############################################################################################
###############################################################################################
lm.fullfit.4 <- lm(Price ~ SqFt+ +Bath + GSize +factor(Year)+factor(Quality)+factor(Style)+LotSize+factor(Hwy), data=sampled.data)
summary(lm.fullfit.4)
anova(lm.fullfit.4)
###############################################################################################
###############################################################################################
lm.fullfit.5 <- lm(Price ~ SqFt+ +Bath + GSize +factor(Quality)+factor(Style)+LotSize+factor(Hwy), data=sampled.data)
summary(lm.fullfit.5)
anova(lm.fullfit.5)
###############################################################################################
###############################################################################################
lm.fullfit.6 <- lm(Price ~ SqFt +Bath+factor(Year) +factor(Quality)+factor(Style)+LotSize+factor(Hwy), data=sampled.data)
summary(lm.fullfit.6)
anova(lm.fullfit.6)
###############################################################################################
###############################################################################################
lm.fullfit.7 <- lm(Price ~ SqFt+ +factor(Year) +factor(Quality)+factor(Style)+LotSize+factor(Hwy), data=sampled.data)
summary(lm.fullfit.7)
anova(lm.fullfit.7)
###############################################################################################
###############################################################################################
lm.fullfit.8 <- lm(Price ~ SqFt+Bath  +factor(Quality)+factor(Style)+LotSize+factor(Hwy), data=sampled.data)
summary(lm.fullfit.8)
anova(lm.fullfit.8)
stepAIC()
