#Author : Pallavi Karan
#Date: 11/19/2016
#Purpose: Assignment no 6

rm(list = ls()) #Clear the lists
#Ans 1) 
library(XLConnect)
library(leaps)
library(MASS)
library('corrplot') #package corrplot
dataframe <- readWorksheet(loadWorkbook("C://Users//pallavikaran//Desktop//RealEstateData.xlsx"),sheet=1)

press.p<-function(model){
x.mat=model.matrix(model)
lev=hat(x.mat)
d=model$res/(1-lev)
return (sum(d^2))
}

########################################################################
#Step 1: EDA
### From domain knowledge, ID doesn't play an important role. Hence drop it.
### Categorical variables are: AC, Pool, Year, Qulaity, style, hwy
########################################################################
set.seed(155)
full.data<-dataframe[ , -which(names(dataframe) %in% c("ID"))]
sample.data <- sample(seq_len(nrow(full.data)), size = 300)
train <- full.data[sample.data, ]
test <- full.data[-sample.data, ]
attach(train)

#BOXPLOTS
boxplot(sampled.data[ , -which(names(sampled.data) %in% c("Price"))])

#SCATTERPLOT
par(mfrow=c(2,2))
plot(SqFt)
plot(Bed)
plot(Bath)
plot(AC)
par(mfrow=c(2,2))
plot(GSize)
plot(Pool)
plot(Year)
plot(Quality)
par(mfrow=c(2,2))
plot(Style)
plot(LotSize)
plot(Hwy)
plot(train)

#Co-Relation Matrix
cor(sampled.data)
corrplot(cor(sampled.data), method = "circle") #plot matrix

#full model 
lm.fit.full <- lm(Price ~ SqFt+ Bed +Bath +factor(AC)+ GSize +factor(Pool)+ Year+factor(Quality)+factor(Style)+LotSize+factor(Hwy), data=train)
summary(lm.fit.full)
anova(lm.fit.full)
plot(lm.fit.full)
#BoxCox to get rid of variability
par(mfrow=c(1,2))
boxcox(lm.fit.full)
boxcox(lm.fit.full, lambda = seq(-0.4,0, by=0.1))

#Model Transformation: 1 sqrt
lm.fit.mod1 <- lm(sqrt(Price) ~ SqFt+ Bed +Bath +factor(AC)+ GSize +factor(Pool)+ Year+factor(Quality)+factor(Style)+LotSize+factor(Hwy), data=train)
summary(lm.fit.mod1)
anova(lm.fit.mod1)
plot(lm.fit.mod1)

#Model Transformation: 2 log
lm.fit.mod2 <- lm(log(Price) ~ SqFt+ Bed +Bath +factor(AC)+ GSize +factor(Pool)+ Year+factor(Quality)+factor(Style)+LotSize+factor(Hwy), data=train)
summary(lm.fit.mod2)
anova(lm.fit.mod2)
plot(lm.fit.mod2)

#Model Transformation: 3 box cox (-0.22)
lm.fit.mod3 <- lm((Price^(-0.22)) ~ SqFt+ Bed +Bath +factor(AC)+ GSize +factor(Pool)+ Year+factor(Quality)+factor(Style)+LotSize+factor(Hwy), data=train)
summary(lm.fit.mod3)
anova(lm.fit.mod3)
plot(lm.fit.mod3)

#adj2 top 10
x.mat<-model.matrix(lm.fit.mod2)[,-1]
y<-log(Price)
checks.adjr2<-leaps(x.mat,y,method="adjr2")
maxadjr(checks.adjr2, best=10)
#Mallow's CP
checks.cp<-leaps(x.mat,y,method="Cp")
library(faraway)
Cpplot(checks.cp)

#STEP AIC
lm.fit.mod.null <- lm(log(Price) ~ 1, data=train)

stepAIC(lm.fit.mod.null, scope=list(lower=lm.fit.mod.null, upper=lm.fit.mod2), direction="forward", trace=F)
stepAIC(lm.fit.mod2, scope=list(lower=lm.fit.mod.null, upper=lm.fit.mod2), direction="backward", trace=F)
stepAIC(lm.fit.mod2, scope=list(lower=lm.fit.mod.null, upper=lm.fit.mod2), direction="both", trace=F)

all.subset<-regsubsets(log(Price) ~ SqFt+ Bed +Bath +factor(AC)+ GSize +factor(Pool)+ Year+factor(Quality)+factor(Style)+LotSize+factor(Hwy), data=train)
summary(all.subset)
summary(all.subset)$bic

lm.fit.mod4 <- lm(log(Price) ~SqFt+Bath+ Year+ I(Quality==2)+I(Quality==3)+LotSize, data=train)
summary(lm.fit.mod4)
anova(lm.fit.mod4)
plot(lm.fit.mod4)

lm.fit.mod5 <- lm(log(Price) ~SqFt+Bath+GSize+ Year+ I(Quality==2)+I(Quality==3)+LotSize, data=train)
summary(lm.fit.mod5)
anova(lm.fit.mod5)
plot(lm.fit.mod5)

lm.fit.mod6 <- lm(log(Price) ~SqFt+ Bath+ GSize+ Year+ I(Quality==2)+I(Quality==3)+I(Style==11)+LotSize, data=train)
summary(lm.fit.mod6)
anova(lm.fit.mod6)
plot(lm.fit.mod6)

press.stats<-c(press.p(lm.fit.mod4),press.p(lm.fit.mod5),press.p(lm.fit.mod6))
mod.ss<-c(tail(anova(lm.fit.mod4)$"Sum Sq",1),tail(anova(lm.fit.mod5)$"Sum Sq",1),tail(anova(lm.fit.mod6)$"Sum Sq",1))

######### 1 (b)

#Prediction for test dataset using the three models fitted on the training dataset
pred1<-predict.lm(lm.fit.mod4, test)
pred2<-predict.lm(lm.fit.mod5, test)
pred3<-predict.lm(lm.fit.mod6, test)
#Comparing Mean square prediction error and MSE
ypred<-(test$Price)
yp<-log(ypred)
sum((yp-pred1)^2)/length(yp)
sum((yp-pred2)^2)/length(yp)
sum((yp-pred3)^2)/length(yp)
c(tail(anova(lm.fit.mod4)$"Mean Sq",1), tail(anova(lm.fit.mod5)$"Mean Sq",1), tail(anova(lm.fit.mod6)$"Mean Sq",1))

#Residual Plot for mod1 which is the selected best predictor subset model
par(mfrow=c(1,2))
plot(lm.fit.mod5$fit, lm.fit.mod5$residuals, main="Residual vs. Fitted Plot")
abline(h=0)
qqnorm(lm.fit.mod5$residuals)
qqline(lm.fit.mod5$residuals)


#################### 1 (c)
std.res <- rstudent(lm.fit.mod5)
ti.max <- std.res[abs(std.res)==max(abs(std.res))]

p.val <- 2*(1-pt(abs(ti.max),df=300-5-1))
p.val
p.val<0.05/300
#104 is the outlier
x.mat <- model.matrix(lm.fit.mod5)
lev <- hat(x.mat)
plot(lev,ylab = "Leverages",main="Index plot of leverages",pch=20)
abline(h=(2*3)/300)
identify(1:300,lev,1:300)

#cook's distance
cook<-cooks.distance(lm.fit.mod5)
plot(cook, xlab="Cases", ylab="Cook's distance", pch=20)
segments(1:300, 0, 1:300, cook)
identify(1:300, cook, 1:300)

cor(dataframe[c(1,2,3,4,5,7,9,12)])
corrplot((cor(dataframe[c(1,2,3,4,5,7,9,12)])), method = "circle")

