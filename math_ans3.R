#Author : Pallavi Karan
#Date: 10/10/2016
#Purpose: Assignmnet no 4 

rm(list = ls()) #Clear the lists

my_data <- read.table("D://MATH-DA//PatientSatisfaction.txt", sep = "", header = TRUE)
attach(my_data)
View(my_data)
head(my_data)
#library(scatterplot3d)
#library(rgl)
pairs(my_data) #ANS" 3a Scatterplot
cor(my_data) #ANS: 3a corelation

######################################## 3b ######################################################

mod1= lm(my_data$Satis~my_data$Age+my_data$Severity+my_data$Anxiety) #Automatic model fitting
summary(mod1)
anova(mod1)

#Manual:
# Calculate the matrices manually
(X= cbind(rep(1,dim(my_data)[1]), my_data[,2:4]))  # cbind = "column-wise bind"
(Y= my_data[,1])
t(X)%*%as.matrix(X)
t(X)%*%Y
solve(t(X)%*%as.matrix(X))
(beta.hat= solve(t(X)%*%as.matrix(X))%*%t(X)%*%Y)

# Plot the estimated response plane
Y<-my_data[,1]
x1<-my_data[,2]
x2<-my_data[,3]
x3<-my_data[,4]

x1s= min(x1); x1m= max(x1)
x2s= min(x2); x2m= max(x2)
x3s= min(x3); x3m= max(x3)

# expand.grid - creates all combinations of the arguments
grid= expand.grid(seq(from=min(x1),to=max(x1),length.out=100),
                  seq(from=min(x2),to=max(x2),length.out=100),
                  seq(from=min(x3),to=max(x3),length.out=100))                                                                                                         
z= beta.hat[1]+ beta.hat[2]*grid[,1] + beta.hat[3]*grid[,2] + beta.hat[4]*grid[,3]

############################################# 3c ##############################################
# Manual ANOVA
n= length(Y)
p= 4
J= matrix(1,nrow= n, ncol= n)
H= as.matrix(X)%*%solve(t(X)%*%as.matrix(X))%*%t(X)
I= diag(rep(1,n))

(SST= t(Y)%*%(I-(1/n)*J)%*%Y)
(SSE= t(Y)%*%(I-H)%*%Y)
(SSR= t(Y)%*%(H-(1/n)*J)%*%Y)

# F test
alpha= .10
(F.stat= (SSR/(p-1))/(SSE/(n-p))) #test stat
qf(1-alpha,p-1,n-p)   # Critical value for the above test
pval = 2 ??? pt(F.stat, df=n???4)
pval
# Multiple and adjusted Rsquared
(R.sq= SSR/SST)
(R.sq.adj= 1-((SSE/(n-p))/(SST/(n-1)) ) )

# Fit the model "automatically"
mod1= lm(Y~x1+x2+x3)
summary(mod1)
anova(mod1)
# Notice that the ANOVA table finds the SS associated
# with each predictor separtely. Their sum is the 
# regression SS.


################################## 3d ########################################

# Find C.I.'s about beta1 and beta2
s2beta= as.vector(SSE/(n-p))*(solve(t(X)%*%as.matrix(X)))
se.beta1= sqrt(s2beta[2,2])
se.beta2= sqrt(s2beta[3,3])

alpha=0.1 
print( c(beta.hat[2]-qt(1-alpha/4,n-p)*se.beta1,beta.hat[2]+qt(1-alpha/4,n-p)*se.beta1 ))
print( c(beta.hat[3]-qt(1-alpha/4,n-p)*se.beta2,beta.hat[3]+qt(1-alpha/4,n-p)*se.beta2 ))

###################################3e C.I. about the mean response at x1=35, x2= 45, x3=2.2
xh= c(1, 35, 45, 2.2)
(Y.hat.h= xh%*%beta.hat)
(se.Y.hat.h= sqrt(xh%*%s2beta%*%xh))

alpha= .10
print(c(Y.hat.h-qt(1-alpha/2,n-p)*se.Y.hat.h, Y.hat.h+qt(1-alpha/2,n-p)*se.Y.hat.h))



##################################################ANOVA for X2
(X1= cbind(rep(1,dim(my_data)[1]), my_data[,3]))  # cbind = "column-wise bind"
(Y1= my_data[,1])
t(X1)%*%as.matrix(X1)
t(X1)%*%Y1
solve(t(X1)%*%as.matrix(X1))
(beta.hat= solve(t(X1)%*%as.matrix(X1))%*%t(X1)%*%Y1)
      
      # Plot the estimated response plane
      Y1<-my_data[,1]
      x22<-my_data[,3]
   
      x22s= min(x22); x22m= max(x22)
      
      # expand.grid - creates all combinations of the arguments
      grid2= expand.grid(seq(from=min(x22),to=max(x22),length.out=100))                                                                                                         
      z2= beta.hat[1]+ beta.hat[3]*grid[,2] 
mod2= lm(Y1~x1)
summary(mod2)
anova(mod2)
n1= length(Y1)
p1= 2
J1= matrix(1,nrow= n, ncol= n)
H1= as.matrix(X1)%*%solve(t(X1)%*%as.matrix(X1))%*%t(X1)
I1= diag(rep(1,n1))

(SST1= t(Y1)%*%(I1-(1/n1)*J1)%*%Y1)
(SSE1= t(Y1)%*%(I1-H1)%*%Y1)
(SSR1= t(Y1)%*%(H1-(1/n1)*J1)%*%Y1)

########################################## 3f & g ###############################################3
mod.for.anova= lm(my_data$Satis~my_data$Severity+my_data$Age+my_data$Anxiety)
summary(mod.for.anova)
fit.aov <- anova(mod.for.anova)
tab <- as.table(cbind(
  'SS' = c("SSR(x1, x2, x3)" = sum(fit.aov[1:3, 2]),
           "SSR(x2)"           = fit.aov[1, 2],
           "SSR(x1|x2)"        = fit.aov[2, 2],
           "SSR(x3|x2, x1)"    = fit.aov[3, 2],
           "SSE"               = fit.aov[4, 2],
           "Total"             = sum(fit.aov[, 2])),
  
  'Df' = c(                    sum(fit.aov[1:3, 1]),
                               fit.aov[1, 1],
                               fit.aov[2, 1],
                               fit.aov[3, 1],
                               fit.aov[4, 1],
                               sum(fit.aov$Df)),
  
  'MS' = c(                    sum(fit.aov[1:3, 2]) / sum(fit.aov[1:3, 1]),
                               fit.aov[1, 3],
                               fit.aov[2, 3],
                               fit.aov[3, 3],
                               fit.aov[4, 3],
                               NA)
))
round(tab, 2)

################################################################################
# Manual ANOVA
n= length(Y)
p= 4

# F test
alpha= .025
(F.stat= (fit.aov[3, 2]/1)/(fit.aov[4, 2]/(n-p))) #test stat
critical.val=(-13.702/7.0997)*(-13.702/7.0997)
qf(1-alpha,3,n-p) 

# Multiple and adjusted Rsquared
(R.sq= SSR/SST)
(R.sq.adj= 1-((SSE/(n-p))/(SST/(n-1))) )
pval = 2 * pt(F.stat, df=n-4)
pval
# Fit the model "automatically"
mod.x1x2= lm(Y~x1+x2)
summary(mod.x1x2)
anova(mod.x1x2)
# Notice that the ANOVA table finds the SS associated
# with each predictor separtely. Their sum is the 
# regression SS.
##################################### 3h #####################################################
fit.mod.h<-lm(my_data$Satis~my_data$Age)
anova(fit.mod.h,mod1)
######################################### 3i #################################################
fit.mod.i<-lm(my_data$Satis+my_data$Age~my_data$Anxiety)
anova(fit.mod.h,mod1)
###################################### 3j ####################################################
fit.mod.j<-lm(my_data$Satis~my_data$Age+my_data$Severity)
summary(fit.mod.j)
anova(fit.mod.j)
###################################### 3l ####################################################
fit.mod.l.x1<-lm(my_data$Satis~my_data$Age)
summary(fit.mod.l.x1)
anova(fit.mod.l.x1)
fit.mod.l.x2<-lm(my_data$Satis~my_data$Severity)
summary(fit.mod.l.x2)
anova(fit.mod.l.x2)
fit.mod.l.x1.x3<-lm(my_data$Satis~my_data$Anxiety+my_data$Age)
summary(fit.mod.l.x1.x3)
anova(fit.mod.l.x1.x3)
fit.mod.l.x2.x3<-lm(my_data$Satis~my_data$Anxiety+my_data$Severity)
summary(fit.mod.l.x2.x3)
anova(fit.mod.l.x2.x3)
################################ 3n ###########################################
n= length(Y)
y.val.satis<-(1/sqrt(n-1)) * ((my_data$Satis -mean(my_data$Satis))/sd(my_data$Satis))
x.val.age<-(1/sqrt(n-1)) * ((my_data$Age -mean(my_data$Age))/sd(my_data$Age))
x.val.sev<-(1/sqrt(n-1)) * ((my_data$Severity -mean(my_data$Severity))/sd(my_data$Severity))
x.val.anxi<-(1/sqrt(n-1)) * ((my_data$Anxiety -mean(my_data$Anxiety))/sd(my_data$Anxiety))
mod.new<-lm(y.val.satis~x.val.age+x.val.sev+x.val.anxi)
summary(mod.new)
################################ 3p ##########################################
beta1.age<-(sd(my_data$Satis)/sd(my_data$Age))*(-5.907e-01)
beta1.age
beta1.Severity<-(sd(my_data$Satis)/sd(my_data$Severity))*(-1.106e-01)
beta1.Severity
beta1.Anxiety<-(sd(my_data$Satis)/sd(my_data$Anxiety))*(-2.339e-01)
beta1.Anxiety
