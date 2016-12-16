#
#
# 8050_Ch6_example.R
#
# This is some R code to implement the example in Section 6.5
# of Kutner, et al. (2005). 
#
# 25 Feb 2014

rm(list= ls(all= T))


# Create the data

# Sales - column3
# x1 - column1
# x2 - column2
dat<-matrix(c(68.5, 16.7, 174.4, 45.2, 16.8, 164.4, 91.3, 18.2, 244.2, 47.8, 16.3, 154.6,
               46.9, 17.3, 181.6, 66.1, 18.2, 207.5, 49.5, 15.9, 152.8, 52.0, 17.2, 163.2,
               48.9, 16.6, 145.4, 38.4, 16.0, 137.2, 87.9, 18.3, 241.9, 72.8, 17.1, 191.1,
               88.4, 17.4, 232.0, 42.9, 15.8, 145.3, 52.5, 17.8, 161.1, 85.7, 18.4, 209.7,
               41.3, 16.5, 146.4, 51.7, 16.3, 144.0, 89.6, 18.1, 232.6, 82.7, 19.1, 224.1,
               52.3, 16.0, 166.5),nrow=21,ncol=3, byrow=TRUE)

colnames(dat)= c("X1", "X2", "Y")  # names the columns to something useful

head(dat)  # Looks at the first few rows of the data

# Create a 3d scatter plot to explore the data
library(scatterplot3d)
library(rgl)

pairs(dat)
scatterplot3d(x=dat[,1], y=dat[,2], z=dat[,3], xlab="X1", ylab="x2", zlab="Y" )

# This creates a rotatable 3d scatter plot
plot3d(x=dat[,1],y=dat[,2],z=dat[,3], xlab="X1", ylab="x2", zlab="Y", col="red") 

# Calculate the matrices manually
(X= cbind(rep(1,dim(dat)[1]), dat[,1:2]))  # cbind = "column-wise bind"
(Y= dat[,3])
t(X)%*%X
t(X)%*%Y
solve(t(X)%*%X)
(beta.hat= solve(t(X)%*%X)%*%t(X)%*%Y)

# Plot the estimated response plane
Y<-dat[,3]
x1<-dat[,1]
x2<-dat[,2]

x1s= min(x1); x1m= max(x1)
x2s= max(x2); x2m= max(x2)

# expand.grid - creates all combinations of the arguments
grid= expand.grid(seq(from=min(x1),to=max(x1),length.out=100),
                  seq(from=min(x2),to=max(x2),length.out=100))                                                                                                         
z= beta.hat[1]+beta.hat[2]*grid[,1]+beta.hat[3]*grid[,2]
plot3d(x=dat[,1],y=dat[,2],z=dat[,3], xlab="X1", ylab="x2", zlab="Y", col="red")
plot3d(grid[,1],grid[,2],z,add=TRUE,type="l")


############################

# Manual ANOVA
n= length(Y)
p= 3
J= matrix(1,nrow= n, ncol= n)
H= X%*%solve(t(X)%*%X)%*%t(X)
I= diag(rep(1,n))

(SST= t(Y)%*%(I-(1/n)*J)%*%Y)
(SSE= t(Y)%*%(I-H)%*%Y)
(SSR= t(Y)%*%(H-(1/n)*J)%*%Y)

# F test
alpha= .05
(F.stat= (SSR/(p-1))/(SSE/(n-p)))
qf(1-alpha,p-1,n-p)   # Critical value for the above test

# Multiple and adjusted Rsquared
(R.sq= SSR/SST)
(R.sq.adj= 1-((SSE/(n-p))/(SST/(n-1)) ) )



#################################

# Fit the model "automatically"
mod1= lm(Y~x1+x2)
summary(mod1)
anova(mod1) # Notice that the ANOVA table finds the SS associated
# with each predictor separtely. Their sum is the 
# regression SS.
23371.8+643.5


##################################

# Find C.I.'s about beta1 and beta2
s2beta= as.vector(SSE/(n-p))*(solve(t(X)%*%X))
se.beta1= sqrt(s2beta[2,2])
se.beta2= sqrt(s2beta[3,3])

alpha=0.1 
print( c(beta.hat[2]-qt(1-alpha/4,n-p)*se.beta1,beta.hat[2]+qt(1-alpha/4,n-p)*se.beta1 ))
print( c(beta.hat[3]-qt(1-alpha/4,n-p)*se.beta2,beta.hat[3]+qt(1-alpha/4,n-p)*se.beta2 ))


###################################

# C.I. about the mean response at x1=65.4 and x2= 17.6
xh= c(1, 65.4, 17.6)
(Y.hat.h= xh%*%beta.hat)
(se.Y.hat.h= sqrt(xh%*%s2beta%*%xh))

alpha= .05
print(c(Y.hat.h-qt(1-alpha/2,n-p)*se.Y.hat.h, Y.hat.h+qt(1-alpha/2,n-p)*se.Y.hat.h))


#####################################

# Simultaneous prediction intervals
xh= c(1, 65.4, 17.6)
(Y.hat.h= xh%*%beta.hat)
(se2.pred= (SSE/(n-p)) + xh%*%s2beta%*%xh )

alpha= .05
print(c(Y.hat.h-qt(1-alpha/2,n-p)*sqrt(se2.pred), Y.hat.h+qt(1-alpha/2,n-p)*sqrt(se2.pred)))



xh2= c(1, 53.1, 17.7)
(Y.hat.h2= xh2%*%beta.hat)
(se2.pred= (SSE/(n-p)) + xh2%*%s2beta%*%xh2 )

alpha= .05
print(c(Y.hat.h2-qt(1-alpha/2,n-p)*sqrt(se2.pred), Y.hat.h2+qt(1-alpha/2,n-p)*sqrt(se2.pred)))



# Diagnostics (later in the course)
plot(mod1$fit, mod1$res, xlab= "Fit", ylab= "Res", pch= 20)
abline(h=0)

# Plot residuals against predictors and interaction
# term
x1x2<- x1*x2
par(mfrow= c(1,3))
plot(x1, mod1$res, xlab= "TargetPop", ylab= "Res", pch= 20)
abline(h=0)
plot(x2, mod1$res, xlab= "DisposInc", ylab= "Res", pch= 20)
abline(h=0)
plot(x1x2, mod1$res, xlab= "x1x2", ylab= "Res", pch= 20)
abline(h=0)

# QQ plot
qqnorm(mod1$res,pch=20)
qqline(mod1$res)