#
#
# 8050_hw2Script_fa14.R
#
# Andrew Brown
#
# 5 Sept 2014

# If you're writing an entire script that is meant to be run in a "fresh"
# R environment (with no previously declared variables, etc), it is helpful
# to clear out all stored values to avoid potentially confusing results or
# broken code.
rm(list= ls())

# Move R to the right directory
setwd("C:/Users/ab7/Desktop/NDrive/Courses/Clemson/805/FA14/Datasets")

# Read in the salt concentration data, after converting it to a tab-delimited 
# text file
salt <- read.table("SaltConc.txt", header= TRUE)

# We can find everything but the standard deviation using the summary function
# (Or you can use the mean(), median(), max(), etc. functions)
summary(salt)
sd(salt$Salt)
sd(salt$Area)

# Side by side boxplots
boxplot(salt)

# Histogram of Area
hist(salt$Area, xlab= "Area", main= "Area Distribution")

# Scatterplot 
plot(Salt ~ Area, data= salt, ylab= "Salt", xlab= "Roadway Area")

# Function to return the second observation in a dataframe, and a test on the 
# salt data
Get2nd <- function(dat) {
  # This function finds the second observation in a dataframe and returns the
  # sum of the observed values
  
  return(sum(dat[2, ]))
  
  
}  # End function Get2nd

Get2nd(salt)

# t test for whether the mean of the salt variable is 20
t.test(salt$Salt, alternative= "two.sided", mu= 20, conf.level= .9)
