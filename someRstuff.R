#
# someRstuff.R
# Sept. 5, 2016
#
#
# It's good manners to include comments!
#
# Here I'm just introducing R to the 8050 class

rm(list= ls())  # This clears the workspace - especially useful
  # if you're running the entire script assuming a fresh workspace

# Create two variables and remove them from memory
dat1 <- 3  # You can highlight code and hit "CTRL-R" to run 
  # directly from the script file
my.var <- "Andrew"

# R is open source - you can look inside most functions for free
lm  # call the function with no "()"

# R comes loaded with a lot of datasets already installed
data()


# R also has some pre-installed packages
library(MASS)  # has some helpful regression functions
  # Based on Modern Applied Statistics with S, by Venables & Ripley


head(Seatbelts)  # look at the "top" of a dataset to see what's 
  # in it
names(Seatbelts)
class(Seatbelts)


# Get the machine data
getwd()
setwd("~/MATH 8050 Notes")
list.files()

machine <- read.table("MachineData.txt", header= TRUE)
head(machine)
names(machine)
names(machine) <- c("mach", "diam")

(wire <- read.table("WireBondData.txt", header= TRUE))

head(wire)

# Make a scatterplot
plot(Strength ~ DieHeight, data= wire, pch= 20, xlab= "X",
    ylab= "Y", main= "A plot")

# The lm() function fits a regression model
# Syntax lm(response ~ predictor, dataset name)
mod <- lm(Strength ~ DieHeight, data= wire)
summary(mod)
abline(mod)


mat <- as.matrix(wire)  # casts the dataframe to a matrix
mat[3,1]
colnames(mat) <- c()

# Negative indexing is basically "everything but"
mat2 <- mat[-c(20,24), ]  # store everything except the 20th and
  # 24th row


summary(mat[ ,1])

names(machine)
boxplot(diam ~ mach, data= machine)
