

#Problem Set 3


#Problem 1
#Created  in excel using if(condition, then, else) command.

#Problem 2

# First set the working directory
setwd("/Users/jacob/Desktop/15:16/Spring 2016/ECON 490/R")
getwd()

# Get the dataset for the problem given
psid = read.csv("psid.csv", header = TRUE)
attach(psid)

apply(lalonde, 2, mean)
apply(lalonde, 2, sd)

# calculate mean and standard deviation BY THE TREATMENT LEVEL
gp.mean = aggregate(psid, by=list(treat==1, treat==0), mean)
gp.sd = aggregate(psid, by=list(treat==1, treat==0), sd)

# to make a summary statistics table
stat_tab = matrix(rep(NA, 66), 11, 6)

stat_tab[ ,1] = t(gp.mean[1, 5:14])
stat_tab[ ,3] = t(gp.mean[2, 5:14])
stat_tab[ ,2] = t(gp.sd[1, 5:14])
stat_tab[ ,4] = t(gp.sd[2, 5:14])
stat_tab[,5] = stat_tab[,1] - stat_tab[,3]
stat_tab[,6] = stat_tab[,5]/sqrt((stat_tab[,2]^2 + stat_tab[,4]^2)/2)
stat_tab

#Problem 3
install.packages("lmtest") ; library(lmtest)
install.packages("sandwich") ; library(sandwich)

reg1 = lm(re78~treat)
reg2 = lm(re78~treat+re74+u74+ re75+u75)
reg3 = lm(re78~treat+black+hisp+age+married+nodegree+education+re74+u74+ re75+u75)

coeftest(reg1, vcov = sandwich)
coeftest(reg2, vcov = sandwich)
coeftest(reg3, vcov = sandwich)