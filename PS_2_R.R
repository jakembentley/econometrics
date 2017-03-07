#Problem Set 2
rm( list =ls())
setwd("/Users/jacob/Desktop/15:16/Spring 2016/ECON 490/R")
lalonde = read.csv("lalonde.csv", header = TRUE)
attach(lalonde)

regHomo <- lm(re78~ treat, data = lalonde)
summary(regHomo)
regHomo_output <- summary(regHomo)
#Std.Error == 632.9
CI_high = 1793.6 + 1.96*632.9
CI_high
CI_low = 1793.6 - 1.96*632.9
CI_low
#CI is (553.116, 3034.084)

#Problem 2
install.packages("lmtest")
library(lmtest)
install.packages("sandwich")
library(sandwich)
coeftest(regHomo, vcov = sandwich)
#ATE = B = 1793.61
#Std. Error = 669.33
CI_hlow = 1793.61 - 669.33*1.96
CI_hhigh = 1793.61 + 669.33*1.96
CI_hlow
CI_hhigh
#CI for conditional heteroskedasticity = ((481.72312, 3105.497))