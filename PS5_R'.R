rm(list=ls())
# Set a working directory
setwd("/Users/jacob/Desktop/15:16/Spring 2016/ECON 490/R")
# Read a data file
eitc <- read.csv("eitc.csv", header = TRUE)
eitc

#Problem 1
eitc$posttreat = as.numeric(eitc$year >=1994)
eitc$anykids = as.numeric(eitc$children != 0)

#Problem 2
imean <- mean(eitc$work[eitc$posttreat ==0 & eitc$anykids == 0])
imean
#imean = .5754597
iimean <- mean(eitc$work[eitc$posttreat == 0 & eitc$anykids == 1])
iimean
#iimean = .4459619
iiimean <- mean(eitc$work[eitc$posttreat == 1 & eitc$anykids == 0])
iiimean
#iiimean = .5733862
ivmean <- mean(eitc$work[eitc$posttreat == 1 & eitc$anykids ==1])
ivmean
#ivmean = .4907615

#Problem 3
tau <- (ivmean - iiimean) - (iimean - imean) 
tau
#EITC effect on women with children is .04687313

#Problem 4  
attach(eitc)
DID_estimate = lm(work~posttreat+anykids+anykids*posttreat)
summary(DID_estimate)
#DID estimated as .046873, and  Standard Error is calculated as .017158
