rm(list=ls())
# Set a working directory
setwd("/Users/jacob/Desktop/15:16/Spring 2016/ECON 490/R")
# Read a data file
psid <- read.csv("updated_psid.csv", header = TRUE)

#Problem 1

logit_out = glm(treat~age+education+married+black+hispanic+nodegree+re74+re75+u74+u75, data = psid, family = binomial(logit))

psid$pscore = logit_out$fitted.values # attached the pscore to psid data


#Problem 2

p.quan = as.numeric(quantile(psid$pscore[psid$treat],  probs = c(0.2, 0.4, 0.6, 0.8, 1)))
summary(p.quan)

for (i in (1:length(psid$pscore))){
  if (psid$pscore[i] <= p.quan[1]) psid$p.class[i]=1 
  if (p.quan[1]<psid$pscore[i] & psid$pscore[i] <= p.quan[2]) psid$p.class[i]=2 
  if (p.quan[2]<psid$pscore[i] & psid$pscore[i] <= p.quan[3]) psid$p.class[i]=3 
  if (p.quan[3]<psid$pscore[i] & psid$pscore[i] <= p.quan[4]) psid$p.class[i]=4 
  if (p.quan[4]<psid$pscore[i]) psid$p.class[i]=5
}
#Problem 3
num.tab = table(psid$p.class, psid$treat==1) 
mean.gp = aggregate(psid$re78, by = list(psid$p.class, psid$treat==1), mean); mean.gp
sd.gp = aggregate(psid$re78, by = list(psid$p.class, psid$treat==1), sd); sd.gp


# Calculate ATT for each group

att.class = mean.gp[6:10,3] - mean.gp[1:5, 3]
se.class = sqrt(sd.gp[1:5,3]^2/num.tab[,1] + sd.gp[6:10,3]^2/num.tab[,2])
att.class
se.class

# Overall ATT and s.e.

att = sum(att.class*num.tab[,2]/sum(num.tab[,2]))
se = sqrt(sum(se.class^2*num.tab[,2]^2/sum(num.tab[,2])^2))

att; se

#Problem 4

install.packages("MatchIt") ; library(MatchIt)

m.out = matchit(treat~age+education+married+black+hispanic+nodegree+re74+re75+u74+u75, method = "subclass", subclass=5, data=psid)

data.pscore = match.data(m.out)
mean.gp = aggregate(data.pscore$re78, by = list(data.pscore$subclass, data.pscore$treat==1), mean)
sd.gp = aggregate(data.pscore$re78, by = list(data.pscore$subclass, data.pscore$treat==1), sd)
mean.gp
sd.gp
# Calculate ATT for each group

att.class = mean.gp[6:10,3] - mean.gp[1:5, 3]

att.class
#Overall ATT
sum(att.class)