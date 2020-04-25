##ONe sampled TTEST
#set workking direactory
setwd("D:/Analytics/R/Dataset")
#get working direactory
getwd()


#read InternetMobileTime file into myData
myData=read.csv("InternetMobileTime.csv", header = TRUE)
myData

##Hypothesis Testing: H0: Mu = 144 H1: Mu not equal to 144
attach(myData)
Xbar = mean(Minutes)
S = sd(Minutes)
Mu = 144
n = 30

tstat = (Xbar???Mu) / (S/(n^0.5))
tstat

degreesoffreedom = 29    ##deg of freed = n ??? 1 = 29
pvalue = pt(tstat,degreesoffreedom)
pvalue

actualPValue = (1 ??? pt(tstat,degreesoffreedom) ) * 2
actualPValue

##since the actual pvalue is 0.23, confidnce level is .0.77 or 77%
##here we accept null hyp as pvalue>alpha
##Eventhough our mean Xbar is 175.2667 we conclude Mu is 144 accepting null hyp. which proves outside variation does not impact statistical prediction
