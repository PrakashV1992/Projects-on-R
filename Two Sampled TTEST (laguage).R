##Two sampled TTEST
#set workking direactory
setwd("D:/Analytics/R/Dataset")
#get working direactory
getwd()


#read Luggage file into myData
myData=read.csv("Luggage.csv", header = TRUE)
myData

##We additionaly assume variance of the two sampled TTest is equal apart from the assumption that its a normal distribution
##alpha is 0.05
attach(myData)

##Hypothesis Testing: H0: Mu = means are equal H1: Mu not equal

##Assume unequal varaince, we do welch test
t.test(WingA,WingB)
##So here results shows means are not equal and we accept alt hyp. means are not equal
##t = 5.1615, df = 37.957, p-value = 8.031e-06
##alternative hypothesis: true difference in means is not equal to 0
##95 percent confidence interval:
##1.38269 3.16731
##sample estimates:
##  mean of x mean of y 
##10.3975    8.1225


##Assume variances are true in both wings
t.test(WingA,WingB, var.equal = TRUE)
## We get the same conclusion means are not equal
##t = 5.1615, df = 38, p-value = 8.008e-06
##alternative hypothesis: true difference in means is not equal to 0
##95 percent confidence interval:
##  1.382723 3.167277
##sample estimates:
##  mean of x mean of y 
##10.3975    8.1225 

##In both tests we can infer it has two tails where results show wingA is superior to wingB. Avgtime taken is more in wingA
##95 percent confidence level factor shows WingA will be varying from wingB as mentioned below.
##A will be superior to be by 1.38 mins in lower side or will be superiror to B by 3.16 mins in the upper side at 95 percent confidence level

##To study at 99% confidence level
t.test(WingA,WingB, var.equal = TRUE, conf.level = 0.99)
##data:  WingA and WingB
##t = 5.1615, df = 38, p-value = 8.008e-06
##alternative hypothesis: true difference in means is not equal to 0
##99 percent confidence interval:
##  1.079848 3.470152
##sample estimates:
##  mean of x mean of y 
##10.3975    8.1225 



