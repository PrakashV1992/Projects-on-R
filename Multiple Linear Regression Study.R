##Multiple linear regression
##Data on annual income, household size and annual credit card charges
##Study consumer charecteristics on the amount charged by credit card user
##Sample has data from 50 households

setwd("D:/Analytics/R/Dataset/L.Regression and multiple L.Regression")
mydata = read.csv("Consumer.csv", header = TRUE)
mydata

attach(mydata)
names(mydata)
##"Annual Income = Income(in $1000)" "No: of people in house = House"  
##"Total Credit card usage = Amount(in $)"
dim(mydata)

Model = lm(Amount ~ Income+House, data = mydata)
Model
summary(Model)
##yhat = 1304.905 + 33.133.(Income) + 356.296.(House)
##Interpretations
###1304.905 USD is not accounted in model with dependant variables OR 
###All house holds use 1304.905 USD defaultly no matter value of Income and House

###If House in constant and if income increases by $1000, 
###then Amount/Cr card usage is estimated to increase by $33.133

###If Income in constant and if House increases by 1 person, 
###then Amount/Cr card usage is estimated to increase by $356.296

###Multiple R-squared:  0.8256,  states 82.56% change in Amount is 
###accounted by independent variables Income and House
###which is again Regression Sum Sq / Total Sum Sq from anova(Model)

anova(Model)

###Fstaistic is 111.2 on 2 and 47 DOF, pvalue is 2.2e-16
###DOF total is 50-1 is 49. DOF of Regression is 2 namely Income and House
###So DOF of Residuals is 49-2 is 47
###Also H0: All Beta's are zero and H1: Atleast one Beta is non zero
###Since p-value is << alpha(0.05), we accept alternate hypothesis
###So we conclude there is good regression existing between Amount, Income and house 

#<<Next question in mind is, okay we confirm there is good regression between
#  the variables but do we know how significant is individual component 
#  (Income,House and Intercept) related to Amount>>

###In summary(Model) we individual signifacance pvalues and tvalues.
###Individual pvalues << alpha, accept alt hypothese in each case as Beta != 0
###Individual tvalues are Estimated / Std. Error
###Since individual Beta's are significant our Model is robust and 
###statistically valid. It can be used for Predictive Analytics

###Next we study confidence interval for different attributes in deciding 
#  population Amount value.

confint(Model,"Income")
#          2.5 %   97.5 %
#Income 25.15061 41.11541
###Prob(Income lieing b/w 25.15 and 41.11) is 95%

confint(Model,"House")
#         2.5 %   97.5 %
#House 289.5043 423.0875
###Prob(House lieing b/w 289.5 and 423) is 95%

confint(Model,"Intercept")
#         2.5 % 97.5 %
#Intercept    NA     NA

###A conservative manager who takes very negligible risk says he would now use
#  $25.15 as increase in population slope of Income for $1000 or 1% increase 
#  in Income.
#  $289.5 as increase in population slope of House for count of no: of person
#  increased by 1
###Also he uses lwr limit values for prediction

###Predict for new data values of Income:$40000 and House:3
newdata = data.frame(Income = 40, House = 3)
Prediction = predict(Model,newdata)
Prediction
###Gives $3699.113 for Amount
###Being conservative manager, we would need high probability event,
Prediction = predict(Model,newdata, interval = "confidence")
Prediction
###      fit      lwr      upr
### 3699.113 3580.297 3817.928
###prob(Cr card usage to be in b/w 3580.29 and 3817.92) is 95%.
###Or I'm 95% confident that predicted value of Amount is 3699.113
###But conservative manager takes Crusage for the family as $3580.29

###Study backtracking ability of the model
Actual = Amount
Prediction = predict(Model)
Prediction
Backtrack = data.frame(Actual,Prediction)
Backtrack

plot(Actual,col = "Red")
lines(Actual,col = "Red")
plot(Prediction,col = "Blue")
lines(Prediction,col = "Blue")
###Predicted values more or less mimics the Actual

###Adjusted RSq
###Value of RSq after adjusting to DOF = 1 - RSq(Inflated Error)
#  RSq(Inflated Error) = RSq(Error) x Multiplier
#  Multiplier = DOF(Total) / DOF(Residuals) = 49 / 47 = 1.04
#  RSq(Error) = 1 - 0.8256 = 0.1744

#  RSq(Inflated Error) = 0.174 x 1.04 = 0.18182
#  Adjusted RSq = 1 - 0.18182 = 0.81817