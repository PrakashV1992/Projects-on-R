##Linear regression sample problem
##Candy problem: Video4 in Week2

###Study relation between Price and sales
###Sales is dependent variable

##Candy sales Data in six cities
City <- c("RF","HD","EW","PC","RE","SW")
Price <- c(1.3,1.6,1.8,2,2.4,2.9)
Sales <- c(100,90,90,40,38,32)
Candy <- data.frame(City,Price,Sales)
Candy

cor(Price,Sales)  ###Correlation is negative 0.8854044

SLM <- lm(Sales~Price)
SLM               ###slope is -48.19  and intercept is 161.39
summary(SLM)      ###All details of summary model
###From above intercept and slope values, for every 1% increase in price
##there will be 
##Residual standard error: 16.3 on 4 degrees of freedom
##Multiple R-squared:  0.7839,	Adjusted R-squared:  0.7299 
##F-statistic: 14.51 on 1 and 4 DF,  p-value: 0.01895
##Multiple RSq is PriceSumSq/TotalSumSq which is 3855.4/4918 equals 0.7839
##i.e. 78.39 % of total variation in sales is explained by price

plot(Price,Sales,col="Blue",abline(lm(Price~Sales),col="Red")) 
###No regression line coming up for the above model

##Study confidence interval forall parameters
confint(SLM,"Price")
##O/P is  below
##          2.5 %   97.5 %
##  Price   -83.31  -13.07

confint(SLM,"Sales")
##O/P is below
##         2.5 %       97.5 %
##Sales    NA           NA

###Check back tracking ability of the model
###Compare Prediction and Actual
Prediction = predict(SLM)
Actual = Sales
Backtrack = data.frame(Actual,Prediction)
Backtrack

plot(Actual,col="Red",xlab="data point")
lines(Actual,col="Red")
plot(Prediction,col="Blue",xlab="data point")
lines(Prediction,col="Blue")

Candy1=data.frame(Price=3)
predict(SLM,Candy1,interval="confidence")

  
  