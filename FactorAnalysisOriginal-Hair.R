##Advance stats project on Hair.csv
##Anova/Ancova, Regression and  Factor analysis

setwd("D:/Analytics/R/Projects/Factor analysis on Hair data set")
MyData= read.csv("Factor-Hair-Revised.csv", header = TRUE)
MyData

rpivotTable(MyData)

attach(MyData)
names(MyData)

##Remove unique id column from study
MyData= MyData[,2:13]
MyData

summary(MyData)
##Summmary gives quartile ranges of each variable.
##This conirms variables are taken as numeric values and not factors
##This also confirms there are no N.A(Missing) values in our dataset

###Rename column names to meaningful values
colnames(MyData)= c("Product Quality","E-Commerce","Technical Support"
                    ,"Complaint Resolution","Advertising","Product line"
                    ,"SalesForce Image","Competitor's Pricing","Waranty Claim"
                    ,"Order Billing","Delivery Speed","Customer Satisfaction")
View(MyData)

sum(is.na(MyData))
##To confirm dataset does not have any missing values

##Calculate upper and lower lower fences to cap the outliers
#for (i in c(1:11)) {
#  Q1=quantile(MyData[,i],0.25)
#  Q3=quantile(MyData[,i],0.75)
#  Iqr= IQR(MyData[,i])
#  upperFence=Q3+1.5*IQR 
#  lowerFence=Q1-1.5*IQR 
#  MyData[,i]=ifelse(MyData[,i]<lowerFence,lowerFence,MyData[,i])
#  MyData[,i]=ifelse(MyData[,i]>upperFence,upperFence,MyData[,i])
#}

###Check for outliers and cap it to upper and lower fence values
boxplot(ProdQual)
boxplot.stats(ProdQual)$out

boxplot(Ecom)
boxplot.stats(Ecom)$out
Q1= as.numeric(quantile(MyData[,2],0.25))
Q3= as.numeric(quantile(MyData[,2],0.75))
Iqr= as.numeric(IQR(MyData[,2]))
upperFence= Q3 + 1.5*(Iqr)  
lowerFence= Q1 - 1.5*(Iqr) 
Ecom= ifelse(Ecom>upperFence, upperFence, Ecom)
Ecom= ifelse(Ecom<lowerFence, lowerFence, Ecom)
##Caps to 2.3 and 4.9

boxplot(TechSup)
boxplot.stats(TechSup)$out

boxplot(CompRes)
boxplot.stats(CompRes)$out

boxplot(Advertising)
boxplot.stats(Advertising)$out

boxplot(ProdLine)
boxplot.stats(ProdLine)$out

boxplot(SalesFImage)
boxplot.stats(SalesFImage)$out
quantile(SalesFImage,0.95) #7.1
SalesFImage= ifelse(SalesFImage>7.1, 7.1, SalesFImage)

boxplot(ComPricing)
boxplot.stats(ComPricing)$out

boxplot(WartyClaim)
boxplot.stats(WartyClaim)$out

boxplot(OrdBilling)
boxplot.stats(OrdBilling)$out
Q1= as.numeric(quantile(MyData[,10],0.25))
Q3= as.numeric(quantile(MyData[,10],0.75))
Iqr= as.numeric(IQR(MyData[,10]))
upperFence= Q3 + 1.5*(Iqr)  
lowerFence= Q1 - 1.5*(Iqr) 
OrdBilling= ifelse(OrdBilling>upperFence, upperFence, OrdBilling)
OrdBilling= ifelse(OrdBilling<lowerFence, lowerFence, OrdBilling)
##Caps to 2.05 and 6.45

boxplot(DelSpeed)
boxplot.stats(Advertising)$out
Q1= as.numeric(quantile(MyData[,11],0.25))
Q3= as.numeric(quantile(MyData[,11],0.75))
Iqr= as.numeric(IQR(MyData[,11]))
upperFence= Q3 + 1.5*(Iqr)  
lowerFence= Q1 - 1.5*(Iqr) 
DelSpeed= ifelse(DelSpeed>upperFence, upperFence, DelSpeed)
DelSpeed= ifelse(DelSpeed<lowerFence, lowerFence, DelSpeed)
##Caps to 1.86 and 5.96

boxplot(Satisfaction)
boxplot.stats(Satisfaction)$out
##All outliers are capped to upper and lower fence values

###Study correlation matrix beore normalization
cor(MyData)
##ProdQual is in moderate correlation with Prodline,CompPrice and Satisfaction
##Ecom is modrate cor with Advert and good cor with SalesFImage
##TechSup has good cor with WarantyClaim
##Complaint Res. has mod cor with Prodline, good cor with OrdBilling
## and and Satisfaction whereas high cor with DelSpeed
##Advert has moderate cor with Ecom and SalesFImage
##Product line has moderate cor with ProdQual, Complaint Resolution,
## Competitor Pricing,OrdBilling, DelSpeed and Satisfaction
##SalesFImage has good cor with Ecom and moderate cor with Advert and Satisfaction
##Competitors price has moderate cor with ProdQual and Prodline
##WarantyClaim has good cor with TechSupport
##OrdBilling has good cor with CompRes and Delspeed and moderate cor with Satisfaction
##DelSpeed has very good cor with CompRes, good cor with Prodline,OrdBilling and Satisfaction
###so finally independent variable customer satisfaction has a moderate cor with ProdQual,
#  ComplaintResolution, Prodline, SalesFImage, Orderbilling and Deliveryspeed
##We can try cor.test(col1,col2) to test cor's between each pair of variables


##Remove independent variable Satisfaction for PCA/FA
MyData= MyData[,1:11]
View(MyData)
cor(MyData)

###Below princomp gives factor loadings of variance explained by each factors as a list 
Princomp_Mydata= princomp(MyData)
print(Princomp_Mydata$loadings)

##MyData_scaled= as.data.frame(scale(MyData))
##summary(MyData_scaled)
##range(MyData_scaled$`Product Quality`)
##range(MyData_scaled$`E-Commerce`)
##sd(MyData_scaled$`Product Quality`)
##Scales all column values to mean 0 and s.d 1 but in ranges

##Diff btw scaled and rescaled is scale different ranges according to values of columns
##and MydData_rescaled will be all values rescaled to range [-1, 1]. Both scaling keeps 
##standard devaition of observations of column in same proportion

##Rescaline individual columns to [-1, 1]
##install.packages("scales")
##library(scales)
##library will make sure current package is installed and when a inbuilt function like rescale
##is called it checks in all installed packages from top and first occurence of function 
##rescale is only used. If previously installed package is cars and it has a rescale func(),
##then R considers car's rescale when called. So we need specify which package to be used 
##first using require(package)
##require(scales)
##MyData_Rescaled= rescale(MyData)

##Below is rescaling of each col but its not reqd as all variables are distributed normally
##We can also use packagename::function name for the same without require(package) 
##ProdQual_rescaled= rescale(ProdQual, to= c(-1, 1), from = range(ProdQual))
##Ecom_rescaled= rescale(Ecom, to= c(-1, 1), from = range(Ecom))
##TechSup_rescaled= rescale(TechSup, to= c(-1, 1), from = range(TechSup))
##CompRes_rescaled= rescale(CompRes, to= c(-1, 1), from = range(CompRes))
##Advertising_rescaled= rescale(Advertising, to= c(-1, 1), from = range(Advertising))
##ProdLine_rescaled= rescale(ProdLine, to= c(-1, 1), from = range(ProdLine))
##SalesFImage_rescaled= rescale(SalesFImage, to= c(-1, 1), from = range(SalesFImage))
##ComPricing_rescaled= rescale(ComPricing, to= c(-1, 1), from = range(ComPricing))
##WartyClaim_rescaled= rescale(WartyClaim, to= c(-1, 1), from = range(WartyClaim))
##OrdBilling_rescaled= rescale(OrdBilling, to= c(-1, 1), from = range(OrdBilling))
##DelSpeed_rescaled= rescale(DelSpeed, to= c(-1, 1), from = range(DelSpeed))
##Satisfaction_rescaled= rescale(Satisfaction, to= c(-1, 1), from = range(Satisfaction))

##Validate if data is feasable for doing PCA using bartlett sphericity test
print(bartlett.test(list(MyData$`Product Quality`,MyData$`E-Commerce`,
                         MyData$`Technical Support`,MyData$`Complaint Resolution`,
                         MyData$Advertising,MyData$`Product line`,MyData$`SalesForce Image`,
                         MyData$`Competitor's Pricing`,MyData$`Waranty Claim`,
                         MyData$`Order Billing`,MyData$`Delivery Speed`)))
##pvalue is 2.2e-16 which ishighly significant.So Model is very good for doing PCA

library(nFactors)
EV= eigen(cor(MyData))
EigenValue= EV$values          ##Total variance explained by each factor 
EigenVector= EV$vectors        ##Covariance matrix of x
##Gives Eigenvector with lamda values of each component and 
##Covariance matrix of the relationship between each variable

##SCREE plot to determine no of  factors
factors= c(1:11)
Scree=data.frame(factors,EigenValue)
plot(Scree, main="Scree plot", col="Red")
lines(Scree,col="Red")
##From Scree plot we can say 5 factors before elbow bend
EigenValue
##Adding Kaiser Normalization, where lambda should be >1, we can consider 4 factors

library(psych)
Unrotate= principal(MyData,nfactors = 4,rotate ="none",residuals = TRUE,scores = TRUE)
###Unrotated model Object created with 4 factors
print(Unrotate)
###Unrotated model loadings of each component to individual variables
###PC1 significantly explains variances in ComplaintRes, Prodline, Ordbilling and DelSpeed 
###PC1 also explains 30% variances in all other variables
###PC2 significantly explains variances in Ecom, SalesFImage, competitor pricing
###PC2 moderately explains variance in ProdQual, Advert, Prodline 
###PC3 significantly explains variance in TechSup, Warrantyclaim and around 30% variance 
### in Ecom, CompRes, SalesFImage, OrdBilling and Delspeed
##PC4 explains variance in ProdQual and 20 to 30% variance in other variables

##Individual components of Unrotated loadings are
Unrotate$residual  ###Error/Residual covariance matrix
Unrotate$scores    ###Score standings of components values for each observation

##Plot Unrotated loadings graphically
unrotatedProfile= plot(Unrotate,row.names(Unrotate$loadings))
###Above plot shows there is no clear demarkation between which component is explaining 
###what all variables

##Plot Unrotated loadings graphically
biplot(Unrotate,scale= 0)
###Gives loading direactions for each component

Rotate= principal(MyData,nfactors = 4,rotate ="varimax",residuals = TRUE,scores = TRUE)
print(Rotate,digits = 3)
###After rotation we could somewhat see clear demarkation between factors 
#  explaining each variables till an extent
###RC1 explains Compres,Ordbilling and delspeed significantly(ie 90%) and Prodline moderately
###RC2 explains variance in Ecom,Advert,SalesFImage significantly
###RC3 explains changes in TechSup and WarrantyClaim significantly
###RC4 explains prodQual significantly, Comppricing moderately
###Advertising and CompPricing are explained moderately only
###communality of Advertising and Competitor's Pricing is 0.58 and 0.64 respectively
###Total variance in all variables explained by 4 factors together is 80% 
###Additionally we can call RC1 as a sales factor
###RC2 is more of social networking factor whereas RC3 is product support factor
###RC4 is a quality factor
###pvalue remains more or less same: unrotated to rotated: 0.0018 to 0.0017

Rotate1= principal(MyData,nfactors = 4,rotate ="oblique",residuals = TRUE,scores = TRUE)
print(Rotate1,digits = 3)
###Net loading correlation have come down compared to ortho rotation

cor(Rotate$loadings)
##Cant find much differences in correlations b/w oblique and orthogonal loadings

###Ready sample for regression analysis
MyDataFactor= factor.scores(MyData,f=Rotate$loadings,method = "Harman")
print(MyDataFactor,digits = 3)
MyDataRegression=MyDataFactor$scores
MyDataRegression= as.data.frame(MyDataRegression)
rpivotTable(MyDataRegression) ##Descriptive stats in tabular format

###REGRESSION
##Omit NA rows
sum(is.na(MyDataRegression))    #0. No N.A values
colnames(MyDataRegression)

##Detect outliers
boxplot(MyDataRegression[,1])  ##No outliers| Looks Normally distributed
boxplot(MyDataRegression[,2])  ##outliers are there |right skewed
boxplot(MyDataRegression[,3])  ##No outliers| left skewed
boxplot(MyDataRegression[,4])  ##No outliers| left skewed
Q1= as.numeric(quantile(MyDataRegression[,2],0.25))
Q3= as.numeric(quantile(MyDataRegression[,2],0.75))
Iqr= as.numeric(IQR(MyDataRegression[,2]))
upperFence= Q3 + 1.5*(Iqr)  
lowerFence= Q1 - 1.5*(Iqr) 
MyDataRegression[,2]= ifelse(MyDataRegression[,2]>upperFence, upperFence, MyDataRegression[,2])
MyDataRegression[,2]= ifelse(MyDataRegression[,2]<lowerFence, lowerFence, MyDataRegression[,2])
##No outliers. Caps to -1.84 and 1.84

##Plot each factors toverify if normally distributed
par(mfrow = c(2,2))   
hist(MyDataRegression[,1])
hist(MyDataRegression[,2])
hist(MyDataRegression[,3])
hist(MyDataRegression[,4])
##RC2 and RC3 looks normally distributed but RC1 is right skewed whereas RC4 has two peaks

##qqnorm plot varification
par(mfrow = c(2,2))   
qqnorm(MyDataRegression[,1])
qqnorm(MyDataRegression[,2])
qqnorm(MyDataRegression[,3])
qqnorm(MyDataRegression[,4])

##Check skewness of each factor
skew(MyDataRegression[,1])   #-0.2367125
skew(MyDataRegression[,2])   #0.2217596
skew(MyDataRegression[,3])   #0.004555717
skew(MyDataRegression[,4])   #-0.04050934
###skew of -1 to 1 is less skewed

###Libraries for using boxcox lamda transformation
library(forecast)
library(moments)
##Find lamda values for each factor
BoxCox.lambda(MyDataRegression[,1])
BoxCox.lambda(MyDataRegression[,2])
BoxCox.lambda(MyDataRegression[,3])
BoxCox.lambda(MyDataRegression[,4])
###All lamda values are between 0.75 to 1.5. So no transformation needed.
###All factors  are already normallyy distributed

###Verify if all factors have good correlation with independent variable Satisfaction 
###First add independent variable Satisaction to MyDataRegression
BoxCox.lambda(Satisfaction)
SatisfactionRegression= sqrt(Satisfaction)
cbind(MyDataRegression,SatisfactionRegression)
qqnorm(SatisfactionRegression)

##Study relaion between each independent variable Satisfaction and dependent variables
par(mfrow = c(2,2)) 
plot(MyDataRegression[,1],Satisfaction)
plot(MyDataRegression[,2],Satisfaction)
plot(MyDataRegression[,3],Satisfaction)
plot(MyDataRegression[,4],Satisfaction)

##Correlaions between each i/p variables and Satisfaction
cor(MyDataRegression[,1],Satisfaction)
cor(MyDataRegression[,2],Satisfaction)
cor(MyDataRegression[,3],Satisfaction)
cor(MyDataRegression[,4],Satisfaction)

##Study relaion between each independent variable SatisfactionRegression and dependent variables
par(mfrow = c(2,2)) 
plot(MyDataRegression[,1],SatisfactionRegression)
plot(MyDataRegression[,2],SatisfactionRegression)
plot(MyDataRegression[,3],SatisfactionRegression)
plot(MyDataRegression[,4],SatisfactionRegression)

##Correlaions between each i/p variables and SatisfactionRegression
cor(MyDataRegression[,1],SatisfactionRegression)
cor(MyDataRegression[,2],SatisfactionRegression)
cor(MyDataRegression[,3],SatisfactionRegression)
cor(MyDataRegression[,4],SatisfactionRegression)
cor(MyDataRegression)
###Verification for no multi colinearity concept

##Model1: MyDataRegression[,3] is not  included as it does not have good cor with Satisfaction
Model1= lm(SatisfactionRegression ~ MyDataRegression[,1] + MyDataRegression[,2] 
           + MyDataRegression[,4], data = MyDataRegression)
summary(Model1)
###RSq is 0.6892 and pvalue isvery highly significant
#  Fstat is 70.96 and pvalue is 2.2e-16

##Verify multicolinearity via varaince inflation test(VIF)
library(car)
VIF1=vif(Model1)    ##1.04  1.008  1.009
###VIF values < 5 .So multicolinearity exists
###To verify again, check if (VIF > 1/(1-Rsq)), then it means predictors are more related to Saisfaction that to themselves
###1/(1-0.68) is 3.125 which is < VIF

###Standard Error in predicted value is sqrt(VIF) value, i.e.
StdErrorPredicted= sqrt(VIF)
colnames(MyDataRegression) = c("Sales","Networking","Support","Quality")

anova(Model1)
###Percent of variances in satisfaction explained by all 3 facors together is 
#  Tot SumSq is 1.48+0.89+1.15+1.59 =5.11; 
#  %variance explained is 3.52/5.11 is 0.68(RSq)
#  pvalues are highly significant

###Study relation b/w  Error and Variables
ResidualCorrelation= c(1:4)
for (i in c(1:4)) {
  ResidualCorrelation[i]= cor(MyDataRegression[,i],Model1$residuals)
}
ResidualCorrelation

ncvTest(Model1,data= MyDataRegression,~ MyDataRegression[,1] + MyDataRegression[,2] 
        + MyDataRegression[,4])
###ncv test pvalue is 0.0007 and Chisq is 16.92

spreadLevelPlot(Model1)

###MODEL2###
###Model2: MyDataRegression[,3] is not  included as it does not have good cor with Satisfaction
###Include interaction effects
Model2= lm(SatisfactionRegression ~ MyDataRegression[,1] + MyDataRegression[,2] + MyDataRegression[,4]
           + MyDataRegression[,1]*MyDataRegression[,2] + MyDataRegression[,2]*MyDataRegression[,4])
summary(Model2)
###RSq is 0.7127 and pvalue remains same(2.2e-16) and Fstat is 46.64
###1*4 and 1*2*4 were not significant interaction parameters. So avoided in Model2

ncvTest(Model2,data= MyDataRegression, ~ MyDataRegression[,1] + MyDataRegression[,2] 
        + MyDataRegression[,4] + MyDataRegression[,1]*MyDataRegression[,2] + 
          MyDataRegression[,2]*MyDataRegression[,4])
###Chisq value has improved to 18.56 but pvalue after including interaction in Model2 has 
#  decreased to 0.0023 from 0.0007.  

#Verify multicolinearity via varaince inflation test(VIF)
VIF2=vif(Model2) 

###Standard Error in predicted value is sqrt(VIF) value, i.e.
StdErrorPredicted= sqrt(VIF2)

anova(Model2)
###Percent of variances in satisfaction explained by all 3 facors together is 
#  Tot SumSq is 1.48+0.89+1.15+1.59 =5.11; 
#  %variance explained is 3.52/5.11 is 0.68(RSq)
#  pvalues are highly significant

spreadLevelPlot(Model1)
spreadLevelPlot(Model2)

confint(Model1,SatisfactionRegression)
confint(Model2,Satisfaction)


