setwd("D:/Analytics/R/Dataset/PCA and factor analysis")
mydata=read.csv("PCA-Brown.csv", header=TRUE)
mydata

attach(mydata)
mydata=mydata[,2:8]

cor(mydata)
###Check for correlated independent input variables and convert to factors
#  using factor analysis/PCA to remove multicolinearity effect b/w i/p vars
###Assumption in MLR: No multicolinearity 

library(nFactors)
###Eigen value coputation
ev = eigen(cor(mydata)) # get eigenvalues and eigenvectors
ev                      # EigenValues and NxN EigenVector matrix                
###Limit ev to 5 decimal places
print(ev, digits=5)
 
EigenValue=ev$values    #Filter Eigen value into variable EigenValue
EigenValue                  

###Scree plot to determine no: of factors
Factor=c(1,2,3,4,5,6,7)   
Scree=data.frame(Factor,EigenValue)
plot(Scree,main="Scree Plot", col="Blue",ylim=c(0,4))
lines(Scree,col="Red")
###From above plot,
#  No: of factors is number of Eigen values which are before than flat line or
#  elbow bend. So No: of factors is 4. But from Kaisers Normalisation rules, 
#  any Eigenvalues < 1 are not significant
###So Total No: of factors finally is 3

library(psych)
Unrotate=principal(mydata, nfactors=3, rotate="none", residuals = FALSE)
print(Unrotate,digits=3)

UnrotatedProfile=plot(Unrotate,row.names(Unrotate$loadings ))

Rotate=principal(mydata,nfactors=3,rotate="varimax")
print(Rotate,digits=3)

RotatedProfile=plot(Rotate,row.names(Rotate$loadings),cex=1.0)

