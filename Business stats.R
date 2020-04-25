#set workking direactory
setwd("D:/Analytics/R/Dataset")
#get working direactory
getwd()


#read CardioGoodFitness file into myData
myData <- read.csv("CardioGoodFitness.csv")
myData

attach(myData)              ##to access dataset columns direactly by name
summary(myData)             ##summary variables
by(myData,INDICES = Product,FUN = summary)
by(myData,INDICES = Gender,FUN = summary)                     ##Summary gender wise
hist(Age,col = "RED",main = 'Age')
boxplot(Age,col = "RED",main = 'Age', horizontal = TRUE)
boxplot(Age~Gender,col = c("RED","GREEN"),main = 'Age', horizontal = TRUE)

##table summary
table(Product,Gender)                                       
table(Product,MaritalStatus)

##R pivot table opens up a crazy screen with all types of plots
library(rpivotTable) 
rpivotTable(myData)

##stat variables
sd(Age)
mean(Age)

##To analyze two variable relation for 2 types of third variable
##Miles clocked for male and female in two saperategraphs in a single screen
library(lattice)
histogram(~Miles | factor(Gender), data = myData)

##corelation variable
cor(Miles,Usage)    ##75 percent corelation
cor(Miles,Fitness)  ##79 percent  correlation

##Model equation between two variables
##Predict miles eith known usage
model = lm(Miles~Usage,data = myData)
summary(model)   ##Formula is milespredicted= -22.22 + 36.94*(usage)
##To include fitness in the equation,
model1 = lm(Miles~Usage + Fitness,data = myData)
summary(model1) ##Formula is milespredicted= -56.74 + 20.21*(usage)+ 27.20*(fitness)


###HYPOHESIS TESTING
##Karren problem(Product launch)
##H0 : Mu <= 29000
##H1 : Mu >  29000    ##Since hypothesis test only one side,its called one tailed test. Right skewed
##One tailed test as 

