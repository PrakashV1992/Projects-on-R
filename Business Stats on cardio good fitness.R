#set workking direactory
setwd("D:/Analytics/R/Dataset")
#get working direactory
getwd()


#read CardioGoodFitness file into myData
myData <- read.csv("CardioGoodFitness.csv",header = TRUE)


#Install neccessary packages
install.packages("ggplot2")
library(ggplot2)
install.packages("psych")
library(psych)
install.packages("dplyr")
library("dplyr")
install.packages("lattice")
library("lattice")

myData

##To avoid using $variable every time we use the variable
###column names are declared as variables and to access individual column just use variable name
attach(myData) 

##To get summary product wise
###indices gives saperate by what unique element and FUN is function
by(myData,INDICES = Product,FUN = summary)

##To get summary age wise
###indices gives saperate by what unique element and FUN is function
by(myData,INDICES = Age,FUN = summary)

summary(myData)

##Above 45 are outliers,right squeed box plot
boxplot(Age,col = "Red", main  = "Box plot of Age",horizontal = TRUE)

##To identify gender wise , Age ~ gender is age as function/relation of a gender
boxplot(Age~Gender,col = c("Red","Blue"), main  = "Box plot of Age",horizontal = TRUE)

##To identify gender wise , Age ~ gender is age as function/relation of a gender
###Used to identify individual product usage wrt to Age
boxplot(Age~Product,col = c("Red","Blue","Yellow"), main  = "Box plot of Product",horizontal = TRUE)

##Gives table ,plots of comparision between multiple variables 
install.packages("rpivotTable")
library(rpivotTable)
rpivotTable(myData)

##yaxis is  miles,factor to classify number of graphs in the plot
histogram(~Miles,factor(gender),data = myData)

##corelation btw two variables. corelation percentage is Corvalue * 100
cor(Miles,Usage)

Model1 = lm(Miles~Usage,data = myData)
summary(Model1)

Model2 = lm(Miles~Usage + Fitness,data = myData)
summary(Model2)

