#set workking direactory
setwd("D:/Analytics/R/Dataset")
#get working direactory
getwd()


#read CardioGoodFitness file into myData
myData <- read.csv("CardioGoodFitness.csv")


#Install neccessary packages
install.packages("ggplot2")
library(ggplot2)
install.packages("psych")
library(psych)
install.packages("dplyr")
library("dplyr")


#Tabular representation of CardioGoodFitness
View(myData)
#dimensions of CardioGoodFitness
dim(myData)                      
#Discriptive analysis of CardioGoodFitness
summary(myData)
#Gives brief about CardioGoodFitness
str(myData)

#read individual column data into variables

product <- myData$Product                           ##read products
product                                             ##diplay product
summary(product)                                    ##counts of individual product usage

age <- myData$Age                                   ##read age
age                                                 ##diplay age
table(age)                                          ##Number of people in different age groups     
summary(age)                                        ##descriptive analytics of age

gender <- myData$Gender                             ##read products
gender                                              ##diplay product
summary(gender)                                     ##counts of individual product usage

education <- myData$Education                       ##read Education
education                                           ##diplay Education
table(education)                                    ##Number of people in each education level
summary(education)                                  ##descriptive analytics of education

maritalStatus <- myData$MaritalStatus               ##read MaritalStatus
maritalStatus                                       ##diplay MaritalStatus
table(maritalStatus)                                ##Number of people married and single
summary(maritalStatus)                              ##count of partnered or single

usage <- myData$Usage                               ##read Usage
usage                                               ##diplay Usage
table(usage)                                        ##Number of people at  different usages levels
summary(usage)                                      ##descriptive analytics of usage levels

fitness <- myData$Fitness                           ##read fitness
fitness                                             ##diplay fitness
table(fitness)                                      ##Number of people in various fitness levels
summary(fitness)                                    ##descriptive analytics of fitness levels

income <- myData$Income                             ##read income
income                                              ##diplay income
summary(income)                                     ##descriptive analytics of income

miles <- myData$Miles                               ##read miles
miles                                               ##diplay miles
summary(miles)                                      ##descriptive analytics of miles
table(miles)


#description by graphs
par(mfrow = c(3,3))

##No of  people using different products 
###Usage decreses uniformly from product 1 to 3
plot(product,main = 'Product usage',xlab = 'product', ylab = 'Number of users',col = 'blue')  

##Heat distribution of users in various age groups
###Eventhough the age varies from 18 to 50 , more users are young(25 to 30 years) which is intensified in the plot
hist(age,main = 'Age distribution',xlab = 'Age', ylab = 'Number of users',col = heat.colors(23))

##Box plot of education distribution of users
###Most of the people are in the range 14 to  16 and median shown as black line is 16 
boxplot(education,main = 'Education level of users',xlab = 'Education', ylab = 'Number of users',
        col = 'red')

##male and female users distribution
###no of male users> female users
plot(gender,main = 'Number of male and female user',xlab = 'Gender', ylab = 'Number of users',col = 'yellow')
labels<- c("Female","Male")
#pie(summary(gender),labels,main = "Users gender distribution")

##married and single users distribution
###no of partnered users more than single users
plot(maritalStatus,main = 'Marital status',xlab = 'Married/Single', ylab = 'Number of users',col = 'green')

##Usage distribution
barplot(usage,type = 'line',col = 'red',xlab = 'No of users',ylab = 'usage level',main = 'Usage plot')

##user fitness levels
###eventhough fitness levels varies from 1 to 5, most users fall in level3
hist(fitness,main = 'Fitness levels  of users',xlab = 'Fitness levels', ylab = 'Number of users',col = 'black')

##Income distribution of  users
###Plot shows min,max, 25 percent and 75 percent and median  
boxplot(income,main = 'Income levels of  users', ylab = 'Income in 1000',col = 'orange')

##miles ran by each user
###plot shows miles ran by user where avg lies around 100 miles 
hist(miles,main = 'Miles ran',xlab = 'Miles', ylab = 'Number of users',col = topo.colors(10))


#Data of users using product TM195
TM195 <- myData[myData$Product == "TM195",]
TM195                                               ##display  TM195
summary(TM195)                                      ##summary of TM195 usage

par(mfrow = c(1,3)) 

##Gives relation between age and usage for users of TM195
plot(TM195$Age,TM195$Usage,col = "blue",main = "Age vs Usage comparision",
     abline(lm(TM195$Age~TM195$Usage)),cex = 1.3,pch = 16,xlab = "Age",ylab = "Usage")
##More number of users in age>20 and age<30 fall in usage level 3 . Also in this  age range , usage level 2 and 4 are nearly evenly distributed

##Gives relation between gender and usage for users of TM195
plot(TM195$Gender,TM195$Usage,col = "blue",main = "Gender vs Usage comparision",
     abline(lm(TM195$Gender~TM195$Usage)),cex = 1.3,pch = 16,xlab = "Gender",ylab = "Usage")     
##All females usage lies in level 2 to 3 and male usage lies between 3 to 4 both boundaries inclusive 

##Gives relation between usage and fitness for users of TM195
plot(TM195$Usage,TM195$Fitness,col = "red",main = "Usage vs Fitness comparision",
     abline(lm(TM195$Usage~TM195$Fitness)),cex = 1.3,pch = 16,xlab = "Usage",ylab = "Fitness")     
##High fitness levels are obtained with high usage and mid usage level has nealy all type of fitness level



#Data where product is TM498
TM498 <- myData[myData$Product == "TM498",]
TM498                                                 ##display  TM498
summary(TM498)                                        ##summary of TM498 usage

par(mfrow = c(1,3)) 

##Gives relation between age and usage for users of TM498
plot(TM498$Age,TM498$Usage,col = "blue",main = "Age vs Usage comparision",
     abline(lm(TM498$Age~TM498$Usage)),cex = 1.3,pch = 16,xlab = "Age",ylab = "Usage")
##More number of users are in age>25 and age<35 category where usage level is also more (3 and 4 ) Also in this  age range , usage level 2 and 4 are nearly evenly distributed

##Gives relation between gender and usage for users of TM498
plot(TM498$Gender,TM498$Usage,col = "blue",main = "Gender vs Usage comparision",
     abline(lm(TM498$Gender~TM498$Usage)),cex = 1.3,pch = 16,xlab = "Gender",ylab = "Usage")     
##All median and ditribution of usage level of male users of this product is around level 3 and female usage level is between 3 to 4 

##Gives relation between usage and fitness for users of TM498
plot(TM498$Usage,TM498$Fitness,col = "red",main = "Usage vs Fitness comparision",
     abline(lm(TM498$Usage~TM498$Fitness)),cex = 1.3,pch = 16,xlab = "Usage",ylab = "Fitness")     
##All level usage users are distributed across fitness level 2 to 4 with more people around level 3






#Data where product is TM798
TM798 <- myData[myData$Product == "TM798",]
TM798                                               ##display  TM798
summary(TM798)                                      ##summary of TM798 usage

par(mfrow = c(1,3)) 

##Gives relation between age and usage for users of TM798
plot(TM798$Age,TM798$Usage,col = "blue",main = "Age vs Usage comparision",
     abline(lm(TM798$Age~TM798$Usage)),cex = 1.3,pch = 16,xlab = "Age",ylab = "Usage")
##More number of users are in age>20 and age<35 category where usage level is symetrically distributed with top users of lv7 around age 28 . Users above 35 use lv3 and lv4 usage of this product

##Gives relation between gender and usage for users of TM798
plot(TM798$Gender,TM798$Usage,col = "blue",main = "Gender vs Usage comparision",
     abline(lm(TM798$Gender~TM798$Usage)),cex = 1.3,pch = 16,xlab = "Gender",ylab = "Usage")     
##Female users avg usage is  around lv5 distributed majorly between 4.5 to 5.5 where as male users lie between usage lv 4 to 5 with most of them at lv4

##Gives relation between usage and fitness for users of TM798
plot(TM798$Usage,TM798$Fitness,col = "red",main = "Usage vs Fitness comparision",
     abline(lm(TM798$Usage~TM798$Fitness)),cex = 1.3,pch = 16,xlab = "Usage",ylab = "Fitness")     
##Majority of the users are fully fit with usage in between 4 to 6




NumberofusersTM195 <- c(1:80)
NumberofusersTM498 <- c(1:60)
NumberofusersTM798 <- c(1:40)

par(mfrow = c(1,3))                                #plot of 1 X 3

##Comaprision of Usage,fitness and Education levels of TM195 users
boxplot(NumberofusersTM195,TM195$Education,type="o", col="blue", pch="o", lty=1, ylim = c(0,18),
     main = 'Usage to fitness and education comparision TM195',ylab = 'Usage,fitness and education')
points(NumberofusersTM195,TM195$Usage, col="red", pch="*")
lines(NumberofusersTM195, TM195$Usage, col="red",lty=2)
points(NumberofusersTM195,TM195$Fitness, col="black", pch="#")
lines(NumberofusersTM195, TM195$Fitness, col="black",lty=3)
##Usage is less than fitness by one level till a set of people and increases linearly to same end point

##Comaprision of Usage,fitness and Education levels of TM498 users
boxplot(NumberofusersTM498,TM498$Education,type="o", col="blue", pch="o", lty=1, ylim = c(0,18),
     main = 'Usage to fitness and education comparision TM498',ylab = 'Usage,fitness and education')
points(NumberofusersTM498,TM498$Usage, col="red", pch="*")
lines(NumberofusersTM498, TM498$Usage, col="red",lty=2)
points(NumberofusersTM498,TM498$Fitness, col="black", pch="#")
lines(NumberofusersTM498, TM498$Fitness, col="black",lty=3)
##First half of users are more educated than second half where as fitness is nearly constant completely and usage has a drop in second half of users but ends closely

##Comaprision of Usage,fitness and Education levels of TM798 users
boxplot(NumberofusersTM798,TM798$Education,type="o", col="blue", pch="o", lty=1, ylim = c(0,18),
     main = 'Usage to fitness and education comparision',ylab = 'Usage,fitness and education')
points(NumberofusersTM798,TM798$Usage, col="red", pch="*")
lines(NumberofusersTM798, TM798$Usage, col="red",lty=2)
points(NumberofusersTM798,TM798$Fitness, col="black", pch="#")
lines(NumberofusersTM798, TM798$Fitness, col="black",lty=3)
##First half is  scattered across level 10 to 15 whereas next half have more less distribution of education levels
##First set of people are able to improve fitness with decreasing usage also but second set of people fitness is constant with small scale improving usage




summary(TM195$Fitness)
summary(TM498$Fitness)
summary(TM798$Fitness)
##More fit people are prefering TM798

summary(TM195$Education)
summary(TM498$Education)
summary(TM798$Education)
##Highly educated people(16 to 21) prefer TM798

summary(TM195$Gender)
summary(TM498$Gender)
summary(TM798$Gender)
##TM195 is equally prefered by men and women , TM498 is also nearly equally prefered by men and women but TM798 is mostly prefered  by  men

summary(TM195$Age)
summary(TM498$Age)
summary(TM798$Age)
##All three products are nearly prefered by people of same age group (18 to 50) and TM798 has median age slightly more than others to

summary(TM195$MaritalStatus)
summary(TM498$MaritalStatus)
summary(TM798$MaritalStatus)
##Partnered to singles ratio of  users is around 3:2

summary(TM195$Income)
summary(TM498$Income)
summary(TM798$Income)
##TM195 and TM498 75 percent of users has less than 55k inncome but TM795 has most users whose income is above 55k

summary(TM195$Miles)
summary(TM498$Miles)
summary(TM798$Miles)
##TM195 and TM498 users run an avg of 85 miles/user but TM798 avg is 160 miles/user with max usage going up till 360 miles.
