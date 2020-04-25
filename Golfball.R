setwd("D:/Analytics/R/Dataset/ANOVA")
mydata=read.csv("golfball.csv", header=TRUE)
mydata
attach(mydata)
library(psych)
describeBy(Distance,group=Design)
library(lattice)
histogram(~Distance|Design)
boxplot(Distance~Design,horizontal=TRUE,col=c("Red","Blue","Orange","Pink"))
Model=aov(Distance~Design,data=mydata)
Model
summary(Model)
TukeyHSD(Model)
