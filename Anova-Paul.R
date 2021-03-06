setwd("G:/")
mydata=read.csv("newfood-paul.csv", header=TRUE)
attach(mydata)
names(mydata)
Sales=Sales1+Sales2+Sales3
Price=factor(PriceLevel, levels=c(1,2,3),labels=c("LowPrice", "MediumPrice", "HighPrice"))
Advertisement=factor(AdLevel,levels=c(1,2),labels=c("LowAd", "HighAd"))
ANOVAMain=aov(Sales~Price+Advertisement)
summary(ANOVAMain)
interaction.plot(Price,Advertisement,Sales, col=c("Red","Blue"),main="Interaction between Price and Advertisement")
ANOVAInter=aov(Sales~Price+Advertisement+Price*Advertisement)
summary(ANOVAInter)
ANCOVA=aov(Sales~StoreSize+Price+Advertisement+Price*Advertisement)
summary(ANCOVA)
library(car)
Anova(ANCOVA,type="II")

