setwd("D:/Analytics/Notes/Advance stats/Regression")

RKP <- read.csv("RK Puram_Delhi_PD.csv", header = TRUE)
View(RKP)

#Renaming columns
colnames(RKP)[colnames(RKP)=="BEN"] <- "Benzene"
colnames(RKP)[colnames(RKP)=="TOL"] <- "Toulene"
colnames(RKP)[colnames(RKP)=="PXY"] <- "P_Xylene"
colnames(RKP)[colnames(RKP)=="WD"] <- "WindDirection"
colnames(RKP)[colnames(RKP)=="AT"] <- "Temp"
colnames(RKP)[colnames(RKP)=="WS"] <- "WindSpeed"
colnames(RKP)[colnames(RKP)=="VWS"] <- "VerticalWindSpeed"
colnames(RKP)[colnames(RKP)=="SR"] <- "Solar"
colnames(RKP)[colnames(RKP)=="BP"] <- "BarPressure"

summary(RKP)
#Some columns have all blanks, they can be removed
RKP <- RKP[, -which(names(RKP)=="SPM")]
RKP <- RKP[, -which(names(RKP)=="Ethylben")]
RKP <- RKP[, -which(names(RKP)=="O_Xylene")]
RKP <- RKP[, -which(names(RKP)=="CH4")]
RKP <- RKP[, -which(names(RKP)=="NMHC")]
RKP <- RKP[, -which(names(RKP)=="THC")]

str(RKP)
#converting factor to numeric
RKP$NO <- as.numeric(as.character(RKP$NO))
RKP$CO <- as.numeric(as.character(RKP$CO))
RKP$NO2 <- as.numeric(as.character(RKP$NO2))
RKP$O3 <- as.numeric(as.character(RKP$O3))
RKP$SO2 <- as.numeric(as.character(RKP$SO2))
RKP$PM2.5 <- as.numeric(as.character(RKP$PM2.5))
RKP$Benzene <- as.numeric(as.character(RKP$Benzene))
RKP$Toulene <- as.numeric(as.character(RKP$Toulene))
RKP$P_Xylene <- as.numeric(as.character(RKP$P_Xylene))
RKP$NOx <- as.numeric(as.character(RKP$NOx))
RKP$PM10 <- as.numeric(as.character(RKP$PM10))
RKP$WindDirection <- as.numeric(as.character(RKP$WindDirection))
RKP$NH3 <- as.numeric(as.character(RKP$NH3))
RKP$RH <- as.numeric(as.character(RKP$RH))
RKP$Temp <- as.numeric(as.character(RKP$Temp))
RKP$WindSpeed <- as.numeric(as.character(RKP$WindSpeed))
RKP$VerticalWindSpeed <- as.numeric(as.character(RKP$VerticalWindSpeed))
RKP$Solar <- as.numeric(as.character(RKP$Solar))
RKP$BarPressure <- as.numeric(as.character(RKP$BarPressure))
RKP$PD_PM2.5 <- as.numeric(as.character(RKP$PD_PM2.5))
RKP$PD_PM10 <- as.numeric(as.character(RKP$PD_PM10))
RKP$PD_NO2 <- as.numeric(as.character(RKP$PD_NO2))
RKP$PD_SO2 <- as.numeric(as.character(RKP$PD_SO2))
RKP$PD_CO <- as.numeric(as.character(RKP$PD_CO))

#Omitting the rows with na
RKP <- na.omit(RKP)
row.names(RKP) <- 1:nrow(RKP)

#Check for missing values
sum(is.na(RKP$PM2.5))
sum(is.na(RKP$CO))

#Check for outliers

#Outlier detection
boxplot(RKP$NO)
summary(RKP$NO)
#IQR = Q3-Q1
#IQR*1.5

boxplot.stats(RKP$NO)$out

##NO
summary(RKP$NO)
quantile(RKP$NO, 0.75) #103.65
quantile(RKP$NO, 0.80) #123.26
quantile(RKP$NO, 0.85) #169.81
quantile(RKP$NO, 0.90) #208.5
quantile(RKP$NO, 0.95) #303.79
quantile(RKP$NO, 1.00) #414.95

##CO
summary(RKP$CO)
boxplot(RKP$CO)
quantile(RKP$CO, 0.75) #2.62
quantile(RKP$CO, 0.80) #2.89
quantile(RKP$CO, 0.85) #3.49
quantile(RKP$CO, 0.90) #4.24
quantile(RKP$CO, 0.95) #5.24
quantile(RKP$CO, 0.96) #5.42
quantile(RKP$CO, 0.97) #5.61
quantile(RKP$CO, 0.98) #5.78
quantile(RKP$CO, 0.99) #6.68
quantile(RKP$CO, 1.00) #19.9
#Capping
RKP$CO <- ifelse(RKP$CO > 6.7, 6.7, RKP$CO)
summary(RKP$CO)

##NO2
summary(RKP$NO2)
quantile(RKP$NO2, 0.75)
quantile(RKP$NO2, 0.80)
quantile(RKP$NO2, 0.85)
quantile(RKP$NO2, 0.90)
quantile(RKP$NO2, 0.95)
quantile(RKP$NO2, 1.00)

##O3
summary(RKP$O3)
quantile(RKP$O3, 0.75)
quantile(RKP$O3, 0.80)
quantile(RKP$O3, 0.85)
quantile(RKP$O3, 0.90)
quantile(RKP$O3, 0.95)
quantile(RKP$O3, 0.96)
quantile(RKP$O3, 0.97)
quantile(RKP$O3, 0.98)
quantile(RKP$O3, 0.99)
quantile(RKP$O3, 1.00)

##SO2
summary(RKP$SO2)
quantile(RKP$SO2, 0.80)
quantile(RKP$SO2, 0.85)
quantile(RKP$SO2, 0.90)
quantile(RKP$SO2, 0.95)
quantile(RKP$SO2, 0.96)
quantile(RKP$SO2, 0.97)
quantile(RKP$SO2, 0.98)
quantile(RKP$SO2, 0.99)
quantile(RKP$SO2, 1.00)
#Capping
RKP$SO2 <- ifelse(RKP$SO2 > 89.7, 89.7, RKP$SO2)
summary(RKP$SO2)

##PM2.5
summary(RKP$PM2.5)
quantile(RKP$PM2.5, 0.80)
quantile(RKP$PM2.5, 0.85)
quantile(RKP$PM2.5, 0.90)
quantile(RKP$PM2.5, 0.95)
quantile(RKP$PM2.5, 0.96)
quantile(RKP$PM2.5, 0.97)
quantile(RKP$PM2.5, 0.98)
quantile(RKP$PM2.5, 0.99)
quantile(RKP$PM2.5, 1.00)




library(moments)
library(forecast)
#Normality of Data - Box Cox test
##Transformations
#If lambda = -2.5 to -1.5, inverse square (1/y2)
#If lambda = -2.5 to -0.75, reciprocal (1/y)
#If lambda = -0.75 to -0.25, inverse square root (1/sqrt(y))
#If lambda = -0.25 to 0.25, natural log
#If lambda = 0.25 to 0.75, square root (sqrt(y))
#If lambda = 0.75 to 1.5, none
#If lambda = 1.5 to 2.5, square (y2)
##Variable 1: NO
qqnorm(RKP$NO, main="NO normality")
hist(RKP$NO, main="NO normality")
skewness(RKP$NO)
BoxCox.lambda(RKP$NO)
qqnorm(log(RKP$NO), main="NO normality")
hist(log(RKP$NO), main="NO normality")

##Variable 2: CO
qqnorm(RKP$CO, main="CO normality")
hist(RKP$CO, main="CO normality")
skewness(RKP$CO)
BoxCox.lambda(RKP$CO)
qqnorm((1/sqrt(RKP$CO)), main="CO normality")
hist((1/sqrt(RKP$CO)), main="CO normality")

##Variable 3: NO2
qqnorm(RKP$NO2, main="NO2 normality")
hist(RKP$NO2, main="NO2 normality")
skewness(RKP$NO2)

##Variable 4: O3
qqnorm(RKP$O3, main="O3 normality")
hist(RKP$O3, main="O3 normality")
skewness(RKP$O3)

##Variable 5: SO2
qqnorm(RKP$SO2, main="SO2 normality")
hist(RKP$SO2, main="SO2 normality")
skewness(RKP$NO)
BoxCox.lambda(RKP$SO2)
qqnorm(log(RKP$SO2), main="SO2 normality", ylim = c(0,10))
hist(log(RKP$SO2), main="SO2 normality")

##Variable 6: PM2.5
qqnorm(RKP$PM2.5, main="PM2.5 normality")
hist(RKP$PM2.5, main="PM2.5 normality")
skewness(RKP$PM2.5)
BoxCox.lambda(RKP$PM2.5)
qqnorm(log(RKP$PM2.5), main="PM2.5 normality")
hist(log(RKP$PM2.5), main="PM2.5 normality")

##Variable 7: Benzene
qqnorm(RKP$Benzene, main="Benzene normality")
hist(RKP$Benzene, main="Benzene normality")
skewness(RKP$Benzene)

##Variable 8: Toulene
qqnorm(RKP$Toulene, main="Toulene normality")
hist(RKP$Toulene, main="Toulene normality")
skewness(RKP$Toulene)

##Variable 9: P_Xylene
qqnorm(RKP$P_Xylene, main="P_Xylene normality")
hist(RKP$P_Xylene, main="P_Xylene normality")
skewness(RKP$P_Xylene)
BoxCox.lambda(RKP$P_Xylene)
qqnorm(log(RKP$P_Xylene), main="P_Xylene normality")
hist(log(RKP$P_Xylene), main="P_Xylene normality")

##Variable 10: NOx
qqnorm(RKP$NOx, main="NOx normality")
hist(RKP$NOx, main="NOx normality")
skewness(RKP$NOx)
BoxCox.lambda(RKP$NOx)
qqnorm(log(RKP$NOx), main="NOx normality")
hist(log(RKP$NOx), main="NOx normality")

##Variable 11: PM10
qqnorm(RKP$PM10, main="PM10 normality")
hist(RKP$PM10, main="PM10 normality")
skewness(RKP$PM10)

##Variable 12: WindDirection
qqnorm(RKP$WindDirection, main="WindDirection normality")
hist(RKP$WindDirection, main="WindDirection normality")
skewness(RKP$WindDirection)

##Variable 13: NH3
qqnorm(RKP$NH3, main="NH3 normality")
hist(RKP$NH3, main="NH3 normality")
skewness(RKP$NH3)
BoxCox.lambda(RKP$NH3)
qqnorm(sqrt(RKP$NH3), main="NH3 normality")
hist(sqrt(RKP$NH3), main="NH3 normality")

##Variable 14: RH
qqnorm(RKP$RH, main="RH normality")
hist(RKP$RH, main="RH normality")
skewness(RKP$RH)

##Variable 15: Temp
qqnorm(RKP$Temp, main="Temp normality")
hist(RKP$Temp, main="Temp normality")
skewness(RKP$Temp)

##Variable 16: WindSpeed
qqnorm(RKP$WindSpeed, main="WindSpeed normality")
hist(RKP$WindSpeed, main="WindSpeed normality")
skewness(RKP$WindSpeed)

##Variable 17: VerticalWindSpeed
qqnorm(RKP$VerticalWindSpeed, main="VerticalWindSpeed normality")
hist(RKP$VerticalWindSpeed, main="VerticalWindSpeed normality")
skewness(RKP$VerticalWindSpeed)

##Variable 18: Solar Radiation
qqnorm(RKP$Solar, main="Solar normality")
hist(RKP$Solar, main="Solar normality")
skewness(RKP$Solar)

##Variable 19: BarPressure
qqnorm(RKP$BarPressure, main="BarPressure normality")
hist(RKP$BarPressure, main="BarPressure normality")
skewness(RKP$BarPressure)
BoxCox.lambda(RKP$BarPressure)
qqnorm((RKP$BarPressure)^4, main="BarPressure normality")
hist((RKP$BarPressure)^4, main="BarPressure normality")

##Variable 20: PD_PM2.5
qqnorm(RKP$PD_PM2.5, main="PD_PM2.5 normality")
hist(RKP$PD_PM2.5, main="PD_PM2.5 normality")
skewness(RKP$PD_PM2.5)
BoxCox.lambda(RKP$PD_PM2.5)
qqnorm(log(RKP$PD_PM2.5), main="PD_PM2.5 normality")
hist(log(RKP$PD_PM2.5), main="PD_PM2.5 normality")

##Variable 21: PD_PM10
qqnorm(RKP$PD_PM10, main="PD_PM10 normality")
hist(RKP$PD_PM10, main="PD_PM10 normality")
skewness(RKP$PD_PM10)

##Variable 22: PD_NO2
qqnorm(RKP$PD_NO2, main="PD_NO2 normality")
hist(RKP$PD_NO2, main="PD_NO2 normality")
skewness(RKP$PD_NO2)

##Variable 23: PD_SO2
qqnorm(RKP$PD_SO2, main="PD_SO2 normality")
hist(RKP$PD_SO2, main="PD_SO2 normality")
skewness(RKP$PD_SO2)
BoxCox.lambda(RKP$PD_SO2)
qqnorm(log(RKP$PD_SO2), main="PD_SO2 normality", ylim=c(0,5))
hist(log(RKP$PD_SO2), main="PD_SO2 normality")

##Variable 24: PD_CO
qqnorm(RKP$PD_CO, main="PD_CO normality")
hist(RKP$PD_CO, main="PD_CO normality")
skewness(RKP$PD_CO)
BoxCox.lambda(RKP$PD_CO)
qqnorm((1/sqrt(RKP$PD_CO)), main="PD_CO normality")
hist((1/sqrt(RKP$PD_CO)), main="PD_CO normality")

#Linear relationship
plot(RKP$PM2.5, RKP$NO)
plot(RKP$PM2.5, RKP$CO)

#Linear relationship b/w dependent and independent variables
plot(log(RKP$PM2.5), log(RKP$NO))
plot(log(RKP$PM2.5), 1/sqrt(RKP$CO))

#Linear Regression
attach(RKP)
mod <- lm(log(PM2.5) ~ log(NO) + 1/sqrt(CO) + Toulene + WindDirection + sqrt(NH3) + RH + Temp + WindSpeed)
summary(mod)
#PM2.5 <- 4.34 + 0.062*log(NO) + 0.008*Toulene + ....


mod1 <- lm(log(PM2.5) ~ log(NO))
summary(mod1)

mod <- lm(log(PM2.5) ~ log(NO) + 1/sqrt(CO))
summary(mod2)

mod_withold <- lm(log(PM2.5) ~ log(NO) + 1/sqrt(CO)  + Toulene + WindDirection + sqrt(NH3) + RH + WindSpeed + PD_PM2.5)
summary(mod_withold)

#Performance 
##VIF
library(car)
vif(mod)
vif(mod_withold)

##Plots
plot(mod)
plot(mod_withold)

#Correlation b/w Residuals and explanatory variables
cor.test(RKP$NO, mod$residuals)

#Homoscedasticity
##Constant error variance
ncvTest(mod)
spreadLevelPlot(mod)

###################
#More things to do#
###################
#Outlier detection
boxplot.stats(RKP$NO)$out
#Replace outliers 

#Dev and test sample
#Generating n random numbers b/w 0 and 1
#Add these randomly generated numbers to the data as a new column
#Splitting the data into dev and testing sample based on the random number
df.dev <- df[which(df$random <= 0.7),]
df.val <- RF[which(df$random > 0.7),]

#Predict values in validation sample using model
predicted=predict(model,newdata=validation_sample)
#RMSE calculation
