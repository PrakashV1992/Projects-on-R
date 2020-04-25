##Study of CEO dataset

#set workking direactory
setwd("D:/Analytics/R/Dataset")
#get working direactory
getwd()


#read InternetMobileTime file into myData
library("readxl")
my_data1 <- read_excel("sample1-1.xlsx")
my_data1
my_data2 <- read_excel("sample2-1.xlsx")
my_data2

attach(my_data1)
attach(my_data2)

##Summary of both datasets
summary(my_data1)            ##Gives minimum, 1st quartile, mean, median, 3rd quartile and aximum values of compensation and ROI on 1st, 2nd, 3rd and 5th fiscal years for sample1
summary(my_data2)            ##Gives minimum, 1st quartile, mean, median, 3rd quartile and aximum values of compensation and ROI on 1st, 2nd, 3rd and 5th fiscal years for sample2 removing all outliers
##Gives brief summary of both datasets

range(my_data1$Compen)       ##Compensation of sample1 varies from 1.2 to 378
range(my_data2$Compen)       ##Compensation of sample1 varies from 12.1 to 77.6 removing compensation outlier values

par(mfrow = c(2,2))
##histogram plot of compansation frequency of different intervals
hist(my_data1$Compen,col = "RED", xlab = "Compensation", main = "Histogram of compensations of all companies")                  ##Histogram of compensation of all companies
hist(my_data2$Compen,col = "GREEN",xlab = "Compensation", main = "Histogram of compensations removing outliers")                ##Histogram of compensation removing outliers

##boxplot plot of compansation frequency of different intervals
boxplot(my_data1$Compen,col = "RED", xlab = "Compensation", main = "Histogram of compensations of all companies")               ##boxplot of compensation of all companies
boxplot(my_data2$Compen,col = "GREEN",xlab = "Compensation", main = "Histogram of compensations removing outliers")             ##boxplot of compensation removing outliers

meancompensationsample1 = mean(my_data1$Compen)
meancompensationsample2 = mean(my_data2$Compen)
sdcompensationsample1 = sd(my_data1$Compen)
sdcompensationsample2 = sd(my_data2$Compen)
meancompensationsample1
meancompensationsample2
sdcompensationsample1
sdcompensationsample2
##We could see mean is same in both the cases but standard deviation is nearly 3 times higher in sample1 with outliers
##Hence we can infer outliers in sample1 have a big impact

##Descriptive stats after first year returns. Save mean and standard deviation in variables
meanyear1returnsample1 = mean(my_data1$Y1Return)
meanyear1returnsample2 = mean(my_data2$Y1Return)
sdyear1returnsample1 = sd(my_data1$Y1Return)
sdyear1returnsample2 = sd(my_data2$Y1Return)
meanyear1returnsample1
meanyear1returnsample2
sdyear1returnsample1
sdyear1returnsample2

##Descriptive stats after second year returns. Save mean and standard deviation in variables
meanyear2returnsample1 = mean(my_data1$Y2Return)
meanyear2returnsample2 = mean(my_data2$Y2Return)
sdyear2returnsample1 = sd(my_data1$Y2Return)
sdyear2returnsample2 = sd(my_data2$Y2Return)
meanyear2returnsample1
meanyear2returnsample2
sdyear2returnsample1
sdyear2returnsample2

##Descriptive stats after third year returns. Save mean and standard deviation in variables
meanyear3returnsample1 = mean(my_data1$Y3Return)
meanyear3returnsample2 = mean(my_data2$Y3Return)
sdyear3returnsample1 = sd(my_data1$Y3Return)
sdyear3returnsample2 = sd(my_data2$Y3Return)
meanyear3returnsample1
meanyear3returnsample2
sdyear3returnsample1
sdyear3returnsample2

##Descriptive stats after fifth year returns. Save mean and standard deviation in variables
meanyear5returnsample1 = mean(my_data1$Y5Return)
meanyear5returnsample2 = mean(my_data2$Y5Return)
sdyear5returnsample1 = sd(my_data1$Y5Return)
sdyear5returnsample2 = sd(my_data2$Y5Return)
meanyear5returnsample1
meanyear5returnsample2
sdyear5returnsample1
sdyear5returnsample2

##From above standard devaition analysis we can figure out standard deviations 
##are slowly increasing from 26 to 38 gradually over five years


##Study corelation between compensation and ROI over five years for sample1 
cor(my_data1$Compen,my_data1$Y1Return)
cor(my_data1$Compen,my_data1$Y2Return)
cor(my_data1$Compen,my_data1$Y3Return)
cor(my_data1$Compen,my_data1$Y5Return)
##Above analysis show correlation between compensation and ROI increases with years 
##for sample1 from 14% to 36%

##Study corelation between compensation and ROI over five years for sample2 
cor(my_data2$Compen,my_data2$Y1Return)
cor(my_data2$Compen,my_data2$Y2Return)
cor(my_data2$Compen,my_data2$Y3Return)
cor(my_data2$Compen,my_data2$Y5Return)
##Above analysis show correlation between compensation and ROI increases signifacantly 
##with years for sample2 from 21% to 61%


##Classify sample2 on basis of groups
my_data2_grp1 = my_data2[my_data2$Groups == "G1",]
my_data2_grp2 = my_data2[my_data2$Groups == "G2",]
my_data2_grp3 = my_data2[my_data2$Groups == "G3",]
my_data2_grp4 = my_data2[my_data2$Groups == "G4",]
my_data2_grp5 = my_data2[my_data2$Groups == "G5",]
my_data2_grp6 = my_data2[my_data2$Groups == "G6",]
my_data2_grp1
my_data2_grp2
my_data2_grp3
my_data2_grp4
my_data2_grp5
my_data2_grp6

