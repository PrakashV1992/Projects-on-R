#loading 
getwd()
setwd("D:/Datasets")
golf_df <- read.csv("golf.csv", header = TRUE)
attach(golf_df)

#descriptive statistics
summary(golf_df)
var(golf_df$Current)
var(golf_df$New)
sd(golf_df$Current)
sd(golf_df$New)

#box plot
#shows no significant difference in distribution of data points

#ANS 5: from the box plot it is evident that both the samples have different median value per se,
#increase in sample size with respect to similar distribution between sample,
#may lead to variablity in p-value

boxplot(golf_df$Current,golf_df$New, data = golf_df, 
        main= "boxplot between distance covered by current and new golf ball" ,
        col="green")

#Paired T test for 2 samples - showing before and after relation
#p-value > 0.05 | accept null hypothesis and reject alternate hypothesis | p-value:0.2092
t.test(golf_df$Current, golf_df$New, paired = TRUE)

?t.test
?boxplot


