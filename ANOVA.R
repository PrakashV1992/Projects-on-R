#Practice Problem 1

p_1996 <- c(30000, 34000, 36000, 38000, 40000)
p_1997 <- c(30000, 35000, 37000, 38000, 40000)
p_1998 <- c(40000, 41000, 43000, 44000, 50000)

all_data <- cbind(p_1996, p_1997, p_1998)
View(all_data)

library(reshape)
mdata <- melt(all_data)
View(mdata)
mdata <- mdata[,-c(1)]
colnames(mdata) <- c("Year", "Price")

anova_price <- aov(Price ~ Year, data = mdata)
summary(anova_price)  

price_diff <- TukeyHSD(anova_price)
price_diff

#Practice Problem 2

months_0 <- c(58.75, 57.94, 58.91, 56.85, 55.21, 57.30)
months_1 <- c(58.87, 56.43, 56.51, 57.67, 59.75, 58.48)
months_2 <- c(59.13, 60.38, 58.01, 59.95, 59.51, 60.34)
months_3 <- c(62.32, 58.76, 60.03, 59.36, 59.61, 61.95)

all_data2 <- cbind(months_0, months_1, months_2, months_3)
mdata1 <- melt(all_data2)
mdata1 <- mdata1[,-c(1)]
colnames(mdata1) <- c("Month", "Calcium")

anova_calcium <- aov(Calcium ~ Month, data = mdata1)
summary(anova_calcium)  

calcium_diff <- TukeyHSD(anova_calcium)
calcium_diff 

