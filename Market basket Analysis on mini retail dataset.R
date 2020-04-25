###Market Basket Analysis in R using small retail data set
setwd("D:/Analytics/R/Dataset/Machine Learning/week1")
Data<- read.csv("MARKET_BASKET_ANALYSIS.csv")
View(Data)

###Now columns in picture which can be used for MBA analaysis is Invoice number 
#  or billnumber and Item description(List of items)
###Since invoice number is repeated along the data set, we have to make it unique
#  and hence all items descriptions comes together in sam row

###So we first convert invoice number variable to categoric
Data$Invoice_No<- as.factor(Data$Invoice_No)
###Now include all items into same row of respective invoice number
?split
###split() classifies Item Desc into groups based on InvoiceNumber 
Agg.Data<- split(Data$Item_Desc,Data$Invoice_No)
###split functionality converts above into list of 415 invoice numbers
class(Agg.Data)
View(Agg.Data)
Agg.Data[1]

###Clean Item desc data removing deplicates which are repeated using "arules"
install.packages("arules")
library(arules)

###Below is logic to remove duplicates from list generally
Agg.Data_DD <- list()
for (i in 1:length(Agg.Data)) {
  Agg.Data_DD[[i]] <- as.character(Agg.Data[[i]][!duplicated(Agg.Data[[i]])])
}

###Next we convert this list to transactions format
Txns<- as(Agg.Data_DD,"transactions")
Txns
summary(Txns)
###Summary gives total number of transactions and products(as columns nums)
#  Items whose count in invoices are max
###element (itemset/transaction) length distribution: count of transactions 
#  with 1 item in invoice is 79, 2 items is 67 and similarlly 69 items is 1
###Box plot values of count for no: of items in transactions


###Next we study and understand SUPPORT parameter of each products
freq<- itemFrequency(Txns)
class(freq)
freq<- freq[order(-freq)]
freq
#  freq holds decimal representations of %values of support i.e. breads and 
#  fruitjuices are available on 21.6% and 16.8% of transactions respectively 
###To see freq of particular product "Appalams"
freq["Appalams"]

###BASIC PLOTS
barplot(freq[1:15])
?itemFrequencyPlot
itemFrequencyPlot(Txns,support=0.1)
itemFrequencyPlot(Txns,topN=10)

###Study the Associations between the products
install.packages("arulesViz")
library(arulesViz)
?apriori
###Build rules1 with default parameters(conf<-0.8 and support<-0.1)
rules1<- apriori(data= Txns)
###To view rules created above
inspect(rules1)
###Algorithm has formulated 4 rules on basic conditions
#lhs                    rhs              support   confidence lift     count
#[1] {Butter}            => {Bread}          0.1036145 0.8431373  3.887800 43   
#[2] {Breakfast Cereals} => {Bread}          0.1060241 0.8148148  3.757202 44   
#[3] {Dahi}              => {Cut Vegetables} 0.1180723 0.9800000  6.893220 49   
#[4] {Cut Vegetables}    => {Dahi}           0.1180723 0.8305085  6.893220 49 
###Rule is given customer buys lhs product, parameters for cust also buys rhs product
#  Joint support, rule confidence and lift values for respective rules
#  Count is no of transactions supporting the rule

###To sort the rules based on conf and lift values
rules1.conf<- sort(rules1,by= "confidence" ,decreasing= TRUE)
inspect(rules1.conf)
rules1.lift<- sort(rules1,by= "lift" ,decreasing = TRUE)
inspect(rules1.lift)
###Confidence signifies the strength of the rule and is direactional
###lift signifies % increase in sales of itemB given cust has bought itemA
#  but irrespective of direaction of the rule
###High lift ratio can be used for clubbing the products together


###Pass conf and support parameters to be considering when forming rules
rules2<- apriori(data= Txns, parameter= list(conf= 0.5, supp= 0.05, maxlen=2))
length(rules2)   ###152 rules for maxlen=2
###View top 30 rules of the rules2 by descending order of support
inspect(sort(rules2,by= "support")[1:30])
###View rules for given customers buy FruitJuice
rules2.Juice= apriori(data=Txns,parameter= list(conf= 0.5, supp= 0.05, maxlen=2)
                      ,appearance= list(default="rhs",lhs="Fruit Juices"))
inspect(rules2.Juice)

###Pass conf and support parameters to be considering when forming rules with length=3
rules3<- apriori(data= Txns, parameter= list(conf= 0.5, supp= 0.05, maxlen=3))
length(rules3)  ###496 rules for maxlen=3
###See bottom 20 rules for 3 items association
inspect(sort(rules3,by= "support")[476:496])
###When maxlen increases, to remove redundant rules i.e. subset rules to larger rules
subset.rules3<- which(colSums(is.subset(rules3,rules3)) > 1)
length(subset.rules3)   ###436 subsets rules
rules3.clean<- rules3[-subset.rules3]
###rules3.clean will have duplicate rows removed rules

###Coming back rules2, how can we fetch these framed rules as EXCEL o/p
rules2_df<- as(rules2,"data.frame")
###Add lhs and rhs support values for better study wrt to sales of the items
rules2_df$lhs_support<- rules2_df$support / rules2_df$confidence
rules2_df$rhs_support<- rules2_df$confidence / rules2_df$lift
View(rules2_df)

getwd()
###Sorting above view in desc order of lhs support gives rules wrt desc order of sales
write.table(rules2_df,file= "MBA Rules output.csv", sep= ",", append= F, row.names= F)
unlink("MBA Rules output.csv")
