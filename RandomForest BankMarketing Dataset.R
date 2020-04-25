###Random forest analysis on marketting dataset
setwd("D:/Analytics/R/Dataset/Machine Learning/week2")
getwd()

Data.dev <- read.csv("DEV_SAMPLE.csv")           ###Training dataset
Data.holdout <- read.csv("HOLDOUT_SAMPLE.csv")   ###Holdout dataset
attach(Data.dev)

library(randomForest)
?randomForest
RF1<- randomForest(as.factor(Target)~. ,data= Data.dev[-1], ntree=100, mtry=4, nodesize=10, 
                  importance=TRUE)
print(RF1)
plot(RF1, main = "Random forest 1 OOB plot") + legend("topright", c("OOB","0","1"), text.col=c(1:6)
                                                    ,lty=c(1:3), col=c(1:3))
###legend part getting error but key is populated in top right of graph
###Inferences from graph includes, we see OOB err is decreasing till a pt and stablizes
###Class0 error rate also decreases till a point and stabilizes
###Class1 error rate increases as expected till and extent and again saturates
###We have to find the ideal value of No: of tress(xaxis) where all 3 err stablizes without any further change

##Random forest1 Error rate
RF1$err.rate
###Gives a list of values plotted in above graph(Error for OOB, class0 and class1)
###these numerical values helps identify the point of stabilization in all 3 lines
###According to OOB cureve, Err rate stabilizes around no:of trees ~(60 to 70) at 0.083/0.082
###From class0 values, Err rate ~0.0054 is achieved around ntrees(90 to 100)
###From class1 values, Err rate increases and hits 0.8850 around 80 tress.
###We can consider a ideal value of 89 trees which is best for all 3 lines

impvar1= round(RF1$importance,2)
impvar2= round(randomForest::importance(RF1), 2)
###impvar1 holds the scaled value of Mean decrease accuracy but signifies importance similar
#  as impvar2. Mean decrease in gini is same across both the variables. 
###From Accuracy perspective, importance of variable in decreasing order goes like Occupation,
#  NoCrTxns,HoldPrd,Gender,Balance,Age. Score is the least significant variable by accuracy
###From Decrease in Gini index perspective, Balance,Score,HoldPrd,NoCrTxns in decreasing order.
#  Age,Occupations and Gender turns to beinsignificant variables. 

View(Data.dev)
###To find optimum number of mtry(variabes to be included) using tuning
tRF1 <- tuneRF(x= Data.dev[-c(1,2)], y= as.factor(Data.dev$Target), mtryStart=2, ntreeTry= 100
              , stepFactor= 1.5, improve=0.0001, trace= TRUE, plot= TRUE, doBest=TRUE, 
              nodesize=100, importance= TRUE)
tRF1$importance

?randomForest
RF2<- randomForest(as.factor(Target)~. ,data= Data.dev[-1], ntree=100, mtry=4, nodesize=100, 
                   importance=TRUE)
plot(RF2)
print(RF2)
###No big differences between classification of responders/non resp
###Even though classfication error is ~8.5% only since our data has only 9% of 
#  of responders and since it is not balanced, classifying responders is very poor

###Prediction
Data.dev$Predict.class= predict(RF2, Data.dev, type = "class")
Data.dev$Predict.prob= predict(RF2, Data.dev, type= "prob")
View(Data.dev)
###RANK ORDERING

###Step1: Deciling
## deciling
## deciling code
decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}

Data.dev$decile <- decile(Data.dev$Predict.prob[,2])
attach(Data.dev)

###Group by decile and add rankorder parameters
library(data.table)
tmp_DT1 = data.table(Data.dev)
rank1 <- tmp_DT1[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=decile][order(-decile)]
rank1$rrate <- round (rank1$cnt_resp / rank1$cnt,2);
rank1$cum_resp <- cumsum(rank1$cnt_resp)
rank1$cum_non_resp <- cumsum(rank1$cnt_non_resp)
rank1$cum_rel_resp <- round(rank1$cum_resp / sum(rank1$cnt_resp),2);
rank1$cum_rel_non_resp <- round(rank1$cum_non_resp / sum(rank1$cnt_non_resp),2);
rank1$ks <- abs(rank1$cum_rel_resp - rank1$cum_rel_non_resp);

library(scales)
rank1$rrate <- percent(rank1$rrate)
rank1$cum_rel_resp <- percent(rank1$cum_rel_resp)
rank1$cum_rel_non_resp <- percent(rank1$cum_rel_non_resp)

View(rank1)
###We saw model built with RF1(mtry=3, nodesize=10) is OVERFIT with ks is 93% 
###In above created rank1 matrix we segregate 93% of classified responders
#  in 2 out of 3deciles ie in 1148 resp in 4347 customers

library(ROCR)  ##For using prediction function
pred1= prediction(Data.dev$Predict.prob[,2],Data.dev$Target)
perf1= performance(pred1,"tpr","fpr")
plot(perf1)
ks1= max( attr(perf1,'y.values')[[1]] - attr(perf1,'x.values')[[1]])  ##0.6866206
auc1= performance(pred1,"auc")
auc1= as.numeric(auc1@y.values)   ###0.8939378

library(ineq)
gini = ineq(Data.dev$Predict.prob[,2], type="Gini")
gini    
###Crazy high gini 0.9023827

## Classification Error
sum(Target)
with(Data.dev, table(Target, Predict.class))
###Classification Error is (34+1114)/(14000) is 8.2% again system can classified
#  responders correct rate is very less (121 out of 1235 responders)
###WE SEE SIMILAR DRAWBACK LIKE CART ALGORITHM AS MODEL CANNOT CLASSIFY RESP
#  CORRECTLY AS DATA IS NOT BALANCED. SO WE MAKE DATA BALANCED BY OVERSAMPLING
#  OR UNDER SAMPLING 


###==============###
###DATA BALANCING###
###================================###
###OVER SAMPLING AND UNDER SAMPLING###
###================================###
library("ROSE")
?ovun.sample()
setwd("D:/Analytics/R/Dataset/Machine Learning/week2")
getwd()

Data.dev.balanced <- read.csv("DEV_SAMPLE.csv")           ###Training dataset
attach(Data.dev.balanced)
?ovun.sample
Data.dev.balanced1 <- ovun.sample(Target~ ., 
                      Data.dev.balanced, method= "both", N = 14000, p = 0.35)
Data.dev.balanced2 <- ovun.sample(Target~ .,
                      Data.dev.balanced, method= "both", N = 14000, p = 0.5)

Bal.RF1<- randomForest(as.factor(Target)~ Age+ Gender+ Balance+ Occupation+
                         No_OF_CR_TXNS+ SCR+ Holding_Period ,data= Data.dev.balanced1[-1], 
                       ntree=89, mtry=4, nodesize=10, importance=TRUE)
print(Bal.RF1)
plot(Bal.RF1, main = "Random forest 1 OOB plot") + legend("topright", c("OOB","0","1"), text.col=c(1:6)
                                                      ,lty=c(1:3), col=c(1:3))

Bal.RF2<- randomForest(as.factor(Target)~ Age+ Gender+ Balance+ Occupation+
                         No_OF_CR_TXNS+ SCR+ Holding_Period ,data= Data.dev.balanced2[-1], 
                       ntree=89, mtry=4, nodesize=10, importance=TRUE)
print(Bal.RF2)
plot(Bal.RF2, main = "Random forest 1 OOB plot") + legend("topright", c("OOB","0","1"), text.col=c(1:6)
                                                          ,lty=c(1:3), col=c(1:3))

