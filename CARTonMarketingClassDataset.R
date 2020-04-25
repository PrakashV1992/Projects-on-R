###CART analysis on marketing classification dataset

setwd("D:/Analytics/R/Dataset/Machine Learning/week2")
getwd()

##libraries needed
##Neccessary packages for CART classification
library(rpart)
library(rpart.plot)
library(rattle)     ###For using fancy rplot()
library(data.table)
library(scales)
###Above data.table and scales packages are needed for using groupby and orderby while creating rank ordered matrix
library(ROCR)
library(ineq)
##ROCR and ineq are used for prediction() and perf() in calc auc and ks mathematically 
library("ROSE")  ###For over and under sampling or both

##Read sample, dev and holdout samples in to data variables
Data.sample <- read.csv("CF_TREE_SAMPLE.csv")    ###Sample dataset
Data.dev <- read.csv("DEV_SAMPLE.csv")           ###Training dataset
Data.holdout <- read.csv("HOLDOUT_SAMPLE.csv")   ###Holdout dataset

nrow(Data.dev)            ##14000
nrow(Data.holdout)        ## 6000
nrow(Data.sample)         ##20000

str(Data.dev)             
str(Data.holdout)
###All variables are already in right types(factors/numeric)
###Target variable is numeric should be converted to factor 

##Compute response rate of Target in all three samples 
rrate_sample=sum(Data.sample$Target)/20000         ##8.665%
rrate_dev=sum(Data.dev$Target)/14000               ##8.821%      
rrate_holdout=sum(Data.holdout$Target)/6000        ##8.3 %

#Data.sample$Target <- as.factor(Data.sample$Target) 
#Data.dev$Target <- as.factor(Data.dev$Target) 
#Data.holdout$Target <- as.factor(Data.holdout$Target) 

summary(Data.sample)
summary(Data.dev)
summary(Data.holdout)
###Target is distributed in 1:10 ratio between class1:class0 in all three samples
###We have both charecteristic(Age,Gender,Occupation) and demographic(Balance,NoofCRTrans,SCR,
#  HoldingPeriod) along with Target class variables about 20000 customers of a Bank for a prod.


?rpart          ##Function to plot fit rpart model
?rpart.control  ##Function to pass control parameters of rplot tree algorithm

###==========================================================###
###CREATING MULTIPLE MODELS WITH DIFFERENT CONTROL PARAMETERS###
###==========================================================###

###MODEL1###
############
control1= rpart.control(minsplit=30, minbucket=10, cp=0, xval=10)
###Min no of obs for a split to happen = 100
###Min no of obs to be available in last leaf node
###cp is the cost improvement parameter where algorithm checks for systems main 
#  evaluation parameter to increase by minimum of cp value
###xval is no: of cross validations subsets data should be split into 
M1=rpart(formula= Target~., data=Data.dev[-1], method="class", control=control1)
M1

fancyRpartPlot(M1)
###Better view of M1 
###Read each model like node# at top
###Each body node top to bottom as Majorityclass, prob(y=0,y=1), % of tot obs in node
###Condn for split below node; YES and NO direactions follows as top node through out the tree
printcp(M1)
###rows for splits were Cp/classification error decreases are only included
###relative Err is 1 for nsplit=0 i.e. Error at rootnode is 8.8%(% of min class i.e 1)
###Successive Relative errors are relative errors compared to 8.8% as 1
###xerror is cross validation predicted Error at node level
###CP = (RelErr1 - RelErr2)/nsplit
plotcp(M1)
###Unprunned plot of M1 shows minimal Xerror(0.97409) at nodesplit=17 with cp=0.00215
##pruned tree from M1
?prune
ptreeM1= prune(M1, cp=0.00215, "cp")

###MODEL2###
############
###Increasing minsplit to 100 instead of 30
control2=rpart.control(minsplit= 100, minbucket=10, cp=0, xval=10)
M2=rpart(formula= Target~., data=Data.dev[-1], method="class", control=control2)
M2
fancyRpartPlot(M2)
###Decreases count of node splits and leaf nodes
printcp(M2)
plotcp(M2)
###Minimum cross validation error is 0.97814 and cp is 0.00202
##pruned tree from M1
?prune
ptreeM2= prune(M2, cp=0.00202, "cp")
###From cross validation Errors Model1 is slightly better than Model2

###MODEL3###
############
###Increasing minsplit to 400 instead of 150(IDEAL VALUES IN LARGE DATASET)
#  Ideal values of min bucket(min no: of observations on leaf nodes) is 1% or 2% of sample size
#  So we takemin bucket as 150 and minsplit~ minbucket*3. So we take minsplit ~ 400
#  Ideal values of cross validation segregations is spliting data into 10 parts
control3=rpart.control(minsplit= 300, minbucket=150, cp=0, xval=10)
nrow(Data.dev[,-1])
M3=rpart(formula= Target~., data=Data.dev[,-1], method="class", control=control3)
M3
fancyRpartPlot(M3)
###Decreases count of node splits and leaf nodes
printcp(M3)
plotcp(M3)
###Only few sub node spliting is happening and hence model is already prunned


###PREDICTIONS ON DEVDATA SAMPLES USING MODELS CREATED###
###MODEL1 PREDICTIONS
##We predict class and probabilities for our dev data itself saved in two new added columns in Dev dataset 
Data.dev$predict.classM1 = predict(ptreeM1, Data.dev, type= "class")
Data.dev$predict.scoreM1 = predict(ptreeM1, Data.dev, type= "prob")
Data.dev$predict.scoreM1 
###Has probabilities of both class to be 0 and 1 for each observation

###MODEL2 PREDICTIONS
##We predict class and probabilities for our dev data itself saved in two new added columns in Dev dataset 
Data.dev$predict.classM2 = predict(ptreeM2, Data.dev, type= "class")
Data.dev$predict.scoreM2 = predict(ptreeM2, Data.dev, type= "prob")
###Has probabilities of both class to be 0 and 1 for each observation

###MODEL3 PREDICTIONS
##We predict class and probabilities for our dev data itself saved in two new added columns in Dev dataset 
Data.dev$predict.classM3 = predict(M3, Data.dev, type= "class")
Data.dev$predict.scoreM3 = predict(M3, Data.dev, type= "prob")
###Has probabilities of both class to be 0 and 1 for each observation



###================###
###MODEL EVALUATION###
###================###

##STEP1: Sorting based on prob(y)and deciling into 10 deciles
##Below is generic function for deciling
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

##Deciling using all 3 Model probabilities of target=1
Data.dev$decileM1 = decile(Data.dev$predict.scoreM1[,2])
Data.dev$decileM2 = decile(Data.dev$predict.scoreM2[,2])
Data.dev$decileM3 = decile(Data.dev$predict.scoreM3[,2])
###decileM# column is added as a new column to dev dataset


##STEP2: Creating rank ordering matrix

tmp_DT= data.table(Data.dev)
rank1= tmp_DT[,list(count=length(Target), count_resp=sum(Target), 
                    count_non_resp=sum(Target==0)), by= decileM1][order(-decileM1)]
rank2= tmp_DT[,list(count=length(Target), count_resp=sum(Target), 
                    count_non_resp=sum(Target==0)), by= decileM2][order(-decileM2)]
rank3= tmp_DT[,list(count=length(Target), count_resp=sum(Target), 
                    count_non_resp=sum(Target==0)), by= decileM3][order(-decileM3)]
View(Data.dev)


##STEP3: Add other additional columns to ranked matrixes
###MODEL1 Evaluation parameters:::KS
rank1$rrate= round( rank1$count_resp/rank1$count, 4)
rank1$cum_resp= cumsum(rank1$count_resp)
rank1$cum_non_resp= cumsum(rank1$count_non_resp)
rank1$cum_rel_resp=round( rank1$count_resp/sum(rank1$count_resp), 4)
rank1$cum_rel_non_resp=round( rank1$count_non_resp/sum(rank1$count_non_resp), 4)
rank1$ks= abs(rank1$cum_rel_non_resp - rank1$cum_rel_resp)
rank1$rrate= percent(rank1$rrate)
rank1$cum_rel_resp= percent(rank1$cum_rel_resp)
rank1$cum_rel_non_resp= percent(rank1$cum_rel_non_resp)
rank1$ks= percent(rank1$ks)
View(rank1)

###MODEL2 Evaluation parameters
rank2$rrate= round( rank2$count_resp/rank2$count, 4)
rank2$cum_resp= cumsum(rank2$count_resp)
rank2$cum_non_resp= cumsum(rank2$count_non_resp)
rank2$cum_rel_resp=round( rank2$count_resp/sum(rank2$count_resp), 4)
rank2$cum_rel_non_resp=round( rank2$count_non_resp/sum(rank2$count_non_resp), 4)
rank2$ks= abs(rank2$cum_rel_non_resp - rank2$cum_rel_resp)
rank2$rrate= percent(rank2$rrate)
rank2$cum_rel_resp= percent(rank2$cum_rel_resp)
rank2$cum_rel_non_resp= percent(rank2$cum_rel_non_resp)
rank2$ks= percent(rank2$ks)
View(rank2)

###MODEL3 Evaluation parameters
rank3$rrate= round( rank3$count_resp/rank3$count, 4)
rank3$cum_resp= cumsum(rank3$count_resp)
rank3$cum_non_resp= cumsum(rank3$count_non_resp)
rank3$cum_rel_resp=round( rank3$count_resp/sum(rank3$count_resp), 4)
rank3$cum_rel_non_resp=round( rank3$count_non_resp/sum(rank3$count_non_resp), 4)
rank3$ks= abs(rank3$cum_rel_non_resp - rank3$cum_rel_resp)
rank3$rrate= percent(rank3$rrate)
rank3$cum_rel_resp= percent(rank3$cum_rel_resp)
rank3$cum_rel_non_resp= percent(rank3$cum_rel_non_resp)
rank3$ks= percent(rank3$ks)
View(rank3)

###From inferential POV, we expect minimum of 40% ks from model which is not achieved 
#  in either of the 3 models. Also we got only 2/3 deciles in our classification 
#  because our Taget variable is not balanced. (no: of y=1) << (no: of y=0)
###Important aspect in CART algorithm is target distribution should be balanced
###Although not satisfied by ks, we again continue evaluation with other parameters

##STEP4:Other evaluation parameters(auc and ks mathematically) 
###KS and auc parameters are calc based on prob of predicted target=1

###Model1
pred1= prediction(Data.dev$predict.scoreM1[,2],Data.dev$Target)
perf1= performance(pred1,"tpr","fpr")
plot(perf1)
ks1= max( attr(perf1,'y.values')[[1]] - attr(perf1,'x.values')[[1]])  ##0.3261106
auc1= performance(pred1,"auc")
auc1= as.numeric(auc1@y.values)                                       ##0.6951257

###Model2
pred2= prediction(Data.dev$predict.scoreM2[,2],Data.dev$Target)
perf2= performance(pred2,"tpr","fpr")
plot(perf2)
ks2= max( attr(perf2,'y.values')[[1]] - attr(perf2,'x.values')[[1]])  ##0.3259019
auc2= performance(pred2,"auc")
auc2= as.numeric(auc2@y.values)                                       ##0.6936569

###Model3
pred3= prediction(Data.dev$predict.scoreM3[,2],Data.dev$Target)
perf3= performance(pred3,"tpr","fpr")
plot(perf3)
ks3= max( attr(perf3,'y.values')[[1]] - attr(perf3,'x.values')[[1]])  ##0.3259019
auc3= performance(pred3,"auc")
auc3= as.numeric(auc3@y.values)                                       ##0.6807706
###Higher area under roc curve(auc), better is Model
###There are no much differences between ks or auc values in our Models


##STEP5: GINI index
###Gini calc are also based on prob(predicted target by model==1)
?ineq
GINI1=ineq(Data.dev$predict.scoreM1[,2],type= "Gini")  ##0.3558
GINI2=ineq(Data.dev$predict.scoreM2[,2],type= "Gini")  ##0.3531
GINI3=ineq(Data.dev$predict.scoreM3[,2],type= "Gini")  ##0.3296
###Again Model Gini values are less than expected(0.4 to 0.6)

##STEP6:Classification matrix and classfication Error
?with
CM1= with(Data.dev,table(Data.dev$Target,Data.dev$predict.classM1))
CM2= with(Data.dev,table(Data.dev$Target,Data.dev$predict.classM2))
CM3= with(Data.dev,table(Data.dev$Target,Data.dev$predict.classM3))
##CE(Classification error is no of wrong predictions by total number of observations)
CE1= (47+1081)/(14000)   ##8.05%
CE2= (88+1078)/(14000)   ##8.32%
CE3= (64+1149)/(14000)   ##8.66%
###CE's are nearly same across models but we see all three models predict class1 very poorly

###Also above evaluations suggests Model trees are not overfitted but we don't see 
#  parameter values satisfying expected limits. So we are not predicting classes and 
#  prob's holdout records using these Models. As part of improvement we try over
#  sampling and under sampling of our data sets and form model trees



###===========================================================================###
###===========================================================================###
###===========================================================================###


###================================###
###OVER SAMPLING AND UNDER SAMPLING###
###================================###
#install.packages("ROSE")
?ovun.sample()
setwd("D:/Analytics/R/Dataset/Machine Learning/week2")
getwd()

###OVersampling andunder sampling using ovun.sample() function
Balanced.Data.dev <- read.csv("DEV_SAMPLE.csv") 
Balanced.Data.dev1=ovun.sample(Target~., Balanced.Data.dev,N=14000,p=0.5,method= "both")$data
Balanced.Data.dev2=ovun.sample(Target~., Balanced.Data.dev,N=28000,p=0.5,method= "both")$data
###ovun.sample(...)$data gives the  tabular format of dataset stored to above variables
#View(Balanced.Data.dev1)
#View(Balanced.Data.dev2)
###when method is given both, algorithm does both over and under sampling
###Few class0 records are removed and class1 records are duplicated multiple times to make the
#  dataset balanced. Note data values for duplicated records are not altered.

###Balanced.Data.dev1 has 14k records while Balanced.Data.dev2 has 28k records
###Wehave to check is the model improving? Also till what extent is the model getting overfitted
#  due to duplication of records. Eg:Balanced.Data.dev2 has 14k records of targer class1 made 
#  from deplication of ~1250 class1 records

##We are also creating a Balance dataset of ratio 3:7 for class0:class1 and validating
Balanced.Data.dev3=ovun.sample(Target~., Balanced.Data.dev,N=14000,p=0.3,method= "both")$data
###p=0.3 is prob(class1)
#View(Balanced.Data.dev3)
###Above balanced data has 9856 rows of class0 and remaining class1 ot of 14k records  

###===============###
###CREATING MODELS###
###===============###
###MODEL4###
###======###
###Using Balanced.Data.dev1 having 14k records
###Our data has 7k class0 and 7k class1 rows, but class1 has many duplicates
##Model created with cp as 0.001, minimum improvement in cost parameter should be 0.1%
##Our cp paramter in CART will be classification Error. So algorithm looks for min of 00.1%
# improvements in CE calculated using cross validated(xval=10) within our dataset 
control4=rpart.control(minsplit= 250, minbucket=100, cp=0.001, xval=10)
M4=rpart(formula= Target~., data=Balanced.Data.dev1[-1], method="class", control=control4)
M4
fancyRpartPlot(M4)
###Differences we see in M4 from previous models is right side of the Tree is predominently class1
###Balanced count of leaf nodes prediction for both classes. Also many nodes looks partial mix
#  of both the classes. We get better picture of M4 in model evaluations done
printcp(M4)
plotcp(M4)
###Minimum cross validation error is 0.55062 and cp is 0.00108 at nodesplit=30

###MODEL5###
###======###
control5=rpart.control(minsplit= 700, minbucket=300, cp=0.0001, xval=10)
M5=rpart(formula= Target~., data=Balanced.Data.dev2[-1], method="class", control=control5)
###We let over tree spread more using cp=0.0001
M5
fancyRpartPlot(M5)
###Compared to M4, M5 looks more biased towards class1, Also we know we have 14k records of class1
#  made from just 1250 reccords on random duplication.
printcp(M5)
plotcp(M5)
###lowest xerror is 0.57221 at nodesplit25

###MODEL6###
###======###
##Samplesize is again 14k so we reduce control parameters accordingly
control6=rpart.control(minsplit= 250, minbucket=100, cp=0.0001, xval=10)
M6=rpart(formula= Target~., data=Balanced.Data.dev3[-1], method="class", control=control6)
###We let over tree spread more using cp=0.0001
M6
fancyRpartPlot(M6)
###M6 tree looks more homogenous with bit more green in tree class(class0 majority)
###Also we see less number of nodes which are mild in color, i.e. nodes having nearly equal mix of 
#  both the classes. More nodes are darker(strong probability)
printcp(M6)
plotcp(M6)
###lowest xerror is 0.78873 at nodesplit28 which is comparitively higher compared to Model M5 & M6


###PREDICTIONS ON DEVDATA SAMPLES USING MODELS M4,M5 & M6###
###MODEL4 PREDICTIONS
##We predict class and probabilities for over/under sampled dev data 
Balanced.Data.dev1$predict.classM4 = predict(M4, Balanced.Data.dev1, type= "class")
Balanced.Data.dev1$predict.scoreM4 = predict(M4, Balanced.Data.dev1, type= "prob")
###Has probabilities of both class to be 0 and 1 for each observation

###MODEL5 PREDICTIONS
Balanced.Data.dev2$predict.classM5 = predict(M5, Balanced.Data.dev2, type= "class")
Balanced.Data.dev2$predict.scoreM5 = predict(M5, Balanced.Data.dev2, type= "prob")

###MODEL6 PREDICTIONS
Balanced.Data.dev3$predict.classM6= predict(M6, Balanced.Data.dev3, type= "class")
Balanced.Data.dev3$predict.scoreM6= predict(M6, Balanced.Data.dev3, type= "prob")
#View(Balanced.Data.dev3)

##Deciling and ranked matrix creation
Balanced.Data.dev1$decileM4 = decile(Balanced.Data.dev1$predict.scoreM4[,2])
Balanced.Data.dev2$decileM5 = decile(Balanced.Data.dev2$predict.scoreM5[,2])
Balanced.Data.dev3$decileM6 = decile(Balanced.Data.dev3$predict.scoreM6[,2])

##Create rank ordered matrix
tmp_DT1= data.table(Balanced.Data.dev1)
rank.M4= tmp_DT1[,list(count=length(Target), count_resp=sum(Target), 
                    count_non_resp=sum(Target==0)), by= decileM4][order(-decileM4)]
tmp_DT2= data.table(Balanced.Data.dev2)
rank.M5= tmp_DT2[,list(count=length(Target), count_resp=sum(Target), 
                    count_non_resp=sum(Target==0)), by= decileM5][order(-decileM5)]
tmp_DT3= data.table(Balanced.Data.dev3)
rank.M6= tmp_DT3[,list(count=length(Target), count_resp=sum(Target), 
                    count_non_resp=sum(Target==0)), by= decileM6][order(-decileM6)]
#View(rank.M4)
#View(rank.M5)
#View(rank.M6)
###We see M4 & M6 have 8 deciles each whereas M5 has 9 deciles
###No: of resp per count is higher at start(decile 10,9,8 etc) and decreases going down

##Calculating other evaluating parameters 
###MODEL4:::KS (Add other parameters of ranked order matrix)
rank.M4$cum_count= cumsum(rank.M4$count)
rank.M4$rrate= round( rank.M4$count_resp/rank.M4$count, 4)
rank.M4$cum_resp= cumsum(rank.M4$count_resp)
rank.M4$cum_non_resp= cumsum(rank.M4$count_non_resp)
rank.M4$cum_rel_resp=round( rank.M4$cum_resp/sum(rank.M4$count_resp) , 4)
rank.M4$cum_rel_non_resp=round( rank.M4$cum_non_resp/sum(rank.M4$count_non_resp), 4)
rank.M4$ks= abs(rank.M4$cum_rel_resp - rank.M4$cum_rel_non_resp)
rank.M4$rrate= percent(rank.M4$rrate)
rank.M4$cum_rel_resp= percent(rank.M4$cum_rel_resp)
rank.M4$cum_rel_non_resp= percent(rank.M4$cum_rel_non_resp)
rank.M4$ks= percent(rank.M4$ks)

###MODEL5:::KS 
rank.M5$cum_count= cumsum(rank.M5$count)
rank.M5$rrate=round(rank.M5$count_resp/rank.M5$count, 4)
rank.M5$cum_resp=cumsum(rank.M5$count_resp)
rank.M5$cum_non_resp=cumsum(rank.M5$count_non_resp)
rank.M5$cum_rel_resp=round( rank.M5$cum_resp/sum(rank.M5$count_resp), 4)
rank.M5$cum_rel_non_resp=round( rank.M5$cum_non_resp/sum(rank.M5$count_non_resp), 4)
rank.M5$ks= abs(rank.M5$cum_rel_non_resp - rank.M5$cum_rel_resp)
rank.M5$rrate= percent(rank.M5$rrate)
rank.M5$cum_rel_resp= percent(rank.M5$cum_rel_resp)
rank.M5$cum_rel_non_resp= percent(rank.M5$cum_rel_non_resp)
rank.M5$ks= percent(rank.M5$ks)

###MODEL6:::KS 
rank.M6$cum_count= cumsum(rank.M6$count)
rank.M6$rrate=round(rank.M6$count_resp/rank.M6$count, 4)
rank.M6$cum_resp=cumsum(rank.M6$count_resp)
rank.M6$cum_non_resp=cumsum(rank.M6$count_non_resp)
rank.M6$cum_rel_resp=round( rank.M6$cum_resp/sum(rank.M6$count_resp), 4)
rank.M6$cum_rel_non_resp=round( rank.M6$cum_non_resp/sum(rank.M6$count_non_resp), 4)
rank.M6$ks= abs(rank.M6$cum_rel_non_resp - rank.M6$cum_rel_resp)
rank.M6$rrate= percent(rank.M6$rrate)
rank.M6$cum_rel_resp= percent(rank.M6$cum_rel_resp)
rank.M6$cum_rel_non_resp= percent(rank.M6$cum_rel_non_resp)
rank.M6$ks= percent(rank.M6$ks)

View(rank.M4)
View(rank.M5)
View(rank.M6)

###INFERENCES FROM RANKED MATRIX###
###=============================###
###M4 has 14k record dataset with max ks value of 49.37% and we are ranking top 50%(3500) and 
#  70%(5200) of responders out of 4363 and 7086 customers in top 3 & 5 deciles respectively
###M5 has 28k obs. and max ks of 45%. Top 52%(7266) and 73%(10193) of responders out of 9388
#  and 14140 records cumalated in top 3 and 5 deciles. 
###M6 has 14k records with max ks of 45% where top 63%(2600) and 81%(3400) out of 4444 and 7067
#  customers. Seeing results we see our predictions are better in M6(63%) where duplications and 
#  data changes were less. We goahead and compute auc and ks values from perfection perpective.


###Compute ks and area under roc curve
###Model4
pred4= prediction(Balanced.Data.dev1$predict.scoreM4[,2],Balanced.Data.dev1$Target)
perf4= performance(pred4,"tpr","fpr")
plot(perf4)
ks4= max( attr(perf4,'y.values')[[1]] - attr(perf4,'x.values')[[1]])  ##0.4937
auc4= performance(pred4,"auc")
auc4= as.numeric(auc4@y.values)                                       ##0.7967

###Model5
pred5= prediction(Balanced.Data.dev2$predict.scoreM5[,2],Balanced.Data.dev2$Target)
perf5= performance(pred5,"tpr","fpr")
plot(perf5)
ks5= max( attr(perf5,'y.values')[[1]] - attr(perf5,'x.values')[[1]])  ##0.4520
auc5= performance(pred5,"auc")
auc5= as.numeric(auc5@y.values)                                       ##0.7879

###Model6
pred6= prediction(Balanced.Data.dev3$predict.scoreM6[,2],Balanced.Data.dev3$Target)
perf6= performance(pred6,"tpr","fpr")
plot(perf6)
ks6= max( attr(perf6,'y.values')[[1]] - attr(perf6,'x.values')[[1]])  ##0.4540
auc6= performance(pred6,"auc")
auc6= as.numeric(auc6@y.values)                                       ##0.7959
###auc values are comparably same for all 3 models

###GINI index
?ineq
GINI4=ineq(Balanced.Data.dev1$predict.scoreM4[,2],type= "Gini")           ##0.3024
GINI5=ineq(Balanced.Data.dev2$predict.scoreM5[,2],type= "Gini")           ##0.2891
GINI6=ineq(Balanced.Data.dev3$predict.scoreM6[,2],type= "Gini")           ##0.4163
###Again evaluating using GINI we get our model M6 is very better compared to M5 and M6
###But again we got minimum satisfying GINI of 41.63% for M6

###Classification matrix and classfication Error
CM4= with(Balanced.Data.dev1,table(Balanced.Data.dev1$Target,Balanced.Data.dev1$predict.classM4))
CM5= with(Balanced.Data.dev2,table(Balanced.Data.dev2$Target,Balanced.Data.dev2$predict.classM5))
CM6= with(Balanced.Data.dev3,table(Balanced.Data.dev3$Target,Balanced.Data.dev3$predict.classM6))
##CE(Classification error is no of wrong predictions by total number of observations)
CE4= (1884+1663)/(14000)   ##25.33% 
CE5= (4305+3370)/(28000)   ##27.41%
CE6= (978+2104)/(14000)    ##22.01%
###Because of duplication's looks like CE have increased exponentially in our models
###Different from M1,M2 and M3 we see error is evenly distrbuted across both class0 and class1
###Increase in CE's were seen as increase in mix(blue and green) colour nodes in tree
###Dark blue or green signifies good strength of classification regarding the particular node



###==========================================###
###Prediction and evaluation in Holdout class###
###==========================================###
#View(Data.holdout)
##Prediction using M4, M5 and M6 models
Data.holdout$predictclass.M4= predict(M4, Data.holdout, type= "class")
Data.holdout$predictprob.M4= predict(M4, Data.holdout, type= "prob")
Data.holdout$predictclass.M5= predict(M5, Data.holdout, type= "class")
Data.holdout$predictprob.M5= predict(M5, Data.holdout, type= "prob")
Data.holdout$predictclass.M6= predict(M6, Data.holdout, type= "class")
Data.holdout$predictprob.M6= predict(M6, Data.holdout, type= "prob")

##Deciling in holdout data
Data.holdout$decile.M4= decile(Data.holdout$predictprob.M4[,2])
Data.holdout$decile.M5= decile(Data.holdout$predictprob.M5[,2])
Data.holdout$decile.M6= decile(Data.holdout$predictprob.M6[,2])

tmp_DT_Holdout= data.table(Data.holdout)
rank.Holdout.M4= tmp_DT_Holdout[,list(count=length(Target), count_resp=sum(Target),
                                   count_non_resp=sum(Target==0)), by=decile.M4][order(-decile.M4)]
rank.Holdout.M5= tmp_DT_Holdout[,list(count=length(Target), count_resp=sum(Target),
                                   count_non_resp=sum(Target==0)), by=decile.M5][order(-decile.M5)]
rank.Holdout.M6= tmp_DT_Holdout[,list(count=length(Target), count_resp=sum(Target),
                                   count_non_resp=sum(Target==0)), by=decile.M6][order(-decile.M6)]

rank.Holdout.M4$rrate= round(rank.Holdout.M4$count_resp/rank.Holdout.M4$count, 4)
rank.Holdout.M4$cum_resp= cumsum(rank.Holdout.M4$count_resp)
rank.Holdout.M4$cum_non_resp= cumsum(rank.Holdout.M4$count_non_resp)
rank.Holdout.M4$cum_rel_resp= round(rank.Holdout.M4$cum_resp/sum(rank.Holdout.M4$count_resp), 4)
rank.Holdout.M4$cum_rel_non_resp= round(rank.Holdout.M4$cum_non_resp/sum(rank.Holdout.M4$count_non_resp),4)
rank.Holdout.M4$ks= rank.Holdout.M4$cum_rel_resp - rank.Holdout.M4$cum_rel_non_resp
rank.Holdout.M4$cum_count= cumsum(rank.Holdout.M4$count)
rank.Holdout.M4.modelAccuracy= sum(rank.Holdout.M4$count_resp)/ sum(rank.Holdout.M4$count)
rank.Holdout.M4$cum_wgt_resp_rate= rank.Holdout.M4$cum_resp/rank.Holdout.M4$cum_count
rank.Holdout.M4$lift= rank.Holdout.M4$cum_wgt_resp_rate/rank.Holdout.M4.modelAccuracy
rank.Holdout.M4$rrate= percent(rank.Holdout.M4$rrate)
rank.Holdout.M4$cum_rel_resp=percent(rank.Holdout.M4$cum_rel_resp)
rank.Holdout.M4$cum_rel_non_resp=percent(rank.Holdout.M4$cum_rel_non_resp)
rank.Holdout.M4$ks=percent(rank.Holdout.M4$ks)

rank.Holdout.M5$rrate= round(rank.Holdout.M5$count_resp/rank.Holdout.M5$count, 4)
rank.Holdout.M5$cum_resp= cumsum(rank.Holdout.M5$count_resp)
rank.Holdout.M5$cum_non_resp= cumsum(rank.Holdout.M5$count_non_resp)
rank.Holdout.M5$cum_rel_resp= round(rank.Holdout.M5$cum_resp/sum(rank.Holdout.M5$count_resp), 4)
rank.Holdout.M5$cum_rel_non_resp= round(rank.Holdout.M5$cum_non_resp/sum(rank.Holdout.M5$count_non_resp),4)
rank.Holdout.M5$ks= rank.Holdout.M5$cum_rel_resp - rank.Holdout.M5$cum_rel_non_resp
rank.Holdout.M5$cum_count= cumsum(rank.Holdout.M5$count)
rank.Holdout.M5.modelAccuracy= sum(rank.Holdout.M5$count_resp)/ sum(rank.Holdout.M5$count)
rank.Holdout.M5$cum_wgt_resp_rate= rank.Holdout.M5$cum_resp/rank.Holdout.M5$cum_count
rank.Holdout.M5$lift= rank.Holdout.M5$cum_wgt_resp_rate/rank.Holdout.M5.modelAccuracy
rank.Holdout.M5$rrate= percent(rank.Holdout.M5$rrate)
rank.Holdout.M5$cum_rel_resp=percent(rank.Holdout.M5$cum_rel_resp)
rank.Holdout.M5$cum_rel_non_resp=percent(rank.Holdout.M5$cum_rel_non_resp)
rank.Holdout.M5$ks=percent(rank.Holdout.M5$ks)


rank.Holdout.M6$rrate= round(rank.Holdout.M6$count_resp/rank.Holdout.M6$count, 4)
rank.Holdout.M6$cum_resp= cumsum(rank.Holdout.M6$count_resp)
rank.Holdout.M6$cum_non_resp= cumsum(rank.Holdout.M6$count_non_resp)
rank.Holdout.M6$cum_rel_resp= round(rank.Holdout.M6$cum_resp/sum(rank.Holdout.M6$count_resp), 4)
rank.Holdout.M6$cum_rel_non_resp= round(rank.Holdout.M6$cum_non_resp/sum(rank.Holdout.M6$count_non_resp),4)
rank.Holdout.M6$ks= rank.Holdout.M6$cum_rel_resp - rank.Holdout.M6$cum_rel_non_resp
rank.Holdout.M6$cum_count= cumsum(rank.Holdout.M6$count)
rank.Holdout.M6.modelAccuracy= sum(rank.Holdout.M6$count_resp)/ sum(rank.Holdout.M6$count)
rank.Holdout.M6$cum_wgt_resp_rate= rank.Holdout.M6$cum_resp/rank.Holdout.M6$cum_count
rank.Holdout.M6$lift= rank.Holdout.M6$cum_wgt_resp_rate/rank.Holdout.M6.modelAccuracy
rank.Holdout.M6$rrate= percent(rank.Holdout.M6$rrate)
rank.Holdout.M6$cum_rel_resp=percent(rank.Holdout.M6$cum_rel_resp)
rank.Holdout.M6$cum_rel_non_resp=percent(rank.Holdout.M6$cum_rel_non_resp)
rank.Holdout.M6$ks=percent(rank.Holdout.M6$ks)

View(rank.Holdout.M4)
View(rank.Holdout.M5)
View(rank.Holdout.M6)
###Inferences from ranked matrix of holdout data
###Holdout data has 6000 records, with ~500 responders and 5500 nonresponders
###Our ratio of responders to non responders in holdoout data  is 1:11

###Applying model M4 on holdout dataset,we segregated 65%(327) of responders in top 3 deciles
#  In top 3 deciles we approach ~1900 customers with lift of 2.71, 2.35 and 2.05 resp.
#  We get max ks of 39.31%

###Applying model M5 to holdout, we get max(ks) of 39.45% where we have segregated 66.3%(330)
#  of responders by approaching 1880 customers in top 3 deciles. Lift of top 3 deciles are 
#  2.79, 2.43 and 2.11 resp. 

###Applying model M6 to holdout, we get max(ks) of 36.47% and we get 61.6% and 78.5% of 
#  responders filtered in top3 and top4 deciles resp. No of cust's approached in top3 and top4
#  deciles are 1837 and 2763. We have a lift of 3.01, 2.29 and 2.01 for top 3 deciles

##Compute ks and auc values 
pred.holdout4= prediction(Data.holdout$predictprob.M4[,2],Data.holdout$Target)
pred.holdout5= prediction(Data.holdout$predictprob.M5[,2],Data.holdout$Target)
pred.holdout6= prediction(Data.holdout$predictprob.M6[,2],Data.holdout$Target)
perf.holout4= performance(pred.holdout4,"tpr","fpr")
perf.holout5= performance(pred.holdout5,"tpr","fpr")
perf.holout6= performance(pred.holdout6,"tpr","fpr")
plot(perf.holout4)
plot(perf.holout5)
plot(perf.holout6)
ks.holdout4=max( attr(perf.holout4,'y.values')[[1]] - attr(perf.holout4,'x.values')[[1]])
ks.holdout5=max( attr(perf.holout5,'y.values')[[1]] - attr(perf.holout5,'x.values')[[1]])
ks.holdout6=max( attr(perf.holout6,'y.values')[[1]] - attr(perf.holout6,'x.values')[[1]])
auc.holdout4= performance(pred.holdout4,"auc")
auc.holdout5= performance(pred.holdout5,"auc")
auc.holdout6= performance(pred.holdout6,"auc")
auc.holdout4= as.numeric(auc.holdout4@y.values)
auc.holdout5= as.numeric(auc.holdout5@y.values)
auc.holdout6= as.numeric(auc.holdout6@y.values)

##GINI index
GINI.holdout.M4=ineq(Data.holdout$predictprob.M4[,2],type= "Gini")           ##0.360132
GINI.holdout.M5=ineq(Data.holdout$predictprob.M5[,2],type= "Gini")           ##0.3205656
GINI.holdout.M6=ineq(Data.holdout$predictprob.M6[,2],type= "Gini")           ##0.4183152
###By GINI values, only M6 prediction for holdout data crosses expected GINI of 40%

##Classification matrix and classification error
Holdout.CM4= with(Data.holdout,table(Data.holdout$Target,Data.holdout$predictclass.M4))
Holdout.CM5= with(Data.holdout,table(Data.holdout$Target,Data.holdout$predictclass.M5))
Holdout.CM6= with(Data.holdout,table(Data.holdout$Target,Data.holdout$predictclass.M6))
##CE(Classification error is no of wrong predictions by total number of observations)
CE4= (160+1743)/(6000)   ##31.71% 
CE5= (1609+164)/(6000)   ##29.55%
CE6= (602+333)/(6000)    ##15.58%
###M6 has lower classification Error but again it makes 2 wrong predictions for every 3 class1
#  i.e it potentially identifies and predicts only ~33% of responders correctly

