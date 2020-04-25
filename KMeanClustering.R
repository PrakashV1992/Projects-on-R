###Kmeans clustering on Retail RCDF dataset
#We have identify no of clusters using elbow rule in scree plot and design clusters

setwd("D:/Analytics/R/Dataset/Machine Learning/week1")
RCDF <- read.csv("Cust_Spend_Data.csv")
View(RCDF)

###Standardization of the data [-x,+x]
scaled.RCDF <- scale(RCDF[,3:7])
View(scaled.RCDF)

wssplot <- function(data,nc=15,seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data, 2, var))

  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data,centres<-i)$withinss)}
  
  plot(1:nc, wss, type="b", xlab="No: of Clusters",
       ylab="Within Sum square of the group")} 

wssplot(scaled.RCDF,nc=5)
###From above plot we can see elbow curve after 3, so nc should be 3

###Other way of checking optimal number of clusters is using NbClust package
install.packages("NbClust")
library(NbClust)

set.seed(1234)
?NbClust
nc=NbClust(scaled.RCDF[,c(-1,-2)], min.nc=2, max.nc=4, method="kmeans")
###It is very important to pass the scaled RCDF data set
#  For above number of clusters suggested by both algorithms is 3

###Next step iswe have to do kmeans clustering 
?kmeans
kmeans.clus<- kmeans(x=scaled.RCDF, centers=3, nstart=25, iter.max= 100)
kmeans.clus
#  btwSumSq/totalSumSq is 60.2%
#  Variances with in clusters are 6.126217, 5.256752, 6.514105

###Plot using fpc
install.packages("fpc")
library(fpc)
plotcluster(scaled.RCDF,kmeans.clus$cluster)

###Better plot using cluster library
install.packages("cluster")
library(cluster)
clusplot(scaled.RCDF, kmeans.clus$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=1)
###Better interpretive plot of with data points marked

###Profiling mean values for the unscaled data clusters 
RCDF$Cluster= kmeans.clus$cluster
View(RCDF)
aggr=aggregate(RCDF[,-c(1,2,8)], list(RCDF$Cluster), mean)
clus.profile<- data.frame(Cluster=aggr[,1], 
                          Freq= as.vector(table(RCDF$Cluster)), aggr[,-1])
View(clus.profile)

###As part of predictive analytics we can apply class logistic  regression 
#  considering cluster as labeled value and other colummns as x variables
###This would help us with better customer segmentation for future customers
