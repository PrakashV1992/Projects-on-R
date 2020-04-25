###Clustering in  R using retail data set of 10 customer rows

setwd("D:/Analytics/R/Dataset/Machine Learning/week1")
RCDF <- read.csv("Cust_Spend_Data.csv")
View(RCDF)

###Standardization of the data [-x,+x]
scaled.RCDF <- scale(RCDF[,3:7])
View(scaled.RCDF)

###Calculate Euclidean's distance between records
Euc.dist <- dist(scaled.RCDF, method = "euclidean")
###Euc.dist <- dist(scaled.RCDF, method = "euclidean", diag=TRUE, upper=TRUE)
print(Euc.dist, digits=3)

###Hierarchical clustering using the above distance matrix
?hclust
clus2 <- hclust(Euc.dist, method="average")
plot(clus2, labels= as.character(RCDF[,2]))
###To identify ascending order of distances wrt which clusters were segregated
clus2$height

###From cluster Dendogram plot, we can main clustering can be done as 3 trees
###So we add an extra column cluster to each row which signifying clusternumber
RCDF$Cluster <- cutree(clus2, k<-3)
###Profiling these clusters
aggr= aggregate(RCDF[,-c(1,2,8)], list(RCDF$Cluster), sum)
aggr
###In above command first arg is data matrix, second item in the function is  
#  by<- list(feature on whose basis we want to segregate the clusters). 
#  In our scenario, it is cluster col with cluster numbers. Third arg is
#  on what mathematical functionality o/p is expected. Can also be "sum" 
#  which designs aggr cluster sum values rather than means

clus.profile<- data.frame(Cluster=aggr[,1], Freq=as.vector(table(RCDF$Cluster))
                          , aggr[,-1])
View(clus.profile)


