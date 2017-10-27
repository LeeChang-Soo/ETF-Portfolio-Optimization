setwd("C:/Users/Desktop/STAT FINA/project/optimization")
ETFnew <- read.csv("ETFstd.csv")


#####Plot
library(ggplot2)
ggplot(data = ETFnew, aes(mean, std, label = ETFname)) +
  geom_point()

#####Exclude risk top three
ETFRisky = subset(ETFnew, std>3.75)
ETFOther = subset(ETFnew, std<3.75)
ggplot(data = ETFOther, aes(mean, std, label = ETFname)) +
  geom_point()

#####Show the risky three ETFs
ggplot(ETFRisky, aes(mean, std)) +
  geom_point() +
  geom_text(aes(label=ETFname)) +
  xlim(0,0.4) +
  ylim(0,10)

#####KMeans Cluster K=10
set.seed(12) 
kcluster <- kmeans(ETFOther[,c("mean","std")], centers=10, nstart=10)
#kcluster
kcluster$cluster <- as.factor(kcluster$cluster)
ggplot(ETFOther, aes(mean, std, color = kcluster$cluster)) + 
  geom_point()
group10 = order(kcluster$cluster)
cluster10 = data.frame(ETFOther$ETFname[group10], kcluster$cluster[group10])

#####KMeans Cluster K=5
set.seed(11) 
kcluster5 <- kmeans(ETFOther[,c("mean","std")], centers=5, nstart=10)
#kcluster5
kcluster5$cluster <- as.factor(kcluster5$cluster)
ggplot(ETFOther, aes(mean, std, color = kcluster5$cluster)) + 
  geom_point()
group5 = order(kcluster5$cluster)
cluster5 = data.frame(ETFOther$ETFname[group5], kcluster5$cluster[group5])

#summary cluster
points.matrix <- cbind(mean = ETFnew$mean, std = ETFnew$std)
kclust <- kmeans(points.matrix, 5)
kclust
summary(kclust)


#generate table for ETFs excluding outliers
name5 = cluster5$ETFOther.ETFname.group5.
cluster5_sharpe = ETFnew[ETFnew$ETFname %in% name5, ]
cluster5_sharpe$cluster = cluster5[match(cluster5_sharpe$ETFname, cluster5$ETFOther.ETFname.group5.),2]

#count the number of ETFs for each cluster
count=as.data.frame(table(cluster5_sharpe$cluster))
count1= count[1,2]
count2= count[2,2]
count3= count[3,2]
count4= count[4,2]

#cort cluster and sharpe ratio
cluster5_sort= cluster5_sharpe[with(cluster5_sharpe, order(cluster,-sharpe.ratio)), ]
cluster5_sort

#generate table for top 2 sharpe ratio ETFs for each cluster
ETFsort1 <- subset(cluster5_sort, sharpe.ratio > cluster5_sort[3,6] & cluster == 1)
ETFsort2 <- subset(cluster5_sort, sharpe.ratio > cluster5_sort[(count1+3),6] & cluster == 2)
ETFsort3 <- subset(cluster5_sort, sharpe.ratio > cluster5_sort[(count2+count1+3),6] & cluster == 3)
ETFsort4 <- subset(cluster5_sort, sharpe.ratio > cluster5_sort[(count3+count2+count1+3),6] & cluster == 4)
ETFsort5 <- subset(cluster5_sort, sharpe.ratio > cluster5_sort[(count4+count3+count2+count1+3),6] & cluster == 5)
library(gtools)
cluster5_pf= smartbind(ETFsort1,ETFsort2,ETFsort3,ETFsort4,ETFsort5)


