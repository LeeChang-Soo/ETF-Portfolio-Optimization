###Comparison
setwd("C:/Users/Desktop/STAT FINA/project/optimization")
# install packages
library(fPortfolio)
library(xts)
library(timeSeries)
library(tseries)
library(TTR)
library(quantmod)
library(PerformanceAnalytics)
#load data
price <- read.csv("price.csv")
SR <- read.csv("SharpeRatio.csv")
bmdata <- read.csv("bmdata.csv")
SRBM <- read.csv("SRBM.csv")

##k-means function
kmean<-function(x,price,SR,bmdata,BMSR)
{
  #generate daily return(%) table 
  DR = data.frame()
  BMDR=data.frame()
  for (j in 1:ncol(price)){
    #(x+120) < (nrow(price)-1)
    for(i in 1:120) {
      DR[i,j]=(price[(x+i),j]-price[((x+i)-1),j])/price[((x+i)-1),j]
      DR[i,j]=DR[i,j]*100
    }
  }
  for (j in 1:ncol(bmdata)){
    for(i in 1:120) {
      BMDR[i,j]=(bmdata[(x+i),j]-bmdata[((x+i)-1),j])/bmdata[((x+i)-1),j]
      BMDR[i,j]=BMDR[i,j]*100
    }
  }
  
  colnames(BMDR) = colnames(bmdata)
  for(j in 1:ncol(price)){
    colnames(DR)[j] = colnames(price)[j]
  }
  
  #generate return&std&sharpe ratio table 
  ETF = data.frame()
  BM=data.frame()
  for(i in 1:ncol(DR)){
    ETF[i,1]=sqrt(var(DR[,i]))
  }
  
  for(i in 1:ncol(BMDR)){
    BM[i,1]=sqrt(var(BMDR[,i]))
    BM[i,2]=mean(BMDR[,i])
    BM[i,3]=colnames(BMDR)[i]
    BM[i,4]=SR[nrow(BMSR),i+1]}
  
  
  for(i in 1:ncol(DR)){
    ETF[i,2]=mean(DR[,i])
  }
  
  for(i in 1:ncol(DR)){
    ETF[i,3]=colnames(DR)[i]}
  
  for(i in 1:ncol(DR)){
    ETF[i,4]=SR[nrow(SR),i+1]}
  
  
  colnames(ETF)[1] <- "std"
  colnames(ETF)[2] <- "mean"
  colnames(ETF)[3] <- "ETFname"
  colnames(ETF)[4] <- "sharpe_ratio"
  
  
  colnames(BM)[1] <- "std"
  colnames(BM)[2] <- "mean"
  colnames(BM)[3] <- "Benchmark name"
  colnames(BM)[4] <- "sharpe_ratio"
  
  ##plug data to k-means
  
  #Plot
  library(ggplot2)
  ggplot(data = ETF, aes(mean, std, label = ETFname)) +
    geom_point()
  
  #Exclude risk top three
  ETFRisky = subset(ETF, std>3.75)
  ETFOther = subset(ETF, std<3.75)
  ggplot(data = ETFOther, aes(mean, std, label = ETFname)) +
    geom_point()
  
  #Show the risky three ETFs
  ggplot(ETFRisky, aes(mean, std)) +
    geom_point() +
    geom_text(aes(label=ETFname)) +
    xlim(0,0.4) +
    ylim(0,10)
  
  #KMeans Cluster K=5
  set.seed(11) 
  kcluster5 <- kmeans(ETFOther[,c("mean","std")], centers=5, nstart=10)
  #kcluster5
  kcluster5$cluster <- as.factor(kcluster5$cluster)
  ggplot(ETFOther, aes(mean, std, color = kcluster5$cluster)) + 
    geom_point()
  group5 = order(kcluster5$cluster)
  cluster5 = data.frame(ETFOther$ETFname[group5], kcluster5$cluster[group5])
  
  #summary cluster
  points.matrix <- cbind(mean = ETF$mean, std = ETF$std)
  kclust <- kmeans(points.matrix, 5)
  kclust
  summary(kclust)
  
  
  #generate table for ETFs excluding outliers
  name5 = cluster5$ETFOther.ETFname.group5.
  cluster5_sharpe = ETF[ETF$ETFname %in% name5, ]
  cluster5_sharpe$cluster = cluster5[match(cluster5_sharpe$ETFname, cluster5$ETFOther.ETFname.group5.),2]
  
  #count the number of ETFs for each cluster
  count=as.data.frame(table(cluster5_sharpe$cluster))
  count1= count[1,2]
  count2= count[2,2]
  count3= count[3,2]
  count4= count[4,2]
  
  #cort cluster and sharpe ratio
  cluster5_sort= cluster5_sharpe[with(cluster5_sharpe, order(cluster,-sharpe_ratio)), ]
  
  #generate table for top 2 sharpe ratio ETFs for each cluster
  ETFsort1 <- subset(cluster5_sort, sharpe_ratio > cluster5_sort[3,4] & cluster == 1)
  ETFsort2 <- subset(cluster5_sort, sharpe_ratio > cluster5_sort[(count1+3),4] & cluster == 2)
  ETFsort3 <- subset(cluster5_sort, sharpe_ratio > cluster5_sort[(count2+count1+3),4] & cluster == 3)
  ETFsort4 <- subset(cluster5_sort, sharpe_ratio > cluster5_sort[(count3+count2+count1+3),4] & cluster == 4)
  ETFsort5 <- subset(cluster5_sort, sharpe_ratio > cluster5_sort[(count4+count3+count2+count1+3),4] & cluster == 5)
  library(gtools)
  cluster5_pf= c(ETFsort1[1,3],ETFsort1[2,3],ETFsort2[1,3],ETFsort2[2,3],ETFsort3[1,3],ETFsort3[2,3],ETFsort4[1,3],ETFsort4[2,3],ETFsort5[1,3],ETFsort5[2,3])
  return(cluster5_pf)
} 


##efficient frontier function
EFF=function(symbols,price,x,bmdata){
  price1=price
  tempprice=data.frame(nrows=575)
  for(i in 1:length(symbols)){
    temp=which(colnames(price1)==symbols[i])
    tempprice=cbind(tempprice,price[,temp])
    colnames(tempprice)[i+1]=symbols[i]
  }
  tempprice=tempprice[,-1]
  
  p <- xts(tempprice,order.by=as.POSIXct(rownames(price)))
  colnames(p) <-c(symbols)
  
  p.m <-to.period(p,period="months",k=1,indexAt="lastof",OHLC=FALSE)
  p.ret <-na.omit(ROC(p.m,12,"discrete"))
  p.ret.ts <-as.timeSeries(p.ret)
  
  assets <- length(symbols) 
  
  #constrainted optimization (5% ~ 50%)
  spec <- portfolioSpec()
  setSolver(spec) <- "solveRquadprog"
  setNFrontierPoints(spec) <- 30
  constraints <- c("minW[1:10]=0.05", "maxW[1:10]=0.5")
  frontier <- portfolioFrontier(p.ret.ts, spec, constraints) 
  
  #plot efficient frontier 
  tailoredFrontierPlot(object = frontier)      
  
  #plot portfolio weights
  weightsPlot(frontier, col = rainbow(assets)) 
  means=matrix(nrow = 3,ncol=1)
  
  # generate table of conservative, moderate, and aggressive portfolio
  a=frontier@portfolio
  p=a@portfolio$weights
  con=p[4,]
  con=round(con,2)
  mod=p[7,]
  mod=round(mod,2)
  agg=p[10,]
  agg=round(agg,2)
  m=a@portfolio$targetReturn
  s=a@portfolio$targetRisk
  means[1]=m[4,1]
  means[1]=round(means[1],2)
  means[2]=m[7,1]
  means[2]=round(means[2],2)
  means[3]=m[10,1]
  means[3]=round(means[3],2)
  bmreturn=matrix(nrow=5,ncol=1)
  for(i in 1:5){
  bmreturn[i,1]=((bmdata[120+x,i]-bmdata[x,i])/bmdata[x,i])*2.1
  }
  returnvector=matrix(nrow=3,ncol=5)
  for(i in 1:3){
    for(j in 1:5){
      if(means[i,1]>bmreturn[j,1])
      returnvector[i,j]=1
      else
        returnvector[i,j]=0
    }}
  return(returnvector)
}

#test 200 times
answer=data.frame()
for(j in 1:200){
  k=3*(j-1)
clusters=kmean(k,price,SR,bmdata,SRBM)
portfolio=EFF(clusters,price,k,bmdata)
answer=rbind(answer,portfolio)
}


