## Portfolio COnstruction
# install packages
library(timeSeries)
library(xts)
library(TTR)
library(quantmod)
library(PerformanceAnalytics)
library(fPortfolio)
library(tseries)

# download daily prict of 10 ideal ETFs & generate monthly returns
ETFs <-c("GSY","NEAR","SRLN","KIE","MINT","FTSL","SCPB","SOXX","PEY","KBWP")
getSymbols(ETFs, src='yahoo', from='2015-05-04')

for(symbol in ETFs) {
  x <- get(symbol)
  indexFormat(x) <- '%Y-%m-%d'
  colnames(x) <- gsub("x",symbol,colnames(x))
  x <- x[,6]
  assign(symbol,x)
}
p <- do.call(merge, lapply(ETFs, get))
colnames(p) <-c(ETFs)
p.m <-to.period(p,period="months",k=1,indexAt="lastof",OHLC=FALSE)
p.ret <-na.omit(ROC(p.m,12,"discrete"))
p.ret.ts <-as.timeSeries(p.ret)
portfolios <- dim(p.ret.ts)[2] 

## unconstrainted optimization 
spec <- portfolioSpec()
setSolver(spec) <- "solveRquadprog"
setNFrontierPoints(spec) <- 30
cons <- c("LongOnly") 

#plot efficient frontier 
frontier1 <- portfolioFrontier(p.ret.ts, spec, cons)
tailoredFrontierPlot(object = frontier1)     

#plot portfolio weights
weightsPlot(frontier1, col = rainbow(portfolios)) 

##constrainted optimization (5% ~ 50%)
spec <- portfolioSpec()
setSolver(spec) <- "solveRquadprog"
setNFrontierPoints(spec) <- 30
cons <- c("minW[1:portfolios]=0.05", "maxW[1:portfolios]=0.5")

#plot new efficient frontier 
frontier2 <- portfolioFrontier(p.ret.ts, spec, cons) 
tailoredFrontierPlot(object = frontier2)    

#plot newportfolio weights
weightsPlot(frontier2, col = rainbow(portfolios))

#generate table of conservative, moderate, and aggressive portfolios
a=frontier2@portfolio
b=a@portfolio$weights
con=b[4,]
con=round(con,2)
mod=b[7,]
mod=round(mod,2)
agg=b[10,]
agg=round(agg,2)
portfolio=cbind(con,mod,agg)
portfolio

#END#

