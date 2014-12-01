######################Getting avg returns data for all  1034 stocks ####################

library(quantmod)
setwd("C:/Users/Govind/Documents/Learning and development/CQA Challenge")
#Creating a dataframe of the ticker symbols and Betas of all investible stocks
tickers.df <- read.csv('TickersAndBeta.csv',header=T,as.is=T)#Has tickers and Beta
row.names(tickers.df)<-tickers.df[,1]

# Creating a vector of tickers
tickers.vec<- tickers.df[,1]
N<- length(tickers.vec)

#Creating dataset to hold relevant  stock attributes
Data<- data.frame(c(rep(NA,N)),c(rep(NA,N)),c(rep(NA,N)),c(rep(NA,N)))
Data[,1]<-tickers.vec
names(Data)<-c("Stock","Ann Log returns","Daily Net returns","Daily Volatility")

#Retrieving stock attributes 
#Data pull to get  annual log returns and daily net returns
for (i in c(1:1034)) {
  tryCatch({
    d <- getSymbols(tickers.vec[i],from = "2014-08-17",auto.assign=FALSE)  
    Adj.Cl<- Ad(d)  
    Netreturns<-na.omit(diff(Adj.Cl)/lag(Adj.Cl,1))
    AvgNetreturns<- mean(Netreturns)
    returns.sd<- sd(Netreturns)  # Volatility of avg daily returns
    log.returns<- log(1 + Netreturns)
    ann.log.returns <- mean(log.returns) *252
    Data[i,2]<- ann.log.returns
    Data[i,3]<- AvgNetreturns
    Data[i,4]<- returns.sd
    Data[]} , error=function(err){
      Data[i,2]<- NA
      Data[i,3]<- NA
      Data[i,4]<- NA
      
    })
  
}

write.csv(Data,"Returns.csv",row.names=FALSE)
row.names(Data)<-Data[,1]


#######################Getting top 50 and bottom 50 performers#########################
Data<-Data[order(Data[,3],decreasing=T),] #sorting
top50<-Data[1:50,]
bottom50<-last(Data,50)
top50tickers<-top50[,1]
bottom50tickers<- bottom50[,1]
top50.bottom50<-c(top50tickers,bottom50tickers)

########################Calculating covariance matrix #############################
Dailyreturnsdata.100<- matrix(0,nrow=31,ncol=100)
for (i in c(1:100)) {
  
  d <- getSymbols(top50.bottom50[i],from = "2014-10-02",to ="2014-11-16",auto.assign=FALSE)  
  Adj.Cl<- Ad(d)  
  Netreturns<-na.omit(diff(Adj.Cl)/lag(Adj.Cl,1))
  Dailyreturnsdata.100[,i]<-Netreturns
}
Dailyreturnsdata.100 <- 100 * Dailyreturnsdata.100

Varcovar.100 <- var(Dailyreturnsdata.100)
is.positive.definite(Varcovar.100)#Not positive definite
Varcovar.100.P<- as.matrix(nearPD(Varcovar.100, conv.tol = 1e-6)$mat)
library("Matrix")
library("matrixcalc")
#Using the nearPD function to adjust the covariance matrix to make it positive 
# definite as covariance matris  is not positive definite. We can check the eigenvalues
#of the matrix.The negative eigenvalues are quite close to zero.


######################################OPTIMIZATION################################

dvec<-rep(0,100)

# constraints to ensure that long positions are between 0 and 0.05
# and short positions are between 0 and -0.05

M1<-diag(100)
diag(M1)[51:100] <- -1
M2<-diag(100)
diag(M2)[1:50] <- -1

#constraints to ensure long and short positions are fully invested
Dmat <- Varcovar.100.P
ones.zeroes1<- c(rep(1,50),rep(0,50))
ones.zeroes2<- c(rep(0,50),rep(1,50))
Amat<-cbind(ones.zeroes1,ones.zeroes2,Beta,-Beta,ones,-ones,M1,M2)
bvec<- c(1,-1,-0.5,-0.5,-0.05,-0.05,rep(0,100),c(rep(-0.05,50),rep(-0.05,50)) )
#Optimization
opt<- solve.QP(Dmat,dvec,Amat,bvec,meq=0)
soln<-(opt$solution)*100
soln[abs(soln)<.01]<-0
weights<-soln


#Tickers to invest
  portValue=10000000
  nonzero<-weights!=0
  tickers.not0<-top50.bottom50[nonzero]
  weights.not0<-weights[nonzero]
  dollar2trade<-weights.not0*portValue/100
  closeprice<-rep(NA,length(tickers.not0))
  for (i in 1:length(tickers.not0)){
    S <- getSymbols(tickers.not0[i],from="2014-11-10",to="2014-11-17",auto.assign=F)
    closeprice[i]<-as.numeric(Cl(last(S)))
  }
  shares2trade<-floor(dollar2trade/closeprice)
  Trades<-data.frame(tickers.not0,shares2trade)
  


write.csv(Trades,file ="Trades.csv")
