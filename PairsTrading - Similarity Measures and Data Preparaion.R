#Monitor stocks from Jan 2012 - June 2014. Back test for 4 months from Aug 2014 to Oct 214
# Try 1 - Price ratios
# Consider Pepsico - PEP and Coca Cola - CCE

###############################Get and prepare data###########################
library(quantmod)
Coke <- getSymbols("CCE",from = "2012-01-01", to = "2014-10-31",auto.assign=F)
Pepsi<- getSymbols("PEP",from = "2012-01-01", to = "2014-10-31",auto.assign=F)
Coke.prices <- Ad(Coke)
Pepsi.prices <- Ad(Pepsi) 
Coke.prices.tr<-Coke.prices['2012-01-01/2014-06-30']
Pepsi.prices.tr<-Pepsi.prices['2012-01-01/2014-06-30']

################################Price ratios##################################
ratio<-Coke.prices.tr/Pepsi.prices.tr
ratio.mean<-mean(ratio)
ratio.sd <- sd(ratio)
ul<-ratio.mean + 1 * ratio.sd
ll <- ratio.mean - 1 * ratio.sd
#Test period
plot(ratio,main='Price ratios')
abline(h=c(ratio.mean),col="green")
abline(h=c(ul,ll),col="red")

#Full period
ratio1<-Coke.prices/Pepsi.prices
ratio.mean.1<-mean(ratio1)
ratio.sd.1 <- sd(ratio1)
ul.1<-ratio.mean.1 + 1 * ratio.sd.1
ll.1 <- ratio.mean.1 - 1 * ratio.sd.1
plot(ratio1,main='Price ratios',minor.ticks=F)
abline(h=c(ratio.mean.1),col="green")
abline(h=c(ul.1,ll.1),col="red")

#################################Squared Distances##############################
pepsi.max<- max(Pepsi.prices.tr)
pepsi.min<- min(Pepsi.prices.tr)
coke.max<- max(Coke.prices.tr)
coke.min<- min(Coke.prices.tr)

#Normalizing prices
pepsi.norm<- (Pepsi.prices.tr - pepsi.min)/(pepsi.max - pepsi.min)
coke.norm<- (Coke.prices.tr - coke.min)/(coke.max - coke.min)

#Least squares
lstsqr<-sum(abs(pepsi.norm-coke.norm)^2)
lstsqr

#Function to calculate  squared distance
l.sqr <- function(x,y){
  x.prices<- getSymbols(x,from = "2012-01-01", to = "2014-06-30",auto.assign=F)
  y.prices<- getSymbols(y,from = "2012-01-01", to = "2014-06-30",auto.assign=F)
  x.max<- max(x.prices)
  x.min<- min(x.prices)
  y.max<- max(y.prices)
  y.min<- min(y.prices)
  x.prices.norm <- (x.prices - x.min)/(x.max-x.min)
  y.prices.norm <- (y.prices - y.min)/(y.max-y.min)
  lstsqr<- sum(abs(x.prices.norm - y.prices.norm)^2)
  return(lstsqr)
}

l.sqr("MCD","BKW")

###################################Data Preparation for all Stocks######################
#################################Getting data from Jan 1 2012 to June 30 2014#########
# There are 713 trading days between Jan 1, 2012 and June 30, 2014

#Import Ticker and Sector data
TickerSector<- read.csv("TickersAndSector.csv",header=T,as.is=T,strip.white=TRUE)


#Create xts object with data for all stocks # Data available for 1028 stocks
masterdata <-getSymbols(TickerSector[1,1],from = "2012-01-01", to = "2014-06-30",auto.assign=F)
masterdata <- Ad(masterdata)               
nrows <- dim(TickerSector)[1] #1034

for (i in 2:nrows) {
  tryCatch({
  temp<- getSymbols(TickerSector[i,1],from = "2012-01-01", to = "2014-06-30",auto.assign=F) 
  masterdata<- merge(masterdata,Ad(temp))
  }, error = function(err){
    
  })
}



write.csv(masterdata,file="PriceDataComplete.csv")
#Not all tickers have data so identifying 1028 tickers that have data

temp <- colnames(masterdata)
tickers.available<- sapply(temp,function(x){strsplit(x,'[.]')[[1]][1]})
names(tickers.available)<-NULL
tickers.available.df<- as.data.frame(tickers.available)
colnames(tickers.available.df)<-'Ticker'
TickerSector<-merge(TickerSector,tickers.available.df)

#Healthcare
Healthcare <- subset(TickerSector,Sector=="Healthcare",select=Ticker)
#Energy
Energy <- subset(TickerSector,Sector=="energy",select=Ticker)
#Material & Proc
MaP<- subset(TickerSector,Sector=="material&proc",select=Ticker)
# Producer Durables
PD <- subset(TickerSector,Sector=="producer durable",select=Ticker)
#Consumer Discretionary
CD <- subset(TickerSector,Sector=="consumerDiscretionary",select=Ticker)
#Technology
Tech <- subset(TickerSector,Sector=="technology",select=Ticker)
#Financials
Fin <- subset(TickerSector,Sector=="financial",select=Ticker)
#Consumer Staple
CS<- subset(TickerSector,Sector=="consumer staple",select=Ticker)
#Utilities
Utilities<-subset(TickerSector,Sector=="utility",select=Ticker)
