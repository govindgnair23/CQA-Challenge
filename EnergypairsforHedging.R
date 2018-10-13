#Create xts object with data  from 2014-06-01 for all Energy stocks 
masterdata.Energy.2 <-getSymbols(Energy[1,1],from = "2014-06-01",auto.assign=F)
masterdata.Energy.2 <- Ad(masterdata.Energy.2)               
nrows <- dim(Energy)[1] #1034

for (i in 2:nrows) {
  tryCatch({
    temp<- getSymbols(Energy[i,1],from = "2014-06-01",auto.assign=F) 
    masterdata.Energy.2<- merge(masterdata.Energy.2,Ad(temp))
  }, error = function(err){
    
  })
}


#########################Finding the best pairs####################################

#Function to calculate  price ratios of 2 stocks
l.pr <- function(D){
  x.prices<- D[,1]
  y.prices<- D[,2]
  xyratio <- x.prices/y.prices
  xyratio.mean <- mean(xyratio)
  xyratio.sd<- sd(xyratio)
  return(list(xyratio.sd,xyratio.mean))
}

Energylabels<- sapply(Energy,function(x){ paste(x,".Adjusted",sep="")})
PR.df.Energy.2<-data.frame(rep(NA,3240),rep(NA,3240),rep(0,3240),rep(0,3240))
colnames(PR.df.Energy.2)<- c('Stock1','Stock2','SD of PR','Mean of PR')


k=1
for(i in 1:81){
  for(j in i:81){
    if (i != j){
      pairprices <- masterdata.Energy.2[,c(i,j)]
      pairprices <- na.omit(pairprices)
      l.output<-l.pr(pairprices)
      PR.df.Energy.2[k,1]<- Energy[i,1]
      PR.df.Energy.2[k,2]<- Energy[j,1]
      PR.df.Energy.2[k,3]<- l.output[[1]]
      PR.df.Energy.2[k,4]<- l.output[[2]]
      k=k+1
      
    }
    
  }
}

#Calculate coefficient of variation
PR.df.Energy.2$COV <- PR.df.Energy.2$'SD of PR'/PR.df.Energy.2$'Mean of PR'
PR.df.Energy.2<-PR.df.Energy.2[order(PR.df.Energy.2$COV),]
write.csv(PR.df.Energy.2,"EnergyPairs2.csv")

#Plot prices of top 5 pairs
top.5.pairs<- read.csv('Top.5.Pairs.csv',header=T,as.is=T)

for (i in 1:12){
  labels.1 <- top.5.pairs[i,]
  labels.2<- sapply(labels.1,function(x){ paste(x,".Adjusted",sep="")})
  plot.data <- as.zoo(masterdata.Energy.2[,labels.2])
  plot(x=plot.data,ylab="Share Prices",xlab="Time",screens=1,col=1:6)
  title(main=paste("Time Series for pairs of ",labels.1[1],sep=""))
  legend("topright",legend=labels.1,pch=16,col=1:6,cex=.5)
}
