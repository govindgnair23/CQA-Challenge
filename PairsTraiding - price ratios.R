#TickerSector has tickers and sector information for all stocks
#masterdata has timer series data for all stocks


#Function to calculate  price ratios of 2 stocks
l.pr <- function(D){
  x.prices<- D[,1]
  y.prices<- D[,2]
  xyratio <- x.prices/y.prices
  xyratio.mean <- mean(xyratio)
  xyratio.sd<- sd(xyratio)
  return(list(xyratio.sd,xyratio.mean))
}

#######################################################################################
############Calculating std deviation of price ratios for pairs in Healthcare###########

HClabels<- sapply(Healthcare,function(x){ paste(x,".Adjusted",sep="")})
masterdata.HC<- masterdata[,HClabels]
PR.df.healthcare<-data.frame(rep(NA,5050),rep(NA,5050),rep(0,5050),rep(0,5050))
colnames(PR.df.healthcare)<- c('Stock1','Stock2','SD of PR','Mean of PR')


k=1
for(i in 1:101){
  for(j in i:101){
      if (i != j){
        pairprices <- masterdata.HC[,c(i,j)]
        pairprices <- na.omit(pairprices)
        l.output<-l.pr(pairprices)
        PR.df.healthcare[k,1]<- Healthcare[i,1]
        PR.df.healthcare[k,2]<- Healthcare[j,1]
        PR.df.healthcare[k,3]<- l.output[[1]]
        PR.df.healthcare[k,4]<- l.output[[2]]
        k=k+1
        
      }
      
  }
}

#Calculate coefficient of variation
PR.df.healthcare$COV <- PR.df.healthcare$'SD of PR'/PR.df.healthcare$'Mean of PR'
PR.df.healthcare<-PR.df.healthcare[order(PR.df.healthcare$COV),]
write.csv(PR.df.healthcare,"HealthcarePairs.csv")

#########################################################################################
#############Calculating std deviation of price ratios for pairs in Energy############

Energylabels<- sapply(Energy,function(x){ paste(x,".Adjusted",sep="")})
masterdata.Energy<- masterdata[,Energylabels]
PR.df.Energy<-data.frame(rep(NA,3240),rep(NA,3240),rep(0,3240),rep(0,3240))
colnames(PR.df.Energy)<- c('Stock1','Stock2','SD of PR','Mean of PR')


k=1
for(i in 1:81){
  for(j in i:81){
    if (i != j){
      pairprices <- masterdata.Energy[,c(i,j)]
      pairprices <- na.omit(pairprices)
      l.output<-l.pr(pairprices)
      PR.df.Energy[k,1]<- Energy[i,1]
      PR.df.Energy[k,2]<- Energy[j,1]
      PR.df.Energy[k,3]<- l.output[[1]]
      PR.df.Energy[k,4]<- l.output[[2]]
      k=k+1
      
    }
    
  }
}

#Calculate coefficient of variation
PR.df.Energy$COV <- PR.df.Energy$'SD of PR'/PR.df.Energy$'Mean of PR'
PR.df.Energy<-PR.df.Energy[order(PR.df.Energy$COV),]
write.csv(PR.df.Energy,"EnergyPairs.csv")


# Testing for PSX and SSE
pairprices.eg <- masterdata[,c('PSX.Adjusted','SSE.Adjusted')]
pairprices.eg <- na.omit(pairprices.eg)

l.pr.eg <- function(D){
  x.prices<- D[,1]
  y.prices<- D[,2]
  xyratio <- x.prices/y.prices
  xyratio.mean <- mean(xyratio)
  xyratio.sd<- sd(xyratio)
  return(list(xyratio.sd,xyratio.mean,xyratio))
}

temp<-l.pr.eg(pairprices.eg)
temp[[3]]

##########################################################################################3
#########Calculating std deviation of price ratios for pairs in Materials and Procurement######

MaPlabels<- sapply(MaP,function(x){ paste(x,".Adjusted",sep="")})
masterdata.MaP<- masterdata[,MaPlabels]
PR.df.MaP<-data.frame(rep(NA,2775),rep(NA,2775),rep(0,2775),rep(0,2775  ))
colnames(PR.df.MaP)<- c('Stock1','Stock2','SD of PR','Mean of PR')


k=1
for(i in 1:75){
  for(j in i:75){
    if (i != j){
      pairprices <- masterdata.MaP[,c(i,j)]
      pairprices <- na.omit(pairprices)
      l.output<-l.pr(pairprices)
      PR.df.MaP[k,1]<- MaP[i,1]
      PR.df.MaP[k,2]<- MaP[j,1]
      PR.df.MaP[k,3]<- l.output[[1]]
      PR.df.MaP[k,4]<- l.output[[2]]
      k=k+1
      
    }
    
  }
}

#Calculate coefficient of variation
PR.df.MaP$COV <- PR.df.MaP$'SD of PR'/PR.df.MaP$'Mean of PR'
PR.df.MaP<-PR.df.MaP[order(PR.df.MaP$COV),]
write.csv(PR.df.MaP,"Materials&ProcPairs.csv")

#######################################################################################3
#########Calculating std deviation of price ratios for pairs in Producer Durables#########

PDlabels<- sapply(PD,function(x){ paste(x,".Adjusted",sep="")})
masterdata.PD<- masterdata[,PDlabels]
PR.df.PD<-data.frame(rep(NA,9045),rep(NA,9045),rep(0,9045),rep(0,9045))
colnames(PR.df.PD)<- c('Stock1','Stock2','SD of PR','Mean of PR')


k=1
for(i in 1:135){
  for(j in i:135){
    if (i != j){
      pairprices <- masterdata.PD[,c(i,j)]
      pairprices <- na.omit(pairprices)
      l.output<-l.pr(pairprices)
      PR.df.PD[k,1]<- PD[i,1]
      PR.df.PD[k,2]<- PD[j,1]
      PR.df.PD[k,3]<- l.output[[1]]
      PR.df.PD[k,4]<- l.output[[2]]
      k=k+1
      
    }
    
  }
}

#Calculate coefficient of variation
PR.df.PD$COV <- PR.df.PD$'SD of PR'/PR.df.PD$'Mean of PR'
PR.df.PD<-PR.df.PD[order(PR.df.PD$COV),]
write.csv(PR.df.PD,"ProducerDurablePairs.csv")

#########################################################################################
####Calculating std deviation of price ratios for pairs in Consumer Discretionary########

CDlabels<- sapply(CD,function(x){ paste(x,".Adjusted",sep="")})
masterdata.CD<- masterdata[,CDlabels]
PR.df.CD<-data.frame(rep(NA,16110),rep(NA,16110),rep(0,16110),rep(0,16110))
colnames(PR.df.CD)<- c('Stock1','Stock2','SD of PR','Mean of PR')


k=1
for(i in 1:180){
  for(j in i:180){
    if (i != j){
      pairprices <- masterdata.CD[,c(i,j)]
      pairprices <- na.omit(pairprices)
      l.output<-l.pr(pairprices)
      PR.df.CD[k,1]<- CD[i,1]
      PR.df.CD[k,2]<- CD[j,1]
      PR.df.CD[k,3]<- l.output[[1]]
      PR.df.CD[k,4]<- l.output[[2]]
      k=k+1
      
    }
    
  }
}

#Calculate coefficient of variation
PR.df.CD$COV <- PR.df.CD$'SD of PR'/PR.df.CD$'Mean of PR'
PR.df.CD<-PR.df.CD[order(PR.df.CD$COV),]
write.csv(PR.df.CD,"ConsumerDiscretionaryPairs.csv")

########################################################################################
####Calculating std deviation of price ratios for pairs in Technology##################

Techlabels<- sapply(Tech,function(x){ paste(x,".Adjusted",sep="")})
masterdata.Tech<- masterdata[,Techlabels]
PR.df.Tech<-data.frame(rep(NA,6903),rep(NA,6903),rep(0,6903),rep(0,6903))
colnames(PR.df.Tech)<- c('Stock1','Stock2','SD of PR','Mean of PR')


k=1
for(i in 1:118){
  for(j in i:118){
    if (i != j){
      pairprices <- masterdata.Tech[,c(i,j)]
      pairprices <- na.omit(pairprices)
      l.output<-l.pr(pairprices)
      PR.df.Tech[k,1]<- Tech[i,1]
      PR.df.Tech[k,2]<- Tech[j,1]
      PR.df.Tech[k,3]<- l.output[[1]]
      PR.df.Tech[k,4]<- l.output[[2]]
      k=k+1
      
    }
    
  }
}

#Calculate coefficient of variation
PR.df.Tech$COV <- PR.df.Tech$'SD of PR'/PR.df.Tech$'Mean of PR'
PR.df.Tech<-PR.df.Tech[order(PR.df.Tech$COV),]
write.csv(PR.df.Tech,"TechPairs.csv")

########################################################################################
####Calculating std deviation of price ratios for pairs in Financials##################

Finlabels<- sapply(Fin,function(x){ paste(x,".Adjusted",sep="")})
masterdata.Fin<- masterdata[,Finlabels]
PR.df.Fin<-data.frame(rep(NA,24976),rep(NA,24976),rep(0,24976),rep(0,24976))
colnames(PR.df.Fin)<- c('Stock1','Stock2','SD of PR','Mean of PR')


k=1
for(i in 1:224){
  for(j in i:224){
    if (i != j){
      pairprices <- masterdata.Fin[,c(i,j)]
      pairprices <- na.omit(pairprices)
      l.output<-l.pr(pairprices)
      PR.df.Fin[k,1]<- Fin[i,1]
      PR.df.Fin[k,2]<- Fin[j,1]
      PR.df.Fin[k,3]<- l.output[[1]]
      PR.df.Fin[k,4]<- l.output[[2]]
      k=k+1
      
    }
    
  }
}

#Calculate coefficient of variation
PR.df.Fin$COV <- PR.df.Fin$'SD of PR'/PR.df.Fin$'Mean of PR'
PR.df.Fin<-PR.df.Fin[order(PR.df.Fin$COV),]
write.csv(PR.df.Fin,"FinancialsPairs.csv")


###########################################################################################
#######Calculating std deviation of price ratios for pairs in Consumer Staple##################
CSlabels<- sapply(CS,function(x){ paste(x,".Adjusted",sep="")})
masterdata.CS<- masterdata[,CSlabels]
PR.df.CS<-data.frame(rep(NA,1176),rep(NA,1176),rep(0,1176),rep(0,1176))
colnames(PR.df.CS)<- c('Stock1','Stock2','SD of PR','Mean of PR')


k=1
for(i in 1:49){
  for(j in i:49){
    if (i != j){
      pairprices <- masterdata.CS[,c(i,j)]
      pairprices <- na.omit(pairprices)
      l.output<-l.pr(pairprices)
      PR.df.CS[k,1]<- CS[i,1]
      PR.df.CS[k,2]<- CS[j,1]
      PR.df.CS[k,3]<- l.output[[1]]
      PR.df.CS[k,4]<- l.output[[2]]
      k=k+1
      
    }
    
  }
}

#Calculate coefficient of variation
PR.df.CS$COV <- PR.df.CS$'SD of PR'/PR.df.CS$'Mean of PR'
PR.df.CS<-PR.df.CS[order(PR.df.CS$COV),]
write.csv(PR.df.CS,"ConsumerStaplesPairs.csv")

######################################################################################
#######Calculating std deviation of price ratios for pairs in Utilities##################
Utilitieslabels<- sapply(Utilities,function(x){ paste(x,".Adjusted",sep="")})
masterdata.Utilities<- masterdata[,Utilitieslabels]
PR.df.Utilities<-data.frame(rep(NA,1540),rep(NA,1540),rep(0,1540),rep(0,1540))
colnames(PR.df.Utilities)<- c('Stock1','Stock2','SD of PR','Mean of PR')


k=1
for(i in 1:56){
  for(j in i:56){
    if (i != j){
      pairprices <- masterdata.Utilities[,c(i,j)]
      pairprices <- na.omit(pairprices)
      l.output<-l.pr(pairprices)
      PR.df.Utilities[k,1]<- Utilities[i,1]
      PR.df.Utilities[k,2]<- Utilities[j,1]
      PR.df.Utilities[k,3]<- l.output[[1]]
      PR.df.Utilities[k,4]<- l.output[[2]]
      k=k+1
      
    }
    
  }
}

#Calculate coefficient of variation
PR.df.Utilities$COV <- PR.df.Utilities$'SD of PR'/PR.df.Utilities$'Mean of PR'
PR.df.Utilities<-PR.df.Utilities[order(PR.df.Utilities$COV),]
write.csv(PR.df.Utilities,"UtilitiesPairs.csv")

