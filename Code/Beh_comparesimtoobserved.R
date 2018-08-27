#setwd("~/Dropbox/Projects/Ciras/plots_stan/workfiles_new")



created_simvsobs=function(obs.learn,simulated,NBINS)	
{
  
  # prepare matrix file to summarize observations 
  oScore=matrix(,length(obs.learn),3*NBINS)
  colnames(oScore)=c(paste('oAb',1:NBINS,sep=''),
                           paste('oCd',1:NBINS,sep=''),paste('oEf',1:NBINS,sep=''))
  rownames(oScore)=names(obs.learn)
  
  sScore=matrix(,length(obs.learn),3*NBINS)
  colnames(sScore)=c(paste('sAb',1:NBINS,sep=''),
                           paste('sCd',1:NBINS,sep=''),paste('sEf',1:NBINS,sep=''))
  
    for (p in 1:length(obs.learn))
      {
        
           # obs.learn divide ab, cd and ef for subject
           oAB=obs.learn[[p]][obs.learn[[p]][,1]==1,2]
           oCD=obs.learn[[p]][obs.learn[[p]][,1]==3,2]
           oEF=obs.learn[[p]][obs.learn[[p]][,1]==5,2]
           
           # Simulated divide ab, cd and ef for subject
           sAB= simulated[[p]][simulated[[p]][,1]==1,2]
           sCD= simulated[[p]][simulated[[p]][,1]==3,2]
           sEF= simulated[[p]][simulated[[p]][,1]==5,2]
           
           # ok now start making bins
           
           # get number of trials subjects and divide by NBINS
           BinNtr=c(floor(table(obs.learn[[p]][,1])/NBINS),floor(table(simulated[[p]][,1])/NBINS))
           names(BinNtr)=paste(rep(c('odv','sdv'),each=3),rep(c('AB','CD','EF'),2),sep='')
           
           bindex_oAB=seq(1,length(oAB)-1,BinNtr['odvAB'])
           eindex_oAB=seq(0,length(oAB)+1,BinNtr['odvAB'])[-1]
           bindex_oCD=seq(1,length(oCD)-1,BinNtr['odvCD'])
           eindex_oCD=seq(0,length(oCD)+1,BinNtr['odvCD'])[-1]
           bindex_oEF=seq(1,length(oEF)-1,BinNtr['odvEF'])
           eindex_oEF=seq(0,length(oEF)+1,BinNtr['odvEF'])[-1]
           
           bindex_sAB=seq(1,length(sAB)-1,BinNtr['sdvAB'])
           eindex_sAB=seq(0,length(sAB)+1,BinNtr['sdvAB'])[-1]
           bindex_sCD=seq(1,length(sCD)-1,BinNtr['sdvCD'])
           eindex_sCD=seq(0,length(sCD)+1,BinNtr['sdvCD'])[-1]
           bindex_sEF=seq(1,length(sEF)-1,BinNtr['sdvEF'])
           eindex_sEF=seq(0,length(sEF)+1,BinNtr['sdvEF'])[-1]
           
           oscore.AB=c();oscore.CD=c();oscore.EF=c()
           sscore.AB=c();sscore.CD=c();sscore.EF=c()
           
           for (seq in 1:NBINS)
            {
              
              oscore.AB[seq]=length(which(oAB[bindex_oAB[seq]:eindex_oAB[seq]]==0))
              oscore.CD[seq]=length(which(oCD[bindex_oCD[seq]:eindex_oCD[seq]]==0))
              oscore.EF[seq]=length(which(oEF[bindex_oEF[seq]:eindex_oEF[seq]]==0))
              
              sscore.AB[seq]=length(which(sAB[bindex_sAB[seq]:eindex_sAB[seq]]==0))
              sscore.CD[seq]=length(which(sCD[bindex_sCD[seq]:eindex_sCD[seq]]==0))
              sscore.EF[seq]=length(which(sEF[bindex_sEF[seq]:eindex_sEF[seq]]==0))
                }
          
          Oscore=c(oscore.AB/BinNtr['odvAB'], oscore.CD/BinNtr['odvCD'], oscore.EF/BinNtr['odvEF'])
          Sscore=c(sscore.AB/BinNtr['sdvAB'], sscore.CD/BinNtr['sdvCD'], sscore.EF/BinNtr['sdvEF'])
    
    
          oScore[p,]=Oscore
          sScore[p,]=Sscore
     }
  
	Results=round(cbind(oScore,sScore),digits=2)
	rownames(Results)=names(obs.learn)
	
	check=c()
	p.bin=c()
	for (t in 1:{NBINS*3}){
	  p.bin[t]=t.test(Results[,t],Results[,t+{NBINS*3}],paired=T)$p.value
	  check[t]=ifelse(t.test(Results[,t],Results[,t+{NBINS*3}],paired=T)$p.value<0.05,
	         paste(t,t+{NBINS*3}, 'cols differ between obs sim'),'ok')
	  }

  mean.bins=colMeans(Results)
  se.bins=apply(Results,2,sd)/sqrt(length(Results[,1]))
  
  Bin.list=list(Compare43=Results,Meancompare=mean.bins,SEcompare=se.bins,p.ttest.bins=p.bin,check=check)
  return(Bin.list)
  }

#created_simvsobs(obs.learn,simulated,NBINS)	 
  