# evalution script forests
#1- ----------------------load/install libraries ----------------------

# load required packages into memory
#install.packages('pacman')
library(pacman)

packages=c('rpart','rpart.plot','randomForest','RColorBrewer','AUC','doParallel','dplyr',
           'ggplot2','lattice','ggthemes','grid','gridExtra','plotrix','ggpubr')
p_load(packages,character.only = TRUE)

#--------------------------------------------------

rm(list=ls())
setwd('/Users/sarajahfari/Documents/Github/Pearl3T/RF/RF_forests/splitdiff3_noconf/')

ev=list.files()[grep('_onetraintest',list.files())]
event=c('low','med','high')
Type=c('ALL','good')
Gbar_R2R1=data.frame() # evaluated for the good
Abar_R2R1=data.frame() # evaluated for all ppn
Ranking_type=list()



for (t in Type){
  Ranking_group=list()
  
  for (M in event){

    control=ev[grep(M,ev)]
    select=control[grep(t,control)]
    #select=control[grep(Type[2],ev)]
    
    TAUC=matrix(NA,ncol=3)
    Tname=data.frame()
    Taccuracy=data.frame()
    TPR=matrix(NA,ncol=3)
    
    #number=c(1,7:10)
    
    for(i in 1:length(select)){
      load(select[i])
      print(i)
      print(table(AUC))
      TAUC=rbind(AUC,TAUC)
      TPR=rbind(PREDICT,TPR)
      Tname=rbind(IMP.name,Tname)
      Taccuracy=rbind(IMP.Daccuracy,Taccuracy)
      rm(list=c('AUC','IMP.Daccuracy','IMP.name','PREDICT'))
    }
    


    # look at average prediction rate and min, max
    MTRY5_Predict=list()
    MTRY5_Predict$all=TPR[!is.na(TPR[,1]),]
    # original model files are not copied correctly! Check!!!!!
    
    MTRY5_Predict$mean=round(apply(MTRY5_Predict$all,2,mean),digits=2)
    MTRY5_Predict$max=round(apply(MTRY5_Predict$all,2,max),digits=2)
    MTRY5_Predict$min=round(apply(MTRY5_Predict$all,2,min),digits=2)
    MTRY5_Predict$sd=round(apply(MTRY5_Predict$all,2,sd),digits=5)
    #plot(density(MTRY5_Predict$all[,2]),xlim=c(0.5,0.75),ylim=c(0,1000),col='green')
    #points(density(MTRY5_Predict$all[,3]),type='l',col='red')

    # look at area under the curve
    MTRY5_AUC=list()
    MTRY5_AUC$AUC=TAUC[!is.na(TAUC[,1]),]
    MTRY5_AUC$mean=round(apply(MTRY5_AUC$AUC,2,mean),digits=2)
    MTRY5_AUC$max=round(apply(MTRY5_AUC$AUC,2,max),digits=2)
    MTRY5_AUC$min=round(apply(MTRY5_AUC$AUC,2,min),digits=2)
    MTRY5_AUC$sd=round(apply(MTRY5_AUC$AUC,2,sd),digits=2)
    #plot(density(MTRY5_AUC$AUC[,1]))
    
    
    set=data.frame(Type=rep(M,length(MTRY5_AUC$AUC[,1])),
                   ACC=MTRY5_Predict$all[,'pTest'],
                   rACC=MTRY5_Predict$all[,'prandomTest'],
                   AUC=MTRY5_AUC$AUC[,'roc'],
                   Sen=MTRY5_AUC$AUC[,'sensitivity'],
                   Spe=MTRY5_AUC$AUC[,'specifity']
                   )
    
    if (t=='good') Gbar_R2R1=rbind(set,Gbar_R2R1) else
    if (t=='ALL') Abar_R2R1=rbind(set,Abar_R2R1)
    
    
    # now look at raning
    
    IMP.name=na.omit(Tname)
    IMP.Daccuracy=na.omit(Taccuracy)
    Rank=data.frame(Region=rep(NA,dim(IMP.name)[2]),Prob=rep(NA,dim(IMP.name)[2]))
    
    for (iN in 1:dim(IMP.name)[2]){
      count=table(IMP.name[,iN])
      REgion=names(count)[which(count==max(count))]
      
      if(iN>1){
        # check if region is not already in the list above
        while (REgion%in%Rank$Region){
          count=count[-which(names(count)%in%Rank$Region)]
          REgion=names(count)[which(count==max(count))]
        }
      }
      Rank[iN,]=c(REgion,count[REgion])
    }
    
    
    
    forest.rank=which(apply(IMP.name, 1, function(x) all(x == Rank$Region)))
    Region_dAccruacy=data.frame(region=Rank[-1,1],Accuracy=colMeans(IMP.Daccuracy[forest.rank,-1]))
    
    Ranking_group[[M]]=Region_dAccruacy
    
  }

names(Ranking_group)=event
Ranking_type[[t]]=Ranking_group

}


names(Ranking_type)=Type


# data.frame(Ranking_type$good$high,Ranking_type$ALL$high)
# data.frame(Ranking_type$good$low,Ranking_type$ALL$low)
# data.frame(low=Ranking_type$good$low[,'region'],med=Ranking_type$good$med[,'region'],
#            high=Ranking_type$good$high[,'region'])

data.frame(Hard=Ranking_type$ALL$low[,'region'],Middle=Ranking_type$ALL$med[,'region'],
           Easy=Ranking_type$ALL$high[,'region'])


data.frame(Hard=Ranking_type$ALL$low[,'Accuracy'],Middle=Ranking_type$ALL$med[,'Accuracy'],
           Easy=Ranking_type$ALL$high[,'Accuracy'])

# 
# data.frame(Hard=Ranking_type$good$low[,'region'],Middle=Ranking_type$good$med[,'region'],
#            Easy=Ranking_type$good$high[,'region'])
# 
# 
# data.frame(Hard=Ranking_type$good$low[,'Accuracy'],Middle=Ranking_type$good$med[,'Accuracy'],
#            Easy=Ranking_type$good$high[,'Accuracy'])

# prepare R2R plot revision#
#https://rpkgs.datanovia.com/ggpubr/reference/ggbarplot.html

#-----------------------------------------------------------------------------#

# OriginalwEx.Good=
# Gbar_R2R1 %>% 
#   group_by(Type)%>% 
#     summarize(M_acc = mean(ACC, na.rm = TRUE),
#               M_racc = mean(rACC, na.rm = TRUE),
#               M_auc = mean(AUC, na.rm = TRUE),
#               M_sens= mean(Sen, na.rm = TRUE),
#               M_spe= mean(Spe, na.rm = TRUE),
#               group='Good')

OriginalwEx.ALL=
  Abar_R2R1 %>%
  group_by(Type)%>%
  summarize(M_acc = mean(ACC, na.rm = TRUE),
            M_racc = mean(rACC, na.rm = TRUE),
            M_auc = mean(AUC, na.rm = TRUE),
            M_sens= mean(Sen, na.rm = TRUE),
            M_spe= mean(Spe, na.rm = TRUE),
            group='ALL')
# 
# rbind(OriginalwEx.Good,OriginalwEx.ALL)->OriginalwEx


#-----------------------------------------------------------------------------#
#               Now start plotting                                            #
#-----------------------------------------------------------------------------#
#pdf('traintest_slice.pdf',3,2.2)
# prediction part

pdf('Q_diffslice_sliceeasy.pdf',3,2.2)
#OriginalwEx.ALL[OriginalwEx.ALL$Type=='high','M_acc']
# train test set
slices <- c(OriginalwEx.ALL[OriginalwEx.ALL$Type=='high','M_acc'],1-OriginalwEx.ALL[OriginalwEx.ALL$Type=='high','M_acc'])
lbls <- c('Correct','Incorrect')
pie3D(as.numeric(slices),labels=lbls,explode=0.1,radius=1,
      main="",col=c('darkgreen','darkred'))
dev.off()

pdf('Q_diffslice_slicehard.pdf',3,2.2)
#OriginalwEx.ALL[OriginalwEx.ALL$Type=='low','M_acc']
# train test set
slices <- c(OriginalwEx.ALL[OriginalwEx.ALL$Type=='low','M_acc'],1-OriginalwEx.ALL[OriginalwEx.ALL$Type=='high','M_acc'])
lbls <- c('Correct','Incorrect')
pie3D(as.numeric(slices),labels=lbls,explode=0.1,radius=1,
      main="",col=c('darkgreen','darkred'))
dev.off()





# ranking part
Rank_easy = transform(Ranking_type$ALL$high, 
                      region = reorder(region, Accuracy))

Rank_hard = transform(Ranking_type$ALL$low, 
                      region = reorder(region, Accuracy))
#dev.new()
R.easy=ggplot(data=Rank_easy, aes(x=region, y=Accuracy)) + 
  #ggtitle('easy choices')+
  ylab("Mean Decrease Accuracy")+xlab("")+
  geom_bar(stat="identity",fill="black",alpha=1,width=.9)+ 
  coord_flip()+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
        legend.position = "none")

R.hard=ggplot(data=Rank_hard, aes(x=region, y=Accuracy)) + 
  #ggtitle('hard choices')+
  ylab("Mean Decrease Accuracy")+xlab("")+
  geom_bar(stat="identity",fill="black",alpha=1,width=.9)+ 
  coord_flip()+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
        legend.position = "none")

pdf('Q_diffranking_easyhard.pdf',6,2.2)
grid.arrange(R.easy,R.hard,nrow=1)
dev.off()

#-----------------------------------------------------------#
# make slice plot 
# pdf('traintest_slice.pdf',3,2.2)
# train test set
slices <- c(0.7,0.3)
lbls <- c("Train", "Test")
pie3D(slices,labels=lbls,explode=0.1,
      main="",col=c('darkgreen','darkred'))
# dev.off()

# make slice plot 
# pdf('PredictTestcorrect_slice.pdf',3,2.2)
# train test set
slices <- c(MTRY5_Predict$mean[2],1-MTRY5_Predict$mean[2])
lbls <- c('Correct','Incorrect')
pie3D(slices,labels=lbls,explode=0.1,radius=1,
      main="",col=c('darkgreen','darkred'))
# dev.off()




