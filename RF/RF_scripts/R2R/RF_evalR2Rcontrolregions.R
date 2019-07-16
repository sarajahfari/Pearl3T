# evalution script forests
#1- ----------------------load/install libraries ----------------------
# load required packages into memory
#install.packages('pacman')
library(pacman)

packages=c('rpart','rpart.plot','randomForest','RColorBrewer','AUC','doParallel','dplyr',
           'ggplot2','lattice','ggthemes','grid','gridExtra','plotrix','ggpubr')
p_load(packages,character.only = TRUE)

#---------------------------------------------------------------------------------#
#                     

rm(list=ls())


setwd('/Users/sarajahfari/Documents/Github/Pearl3T/RF/RF_forests/SeperateClusters/')
load('R2Rcheck_ntree2000_MTRYdefaulttype_good_proxFALSE.Rdat')
#load('R2RcheckID_ntree2000_MTRYdefaulttype_good_proxFALSE.Rdat')

Good_control=list(PREDICT=PREDICT, AUC=AUC)
rm(list=c('PREDICT', 'AUC','IMP.Daccuracy','IMP.name','conf'))
load('R2Rcheck_ntree2000_MTRYdefaulttype_ALL_proxFALSE.Rdat')
All_control=list(PREDICT=PREDICT, AUC=AUC)
rm(list=c('PREDICT', 'AUC','IMP.Daccuracy','IMP.name','conf'))

GC=data.frame(
  Good_control$PREDICT[rownames(Good_control$PREDICT)%in%c('vision','striatum','frontal'),
                       c('pTest','prandomTest')],
  roc=Good_control$AUC[rownames(Good_control$AUC)%in%c('vision','striatum','frontal'),
                       'roc'],
  group='Good',
  Type=c('vision','striatum','frontal'))
  
AC=data.frame(
  All_control$PREDICT[rownames(All_control$PREDICT)%in%c('vision','striatum','frontal'),
                       c('pTest','prandomTest')],
  roc=All_control$AUC[rownames(All_control$AUC)%in%c('vision','striatum','frontal'),
                       'roc'],
  group='ALL',
  Type=c('vision','striatum','frontal'))


rbind(GC,AC)->InIsolation

#---------

setwd('/Users/sarajahfari/Documents/Github/Pearl3T/RF/RF_forests/R2Rc/')

ev=list.files()[grep('_onetraintest',list.files())]
out=paste0('block',1:5,'_')
for(o in out){ev=ev[-grep(o,ev)]}


event=c('_novis','_noffa','_onetraintest_ntree','exdorsal','exputamen')
Type=c('ALL','good')
Gbar_R2R1=data.frame() # evaluated for the good
Abar_R2R1=data.frame() # evaluated for all ppn

for (t in Type){
  

  
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
    }
}





# prepare R2R plot revision#
#https://rpkgs.datanovia.com/ggpubr/reference/ggbarplot.html

Gbar_R2R1$Type=droplevels(Gbar_R2R1$Type)

levels(Gbar_R2R1$Type)[levels(Gbar_R2R1$Type)=='_onetraintest_ntree'] <- "Original"
levels(Gbar_R2R1$Type)[levels(Gbar_R2R1$Type)=="_noffa"] <- "Ex.FFA"
levels(Gbar_R2R1$Type)[levels(Gbar_R2R1$Type)=="exdorsal"] <- "Ex.Dorsal"
levels(Gbar_R2R1$Type)[levels(Gbar_R2R1$Type)=="exputamen"] <- "Ex.Putamen"
levels(Gbar_R2R1$Type)[levels(Gbar_R2R1$Type)== "_novis"] <- "Ex.Perception"


Abar_R2R1$Type=droplevels(Abar_R2R1$Type)

levels(Abar_R2R1$Type)[levels(Abar_R2R1$Type)=='_onetraintest_ntree'] <- "Original"
levels(Abar_R2R1$Type)[levels(Abar_R2R1$Type)=="_noffa"] <- "Ex.FFA"
levels(Abar_R2R1$Type)[levels(Abar_R2R1$Type)=="exdorsal"] <- "Ex.Dorsal"
levels(Abar_R2R1$Type)[levels(Abar_R2R1$Type)=="exputamen"] <- "Ex.Putamen"
levels(Abar_R2R1$Type)[levels(Abar_R2R1$Type)== "_novis"] <- "Ex.Perception"




#-----------------------------------------------------------------------------#

OriginalwEx.Good=
Gbar_R2R1 %>% 
  group_by(Type)%>% 
    summarize(M_acc = mean(ACC, na.rm = TRUE),
              M_racc = mean(rACC, na.rm = TRUE),
              M_auc = mean(AUC, na.rm = TRUE),
              group='Good')

OriginalwEx.ALL=
  Abar_R2R1 %>% 
  group_by(Type)%>% 
  summarize(M_acc = mean(ACC, na.rm = TRUE),
            M_racc = mean(rACC, na.rm = TRUE),
            M_auc = mean(AUC, na.rm = TRUE),
            group='ALL')

rbind(OriginalwEx.Good,OriginalwEx.ALL)->OriginalwEx




ggbarplot(OriginalwEx, x = "Type", y = "M_acc",
          x.text.angle = 45,
          add.params = list(group = "group"),
          xlim=c(0.5,0.8),
          position=position_dodge(0.8),
          palette=  c("black", "darkgreen"),
          fill="group",
          xlab='Excluded region in RF',
          ylab='Accuracy RF on validation set',
          main='Random Forest alternatives',
          order = c(  "Ex.Dorsal","Ex.Putamen","Ex.Perception","Ex.FFA","Original"),
          
          orientation='horizontal'
          ) +  theme(
            plot.title = element_text(hjust = 0.5),
            legend.title=element_blank())




