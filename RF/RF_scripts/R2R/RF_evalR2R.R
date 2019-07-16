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


setwd('/Users/sarajahfari/Documents/Github/Pearl3T/RF/RF_forests/SeperateClusters/')
load('R2Rcheck_ntree2000_MTRYdefaulttype_good_proxFALSE.Rdat')

Good_control=list(PREDICT=PREDICT, AUC=AUC,IMP.name=IMP.name,IMP.Daccuracy=IMP.Daccuracy)
rm(list=c('PREDICT', 'AUC','IMP.name','IMP.Daccuracy','conf'))

load('R2Rcheck_ntree2000_MTRYdefaulttype_ALL_proxFALSE.Rdat')
All_control=list(PREDICT=PREDICT, AUC=AUC,IMP.name=IMP.name,IMP.Daccuracy=IMP.Daccuracy)
rm(list=c('PREDICT', 'AUC','IMP.name','IMP.Daccuracy','conf'))


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
          add.params = list(group = "group",color='grey'),
          xlim=c(0.5,0.8),
          position=position_dodge(0.8),
          palette= "paired",
          fill="group",
          ylab=FALSE,
          xlab='P(correct)',
          main='Random Forest alternatives',
          order = c("Original", "Ex.FFA", "Ex.Perception","Ex.Putamen", "Ex.Dorsal"),
          orientation='horizontal'
          )






#--------- prepare ranking plot ----------------------------#
pdf('Good_1000it_novisgood_mTRY52000.pdf',3,2.2)
imp.sort2 = transform(Region_dAccruacy, 
                      region = reorder(region, Accuracy))
#dev.new()
ggplot(data=imp.sort2, aes(x=region, y=Accuracy)) + 
  #ggtitle(title=paste('classic structural', "prediction test",
  #   round(eval.models.allsub['predict_test','brainSTmin'],digits=2)))
  ylab("Mean Decrease Accuracy")+xlab("")+
  geom_bar(stat="identity",fill="black",alpha=1,width=.9)+ 
  coord_flip()+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
        legend.position = "none")
dev.off()

#-----------------------------------------------------------#
# make slice plot 
pdf('traintest_slice.pdf',3,2.2)
# train test set
slices <- c(0.7,0.3)
lbls <- c("Train", "Test")
pie3D(slices,labels=lbls,explode=0.1,
      main="",col=c('darkgreen','darkred'))
dev.off()

# make slice plot 
pdf('PredictTestcorrect_slice.pdf',3,2.2)
# train test set
slices <- c(MTRY5_Predict$mean[2],1-MTRY5_Predict$mean[2])
lbls <- c('Correct','Incorrect')
pie3D(slices,labels=lbls,explode=0.1,radius=1,
      main="",col=c('darkgreen','darkred'))
dev.off()



#-----------make and plot confusion matrix---------------------#
pdf('confusion_matrix_novisgood.pdf')
CONF=conf[forest.rank[1:5]]
confusion_matrix <- CONF[[1]][,1:2]
pconf<- CONF[[1]][,3]

layout(matrix(c(1,1,2)))
par(mar=c(2,2,2,2))
plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
title('CONFUSION MATRIX', cex.main=2)

# create the matrix 
rect(150, 430, 240, 370, col='darkgreen')
text(195, 435, 'Correct', cex=2, font=3)
rect(250, 430, 340, 370, col='darkred')
text(295, 435, 'Incorrect', cex=2, font=3)
text(125, 370, 'Predicted', cex=3, srt=90, font=3)
text(245, 440, 'Actual', cex=3, font=3)
rect(150, 305, 240, 365, col='darkred')
rect(250, 305, 340, 365, col='darkgreen')
text(140, 400, 'Correct', cex=2, font=3, srt=90)
text(140, 335, 'Incorrect', cex=2, font=3, srt=90)


res <- as.numeric(confusion_matrix)
text(195, 400, paste(paste0(res[1],','),paste0(round(1-pconf[1],digits=2)*100,'%')), cex=2, font=2, col='white')
text(195, 335, paste(paste0(res[2],','),paste0(round(pconf[2],digits=2)*100,'%')), cex=2, font=2, col='white')
text(295, 400, paste(paste0(res[3],','),paste0(round(pconf[1],digits=2)*100,'%')), cex=2, font=2, col='white')
text(295, 335, paste(paste0(res[4],','),paste0(round(1-pconf[2],digits=2)*100,'%')), cex=2, font=2, col='white')


cm=confusion_matrix
# add in the specifics 
plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n', cex.main=2)
text(15, 30, 'AUC', cex=3, font=3)
text(15, 8, MTRY5_AUC$mean[1], cex=2.5)
text(50, 30, 'sensitivity', cex=3, font=3)
text(50, 8, MTRY5_AUC$mean[3], cex=2.5)
text(85, 30, 'specificity', cex=3, font=3)
text(85, 8, MTRY5_AUC$mean[2], cex=2.5)

# add in the accuracy information 
text(30, 85, 'Validation', cex=3, font=3)
text(30, 60, MTRY5_Predict$mean[2], cex=2.5)
text(70, 85, 'Randomised', cex=3, font=3)
text(70, 60, MTRY5_Predict$mean[3], cex=2.5)

dev.off()
