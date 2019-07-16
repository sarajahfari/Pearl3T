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

load(file='~/Documents/Github/Pearl3T/RF/RF_ranking/splitdiff_Seed218_all.Rdat')


#-----------------------------------------------------------------------------#
#               Now start plotting                                            #
#-----------------------------------------------------------------------------#


pdf('Q_diffslice_sliceeasy.pdf',3,2.2)

# train test set
slices <- c(PREDICT[1,2],1-PREDICT[1,2])
lbls <- c('Correct','Incorrect')
pie3D(as.numeric(slices),labels=lbls,explode=0.1,radius=1,
      main="",col=c('darkgreen','darkred'))
dev.off()

pdf('Q_diffslice_slicehard.pdf',3,2.2)

# train test set
slices <- c(PREDICT[2,2],1-PREDICT[2,2])
lbls <- c('Correct','Incorrect')
pie3D(as.numeric(slices),labels=lbls,explode=0.1,radius=1,
      main="",col=c('darkgreen','darkred'))
dev.off()





# ranking part
Rank_easy = transform(IMP.splitdiff.all$easy, 
                      region = reorder(region, importance))

Rank_hard = transform(IMP.splitdiff.all$hard, 
                      region = reorder(region, importance))
#dev.new()
R.easy=ggplot(data=Rank_easy, aes(x=region, y=importance)) + 
  ggtitle('easy choices')+
  ylab("Mean Decrease Accuracy")+xlab("")+
  ylim(-0.0006,0.009)+
  geom_bar(stat="identity",fill="black",alpha=1,width=.9)+ 
  coord_flip()+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
        legend.position = "none")

R.hard=ggplot(data=Rank_hard, aes(x=region, y=importance)) + 
  ggtitle(paste('hard choices'))+
  ylab("Mean Decrease Accuracy")+xlab("")+
  ylim(-0.0006,0.009)+
  geom_bar(stat="identity",fill="black",alpha=1,width=.9)+ 
  coord_flip()+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
        legend.position = "none")

pdf('Q_diffranking_easyhard.pdf',6,2.2)
grid.arrange(R.easy,R.hard,nrow=1)
dev.off()






