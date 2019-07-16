#------------------------set-up script to run 1000 RFs with different seed to grasp variability ---------------#
rm(list=ls())
#1- ----------------------load/install libraries ----------------------
# load required packages into memory

library(pacman)

packages=c('rpart','rpart.plot','randomForest','RColorBrewer','AUC','doParallel','dplyr',
           'ggplot2','lattice','ggthemes','grid','gridExtra','plotrix','ggpubr')
p_load(packages,character.only = TRUE)

#2- ----------------------load data, define predictors ----------------------

#---------------------------------------------------------------------#
# define test and training set
#set.seed(218)
set.seed(218)

load('/Users/sarajahfari/Documents/Github/Pearl3T/Data/RF_data/forestDat_N43_Rev.Rdat')
#setwd('~/Documents/Pearl3T/NEW/RF/RF_data')
#load('~/Documents/Pearl3T/NEW/RF/RF_data/forestDat_N43_Rev.Rdat')

forestinall=forestinallR2R
rm(forestinallR2R)

forestinall$conflict=droplevels(forestinall)$conflict
forestinall$option=droplevels(forestinall)$option
forestinall$response=ifelse(forestinall$response==0,'incorrect','correct')

#------------- define on subject level Q_diff group in low, med, high ------------------------#
Nforestinall=data.frame()

id=levels(forestinall$subject_id)

for (sub in id)
{
  data=forestinall[forestinall$subject_id==sub,]
  L=floor(length(data$Q.diff)/3)
  low=1:L; med={L+1}:{2*L}; high=(last(med)+1):length(data$Q.diff)
  
  data=data.frame(data,Q_abs=abs(data$Q.diff))
  
  # Divide Q-diff based on absolute values 
  # note (if you not use abs you are giving away the answer to RF, Q<0 is wrong choice then)
  Q_group=rep(NA,length=length(data$Q.diff))
  
  addlow=ifelse(data$Q_abs%in%sort(data$Q_abs)[low],'low',Q_group)
  addmed=ifelse(data$Q_abs%in%sort(data$Q_abs)[med],'med',addlow)
  Q_Gr_abs=ifelse(data$Q_abs%in%sort(data$Q_abs)[high],'high',addmed)
  
  sub_data=data.frame(data,Q_Gr_abs=Q_Gr_abs)
  Nforestinall=rbind(sub_data,Nforestinall)
  
  rm(list=c('Q_group','Q_Gr_abs','addlow','addmed','data','L','low','med','high','sub_data'))
  
}


CBase=c("response","subject_id",
         "Caudate40exc","Putamen40exc","Accumbens",
         "FFA23","V1","VMPFC31thr","MotorBA4",
        "DLPFCposterior","preSMAsmall",'Q.diff')


Type='ALL'
SEEDS=100
PROX='FALSE'
MTRy=5
NTREE=2000

deltaQ=c('low','med','high')

DeltaQ='high'



#---------------------------------------------------------------------#

if (DeltaQ=='low') {forestinall=Nforestinall[Nforestinall$Q_Gr_abs=='low',]} else
  if (DeltaQ=='med') {forestinall=Nforestinall[Nforestinall$Q_Gr_abs=='med',]} else
    if (DeltaQ=='high') {forestinall=Nforestinall[Nforestinall$Q_Gr_abs=='high',]}


if (Type=='ALL') {Forestinall=forestinall[,CBase]} else
  if (Type=='good') {Forestinall=forestinall[forestinall$Learner=='good',CBase]} else
    if (Type=='bad') {Forestinall=forestinall[forestinall$Learner=='bad',CBase]}

#Forestinall=forestinall[,CBase]
samp=sample(1:dim(Forestinall)[1]) # get shuffle index
test=Forestinall[samp[1:floor(0.3*length(samp))],] # define test set (1/3 of trials)
train=Forestinall[-samp[1:floor(0.3*length(samp))],] # define train set (2/3 of trials)
#4.1 sample test and training set
# get number of counts minority class for stratification
nRareSamples = min(table(as.factor(train$response)))-1

# make a random test set to assess basline prediction/shuffle responses
RandomTest=test
RandomTest$response=sample(RandomTest$response)
#
# 	#4.0 set seed
# 	set.seed(218)
#     track=track+1

	# run forest
    rf.strata = randomForest(as.factor(response) ~ .,
        data=train[,-which(colnames(train)=='Q.diff')],strata=train$response,
             sampsize=c(nRareSamples,nRareSamples),
             ntree=NTREE,mtry=MTRy,importance=TRUE)

    ### Check how well these complex model behave
    # # for stratified data
    Backtesting<- predict(rf.strata, train)
    Prediction <- predict(rf.strata, test)
    RPrediction <- predict(rf.strata, RandomTest)
    ptr<-length(which(Backtesting==train$response))/length(train$response)
    pts<-length(which(Prediction==test$response))/length(test$response)
    prts<-length(which(RPrediction==RandomTest$response))/length(test$response)
    #
#p.easy=    round(c(ptr,pts,prts),digits=2)
p.easy=    round(c(ptr,pts,prts),digits=2)



easy=round(c(
      	auc(roc(rf.strata$votes[,2],as.factor(train$response))),
      	auc(specificity(rf.strata$votes[,2],as.factor(train$response))),
      	auc(sensitivity(rf.strata$votes[,2],as.factor(train$response)))
      	),digits=2)

# # 
# #     # now get importance of each region
    im2=as.matrix(importance(rf.strata,scale=F,type=1))
    imp2.sort =  as.matrix(im2[order(-im2[,'MeanDecreaseAccuracy']),])
    iMP2=data.frame(rownames(imp2.sort),imp2.sort[,1])

imp.easy=iMP2
    

# imp.hard
# hard

Imp_easy=data.frame(region=imp.easy[-1,1],importance=imp.easy[-1,2])
Imp_hard=data.frame(region=imp.hard[-1,1],importance=imp.hard[-1,2])

AUC=rbind(easy,hard)
colnames(AUC)=c('AUC','specificity','sensitivity')

PREDICT=rbind(p.easy,p.hard)
colnames(PREDICT)=c('train','test','test_random')

IMP.splitdiff.all=list(Imp_easy,Imp_hard)
names(IMP.splitdiff.all)=c('easy','hard')

save(list=c('PREDICT','AUC','IMP.splitdiff.all'),file='~/Documents/Github/Pearl3T/RF/RF_ranking/splitdiff_Seed218_all.Rdat')


