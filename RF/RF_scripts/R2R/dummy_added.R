#------------------------set-up script to run 1000 RFs with different seed to grasp variability ---------------#
#------------------------set-up script to run 1000 RFs with different seed to grasp variability ---------------#
rm(list=ls())
#1- ----------------------load/install libraries ----------------------
# load required packages into memory

library(pacman)

packages=c('rpart','rpart.plot','randomForest','RColorBrewer','AUC','doParallel','dplyr',
           'ggplot2','lattice','ggthemes','grid','gridExtra','plotrix','ggpubr')
p_load(packages,character.only = TRUE)

#2- ----------------------load data, define predictors ----------------------

# define predictors
CBase=c("response","subject_id",'conflict',
        "Caudate40exc","Putamen40exc","Accumbens",
        "FFA23","V1","VMPFC31thr","MotorBA4",
        "DLPFCposterior","preSMAsmall",'Q.diff')

# this was initially used to find optimal MTry (outcome 5 or 6)
#tuneRF(select(Forestinall,-response), as.factor(Forestinall$response), stepFactor=2,ntree=2000,improve=0.01,plot=T,mtryStart=3)->tune

#3- ----------------------define variables to save resutls and number of seeds --
# variables
Type='good'
SEEDS=100
PROX='FALSE'
MTRy=5
NTREE=2000



setwd("/Users/sarajahfari/Documents/Github/Pearl3T")
#---------------------------------------------------------------------#
# define test and training set
set.seed(218)

#setwd('/Users/sarajahfari/Spinoza/NEW')
load('./Data/RF_data/forestDat_N43.Rdat')
#---------------------------------------------------------------------#

forestinall$conflict=droplevels(forestinall)$conflict
forestinall$option=droplevels(forestinall)$option
forestinall$response=ifelse(forestinall$response==0,'incorret','correct')

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


#rnorm(length(forestinall$preSMAsmall))->dummy_data



#---------------------------------------------------------------------#
if (Type=='good') set.seed(891) 
# this was out of 500 runs of 2000 DT's (i.e., 500 RF's with each another seed the most frequent ranking)
# this check is not in MS, but was done for consistency of ranking.

	# run forest
    rf.strata = randomForest(as.factor(response) ~ .,
        data=train[,-which(colnames(train)=='Q.diff')],strata=train$response,
             sampsize=c(nRareSamples,nRareSamples),
             ntree=NTREE,mtry=MTRy,importance=TRUE)
    
    # prediction of test, train, dummy test
    ### Check how well these complex model behave
    # # for stratified data
    Backtesting<- predict(rf.strata, train)
    Prediction <- predict(rf.strata, test)
    RPrediction <- predict(rf.strata, RandomTest)
    ptr<-length(which(Backtesting==train$response))/length(train$response)
    pts<-length(which(Prediction==test$response))/length(test$response)
    prts<-length(which(RPrediction==RandomTest$response))/length(test$response)
    
    #round(c(ptr,pts,prts),digits=2)
    # PREDICT[track,]=round(c(ptr,pts,prts),digits=2)
    
    # #     # now get importance of each region
    im2=as.matrix(importance(rf.strata,scale=T,type=1))
    imp2.sort =  as.matrix(im2[order(-im2[,'MeanDecreaseAccuracy']),])
    
    # in resubmission we use scale is F because this is recomended (we were unaware of this before)
    im.new=as.matrix(importance(rf.strata,scale=F,type=1))
    impn.sort =  as.matrix(im.new[order(-im.new[,'MeanDecreaseAccuracy']),])
    
   
    AUC=round(c(
      auc(roc(rf.strata$votes[,2],as.factor(train$response))),
      auc(specificity(rf.strata$votes[,2],as.factor(train$response))),
      auc(sensitivity(rf.strata$votes[,2],as.factor(train$response)))
    ),digits=2)
    
    
    # ---------------- ok now see how it works with a dummy column added
    
    #rnorm(length(train$preSMAsmall))->dummy_data
    Train=data.frame(train,dummy=dummy_data)
    #rm(dummy_data)
    Test=data.frame(test,dummy=rnorm(length(test$preSMAsmall)))
    TRandomTest=data.frame(RandomTest,dummy=rnorm(length(RandomTest$preSMAsmall)))
    
    # duplicate --------------------
    # drain=data.frame(train,dup_putamen=train$Putamen40exc)
    # dest=data.frame(test,dup_putamen=test$Putamen40exc)
    # dRandomTest=data.frame(RandomTest,dup_Putamen=test$Putamen40exc)
    # 
    # drain2=data.frame(train,dup_putamen=train$DLPFCposterior)
    # dest2=data.frame(test,dup_putamen=test$DLPFCposterior)
    # dRandomTest2=data.frame(RandomTest,dup_Putamen=test$DLPFCposterior)
    
    
    
    
     rf.strata.dummy = randomForest(as.factor(response) ~ .,
                             data=Train[,-which(colnames(Train)=='Q.diff')],strata=Train$response,
                             sampsize=c(nRareSamples,nRareSamples),
                             ntree=NTREE,mtry=MTRy,importance=TRUE)
     
     
     
     # prediction of test, train, dummy test
     ### Check how well these complex model behave
     # # for stratified data
     DBacktesting<- predict(rf.strata.dummy, Train)
     DPrediction <- predict(rf.strata.dummy, Test)
     DRPrediction <- predict(rf.strata.dummy, TRandomTest)
     Dptr<-length(which(DBacktesting==train$response))/length(train$response)
     Dpts<-length(which(DPrediction==test$response))/length(test$response)
     Dprts<-length(which(DRPrediction==RandomTest$response))/length(test$response)
     
     #round(c(Dptr,Dpts,Dprts),digits=2)
     # PREDICT[track,]=round(c(ptr,pts,prts),digits=2)
    
     # #     # now get importance of each region
     Dim2=as.matrix(importance(rf.strata.dummy,scale=T,type=1))
     Dimp2.sort =  as.matrix(Dim2[order(-Dim2[,'MeanDecreaseAccuracy']),])
     
     Dim.new=as.matrix(importance(rf.strata.dummy,scale=F,type=1))
     Dimpn.sort =  as.matrix(Dim.new[order(-Dim.new[,'MeanDecreaseAccuracy']),])
     
     

#---------------------------------------------------------------------#

     # make variables to save for reporting and plotting
     diff=c(ptr,pts,prts)-c(Dptr,Dpts,Dprts)
     allP_acc=rbind(round(c(Dptr,Dpts,Dprts),digits=3),
                 round(c(ptr,pts,prts),digits=3),
                 round(diff,digits=3))
     colnames(allP_acc)=c('train','test','random_test')
     rownames(allP_acc)=c('Random_var','no_Random','diff')   
     
      # plot ranking for dummy added
      
      dummy_all=data.frame(region=rownames(Dimpn.sort),importance=Dimpn.sort[,1]) 
      dummy_all=dummy_all[-c(1:2),]
      
      dummy_all=transform(dummy_all, 
                region = reorder(region, importance))
      
      # now the same with traditions
      RF_all=data.frame(region=rownames(impn.sort),importance=impn.sort[,1]) 
      RF_all=RF_all[-c(1:2),]
      
      RF_all=transform(RF_all, 
                        region = reorder(region, importance))
      
      save(list=c('dummy_all','RF_all','allP_acc'),
           file='~/Documents/Github/Pearl3T/RF/RF_ranking/all_dummynodummy.Rdat')


#----------------------------------------------------------------------------#
      
rm(list=ls())

# plotting
#load('~/Documents/Github/Pearl3T/RF/RF_ranking/Good_dummynodummy.Rdat')
#load('~/Documents/Github/Pearl3T/RF/RF_ranking/all_dummynodummy.Rdat')

rfg = ggplot(data=RF_good, aes(x=region, y=importance))+ 
  ggtitle(paste("Accuracy on validation", round(pts,digits=2)))+
  ylab("Mean Decrease Accuracy")+xlab("")+
  ylim(-0.0003,0.009)+
  geom_bar(stat="identity",fill="black",alpha=1,width=.9)+ 
  coord_flip()+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
        legend.position = "none")


dg = ggplot(data=dummy_good, aes(x=region, y=importance))+ 
  ggtitle(paste("Accuracy on validation=", round(Dpts,digits=2)))+
  ylab("Mean Decrease Accuracy")+xlab("")+
  ylim(-0.0003,0.009)+
  geom_bar(stat="identity",fill="black",alpha=1,width=.9)+ 
  coord_flip()+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
        legend.position = "none")


rfa = ggplot(data=RF_all, aes(x=region, y=importance))+ 
  ggtitle(paste("Accuracy on validation", round(pts,digits=2)))+
  ylab("Mean Decrease Accuracy")+xlab("")+
  ylim(-0.0003,0.009)+
  geom_bar(stat="identity",fill="black",alpha=1,width=.9)+ 
  coord_flip()+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
        legend.position = "none")


da = ggplot(data=dummy_all, aes(x=region, y=importance))+ 
  ggtitle(paste("Accuracy on validation=", round(Dpts,digits=2)))+
  ylab("Mean Decrease Accuracy")+xlab("")+
  ylim(-0.0003,0.009)+
  geom_bar(stat="identity",fill="black",alpha=1,width=.9)+ 
  coord_flip()+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
        legend.position = "none")





dev.new()
grid.arrange(rfg,dg,rfa,da,nrow=2,ncol=2)





#---------------- drop by colums evaluatie ---------#
     # 
     # columns=c("subject_id",'conflict',
     #           "Caudate40exc","Putamen40exc","Accumbens",
     #           "FFA23","V1","VMPFC31thr","MotorBA4",
     #           "DLPFCposterior","preSMAsmall","dummy")
     # 
     # rent=train
     # 
     # #"response", ,'Q.diff'
     # 
     # get_drop_imp <- function(rent, columns,nRareSamples){
     #   X <- rent[,c(columns, "response")] # data
     #   # randomForest(as.factor(response) ~ .,
     #   #              data=train[,-which(colnames(train)=='Q.diff')],strata=train$response,
     #   #              sampsize=c(nRareSamples,nRareSamples),
     #   #              ntree=NTREE,mtry=MTRy,importance=TRUE)
     #   
     #   
     #   rf <- randomForest(as.factor(response) ~., data = X,strata=train$response,
     #                      sampsize=c(nRareSamples,nRareSamples),
     #                      ntree = 2000, mtry=5, importance=T)
     #   
     #   full_rsq <- -1*mean(rf$err.rate) # 
     #   
     #   imp <- c()
     #   for (c in columns){
     #     X_sub <- X[, !(colnames(X) == c)]
     #     rf <- randomForest(as.factor(response) ~., data = X_sub, 
     #                        strata=train$response,
     #                        sampsize=c(nRareSamples,nRareSamples),
     #                         ntree = 2000, mtry=5, importance=T)
     #     
     #     sub_rsq <- -1*mean(rf$err.rate) # 
     #     diff_rsq <- full_rsq - sub_rsq
     #     imp <- c(imp, diff_rsq)
     #   }
     #   featureImportance <- data.frame(Feature=columns, Importance=imp)
     #   return(featureImportance)
     # }
     # 
     # 
     # columns=c("subject_id",'conflict',
     #           "Caudate40exc","Putamen40exc","Accumbens",
     #           "FFA23","V1","VMPFC31thr","MotorBA4",
     #           "DLPFCposterior","preSMAsmall",'dummy')
     # 
     # 
     # Columns=c(
     #           "Caudate40exc","Putamen40exc","Accumbens",
     #           "FFA23","V1","VMPFC31thr","MotorBA4",
     #           "DLPFCposterior","preSMAsmall",'dummy')
     # 
     # rent=Train
     # 
     # featureImportance <- get_drop_imp(rent, columns, nRareSamples)
     # 
     # featureImportance[order(-featureImportance[,'Importance'],decreasing = T),]
