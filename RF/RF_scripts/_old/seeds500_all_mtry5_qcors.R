#------------------------set-up script to run 1000 RFs with different seed to grasp variability ---------------#
rm(list=ls())
#1- ----------------------load/install libraries ----------------------
# load required packages into memory
library(rpart)
library(rpart.plot)
library(randomForest)
library(RColorBrewer)
library(AUC)
library(doParallel)
library(dplyr)

#2- ----------------------load data, define predictors ----------------------

# define predictors 
CBase=c("response","subject_id",'conflict',
     "Caudate40exc","Putamen40exc","Accumbens", 
     "FFA23","V1","VMPFC31thr","MotorBA4",
     "DLPFCposterior","preSMAsmall","Q.chosen","Q.unchosen","Q.diff")

out=c("Q.chosen","Q.unchosen","Q.diff",'conflict')

# CBase=c("response","Caudate40exc","Putamen40exc","Accumbens", 
#       "FFA23","V1","VMPFC31thr","MotorBA4",
#       "DLPFCposterior","preSMAsmall","Q.chosen","Q.unchosen","Q.diff")

#CBase=c("response",'conflict',"Q.chosen","Q.unchosen","Q.diff")


#setwd('/home/jahfari/Documents/NEW')
#setwd('/Users/sarajahfari/Github/Pearl3T')
setwd('/home/jahfari/Documents/NEW')
load('./RF/RF_data/forestDat_N43.Rdat')
forestinall$conflict=droplevels(forestinall)$conflict
forestinall$option=droplevels(forestinall)$option
forestinall$response=ifelse(forestinall$response==0,'incorret','correct')
#Forestinall=forestinall[,CBase]

# this was initially used to find optimal MTry (5 or 6)
#tuneRF(select(Forestinall,-response), as.factor(Forestinall$response), stepFactor=2,ntree=2000,improve=0.01,plot=T,mtryStart=3)->tune

#3- ----------------------define variables to save resutls and number of seeds --
# variables
Type='ALL'
SEEDS=500
PROX='FALSE'
MTRy=5
NTREE=2000


#data.frames

PREDICT=matrix(,nrow=SEEDS,ncol=3)
rownames(PREDICT)=paste0('seed_',1:SEEDS)
colnames(PREDICT)=c('pTrain','pTest','prandomTest')


AUC=matrix(,nrow=SEEDS,ncol=3)
colnames(AUC)=c('roc','specifity','sensitivity')
rownames(AUC)=paste0('seed_',1:SEEDS)

# IMP=data.frame(regions=1:{length(Forest)-1})
IMP.name=matrix(,nrow=SEEDS,ncol=length(CBase)-4) # save variable names of imp sorted
IMP.Daccuracy=matrix(,nrow=SEEDS,ncol=length(CBase)-4) # save accuracy of imp sorted
colnames(IMP.name)=paste0('Best_',1:length(IMP.name[1,]))
rownames(IMP.name)=paste0('seed_',1:SEEDS)
colnames(IMP.Daccuracy)=paste0('Best_',1:length(IMP.name[1,]))
rownames(IMP.Daccuracy)=paste0('seed_',1:SEEDS)


conf=list()

#4- ----------------------start loop ----------------------

# start for loop over seeds
for (seed in 1:SEEDS){

	#4.0 set seed
	# set.seed(seed)
    set.seed(100)

	#4.1 sample test and training set
    # Define set, all, only good, or only bad
          if (Type=='ALL') {Forestinall=forestinall[,CBase]} else
          if (Type=='good') {Forestinall=forestinall[forestinall$Learner=='good',CBase]} else
          if (Type=='bad') {Forestinall=forestinall[forestinall$Learner=='bad',CBase]}

    #Forestinall=forestinall[,CBase]
    samp=sample(1:dim(Forestinall)[1]) # get shuffle index
    test=Forestinall[samp[1:floor(0.3*length(samp))],] # define test set (1/3 of trials)
    train=Forestinall[-samp[1:floor(0.3*length(samp))],] # define train set (2/3 of trials)

	# get number of counts minority class for stratification
	nRareSamples = min(table(as.factor(train$response)))-1

	# make a random test set to assess basline prediction/shuffle responses
	RandomTest=test
	RandomTest$response=sample(RandomTest$response)


	# run forest
    rf.good.noc = randomForest(as.factor(response) ~ .,
        data=train[,-which(colnames(train)%in%out)],strata=train$response,
             sampsize=c(nRareSamples,nRareSamples),
             ntree=NTREE,importance=TRUE)

    rf.conflict = randomForest(as.factor(response) ~ .,
    data=train[,c('response','conflict')],strata=train$response,
         sampsize=c(nRareSamples,nRareSamples),
         ntree=NTREE,importance=TRUE)

    rf.subject = randomForest(as.factor(response) ~ .,
    data=train[,c('response','subject_id')],strata=train$response,
         sampsize=c(nRareSamples,nRareSamples),
         ntree=NTREE,importance=TRUE)

    # rf.good = randomForest(as.factor(response) ~ .,
    # data=train[,-which(colnames(train)%in%out)],strata=train$response,
    #      sampsize=c(nRareSamples,nRareSamples),
    #      ntree=NTREE,importance=TRUE)

    # rf.strata = randomForest(as.factor(response) ~ .,data=train,strata=train$response,
    #      sampsize=c(nRareSamples,nRareSamples),importance=TRUE,
    #      ntree=NTREE,mtry=MTRy)

    # save output of random forest

    Backtesting2<- predict(rf.subject, train)
    Prediction2 <- predict(rf.subject, test)
    RPrediction2 <- predict(rf.subject, RandomTest)
    ptr2<-length(which(Backtesting2==train$response))/length(train$response)
    pts2<-length(which(Prediction2==test$response))/length(test$response)
    prts2<-length(which(RPrediction2==RandomTest$response))/length(test$response)
    round(c(ptr2,pts2,prts2),digits=2)
    # prediction of test, train, dummy test
    ### Check how well these complex model behave
    # for stratified data 
    Backtesting<- predict(rf.good.noc, train)
    Prediction <- predict(rf.good.noc, test)
    RPrediction <- predict(rf.good.noc, RandomTest)
    ptr<-length(which(Backtesting==train$response))/length(train$response)
    pts<-length(which(Prediction==test$response))/length(test$response)
    prts<-length(which(RPrediction==RandomTest$response))/length(test$response)
    round(c(ptr,pts,prts),digits=2)

    PREDICT[seed,]=round(c(ptr,pts,prts),digits=2)

    # look at relationships predict and Q
    LPprob=predict(rf.subject, train,type='prob')
    Lprob=data.frame(correct=LPprob[,1],incorrect=LPprob[,2],Uncertainty=LPprob[,1]-LPprob[,2])
    cor(Lprob,train[,'Q.diff'])
    # dev.new()
    # plot(Lprob[,1],train[,'Q.diff'],ylab='Q.diff=Q.chosen-Q.unchosen',xlab='P forest predicting optimal on Trial[i]')
    # dev.off()

    TPprob=predict(rf.subject, test,type='prob')
    Tprob=data.frame(correct=TPprob[,1],incorrect=TPprob[,2],Uncertainty=TPprob[,1]-TPprob[,2])
    cor(Tprob,test[,'Q.diff'])

    lm(test[test[,'Q.diff']>0,'Q.diff']~Tprob[test[,'Q.diff']>0,1])->Qplus
    lm(test[test[,'Q.diff']<0,'Q.diff']~Tprob[test[,'Q.diff']<0,1])->Qmin
    lm(abs(test[,'Q.diff'])~Tprob[,1])->Qabs
    
    

    #------------------------------
    pdf("Qdiff_traintest_good218_subjectonly.pdf", 12, 5)
    
    layout(matrix(1:3,1,3))
    
    par(cex= 1, cex.main=1.2,mar = c(4, 5, 1, 2), mgp = c(3.5, 1, 0),
    cex.lab = 1, cex.axis = 1, bty = 'n', las=1, 
    font.axis=1,font.lab=1,font=1,lwd=1, bg='white',pch=20)
    
    plot(Lprob[,1],train[,'Q.diff'],
        ylab='Q.diff=Q.chosen-Q.unchosen',xlab='P choice is optimal [forest]',
        main='Training set',col=ifelse(train[,'Q.diff']>0,'darkgreen','red'),
        xlim=c(0,1),ylim=c(-0.8,0.8))
    abline(h=0,col='red',lty=2)
    abline(v=0.5,col='red',lty=2)

    #z <- lm(train[,'Q.diff'] ~ Lprob[,1])
    #abline(z)

    plot(Tprob[,1],test[,'Q.diff'],
        ylab='Q.diff=Q.chosen-Q.unchosen',xlab='P choice is optimal [forest]',
        main='Validation set',col=ifelse(test[,'Q.diff']>0,'darkgreen','red'),
        xlim=c(0,1),ylim=c(-0.8,0.8))
    abline(h=0,col='red',lty=2)
    abline(v=0.5,col='red',lty=2)
    abline(Qplus,col='black')
    abline(Qmin,col='black')


    plot(Tprob[,1],abs(test[,'Q.diff']),
    ylab='absolute Q.diff',xlab='P choice is optimal [forest]',
    main='Validation set',col='blue',
    xlim=c(0,1),ylim=c(-0.8,0.8))
    abline(h=0,col='red',lty=2)
    abline(v=0.5,col='red',lty=2)
    abline(Qplus,col='black')
    abline(Qabs,col='black')
    #
    #
    dev.off()

    # get AUC, roc, specificity, sensitifity
    AUC[seed,]=round(c(
    	auc(roc(rf.subject$votes[,2],as.factor(train$response))),
    	auc(specificity(rf.subject$votes[,2],as.factor(train$response))),
    	auc(sensitivity(rf.subject$votes[,2],as.factor(train$response)))
    	),digits=2)

    # maybe here see if you can plot curves


    # now get importance of each region
    im2=as.matrix(importance(rf.good,scale=T,type=1))
    imp2.sort =  as.matrix(im2[order(-im2[,'MeanDecreaseAccuracy']),])
    iMP2=data.frame(rownames(imp2.sort),imp2.sort[,1])

    IMP.name[seed,]=rownames(imp2.sort)
    IMP.Daccuracy[seed,]=round(imp2.sort[,1],digits=2)


    # now also save the confusion matrix
    conf[[seed]]=rf.strata$confusion
    print(paste('done' ,seed))

}# end seed loop

names(conf)=paste0('seed_',1:SEEDS)

save(list=c('conf','IMP.name','IMP.Daccuracy','AUC','PREDICT'),
    file=paste('./RF/RF_forests/PAUC_FORESTN43','_ntree',NTREE,
        '_seeds',SEEDS, '_MTRY',MTRy,'type_',Type,'_prox',PROX,'.Rdat',sep=''))




# example for ROC curve plot best model
# pred1=predict(rf.strata,type = "prob")

# library(ROCR)
# perf = prediction(pred1[,2], train$response)

# # 1. Area under curve
# auc = performance(perf, "auc")
# auc

# # 2. True Positive and Negative Rate
# pred3 = performance(perf, "tpr","fpr")

# # 3. Plot the ROC curve
# plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
# abline(a=0,b=1,lwd=2,lty=2,col="gray")

