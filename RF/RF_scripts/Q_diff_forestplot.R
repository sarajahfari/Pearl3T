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
# CBase=c("response","subject_id",'conflict',
#      "Caudate40exc","Putamen40exc","Accumbens",
#      "FFA23","V1","VMPFC31thr","MotorBA4",
#      "DLPFCposterior","preSMAsmall")

CBase=c("response","subject_id",'conflict',
     "Caudate40exc","Putamen40exc","Accumbens",
     "FFA23","V1","VMPFC31thr","MotorBA4",
     "DLPFCposterior","preSMAsmall",'Q.diff')

# this was initially used to find optimal MTry (5 or 6)
#tuneRF(select(Forestinall,-response), as.factor(Forestinall$response), stepFactor=2,ntree=2000,improve=0.01,plot=T,mtryStart=3)->tune

#3- ----------------------define variables to save resutls and number of seeds --
# variables
Type='good'
SEEDS=100
PROX='FALSE'
MTRy=5
NTREE=2000
block=2

#set.seed(218)

#setwd('/home/jahfari/Documents/NEW')
#setwd('/Users/sarajahfari/Github/Pearl3T')
#---------------------------------------------------------------------#
# define test and training set
set.seed(218)

setwd('/Users/sarajahfari/Spinoza/NEW')
#setwd('/home/jahfari/Documents/NEW')
load('./RF/RF_data/forestDat_N43.Rdat')

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

#---------------------------------------------------------------------#
# crusor=seq(1,1000,100)
#
# #data.frames
# PREDICT=matrix(,nrow=SEEDS,ncol=3)
# rownames(PREDICT)=paste0('seed_',crusor[block]:(crusor[block]+99))
# colnames(PREDICT)=c('pTrain','pTest','prandomTest')
#
# AUC=matrix(,nrow=SEEDS,ncol=3)
# colnames(AUC)=c('roc','specifity','sensitivity')
# rownames(AUC)=paste0('seed_',crusor[block]:(crusor[block]+99))
#
# # IMP=data.frame(regions=1:{length(Forest)-1})
# IMP.name=matrix(,nrow=SEEDS,ncol=length(CBase)-2) # save variable names of imp sorted
# IMP.Daccuracy=matrix(,nrow=SEEDS,ncol=length(CBase)-2) # save accuracy of imp sorted
# colnames(IMP.name)=paste0('Best_',1:length(IMP.name[1,]))
# rownames(IMP.name)=paste0('seed_',crusor[block]:(crusor[block]+99))
# colnames(IMP.Daccuracy)=paste0('Best_',1:length(IMP.name[1,]))
# rownames(IMP.Daccuracy)=paste0('seed_',crusor[block]:(crusor[block]+99))
#
# conf=list()
# #------------------------start loop ----------------------
# track=0
# # start for loop over seeds
#
#
# for (seed in crusor[block]:(crusor[block]+99))
# {
#
# 	#4.0 set seed
 	set.seed(891)
#     track=track+1

	# run forest
    rf.strata = randomForest(as.factor(response) ~ .,
        data=train[,-which(colnames(train)=='Q.diff')],strata=train$response,
             sampsize=c(nRareSamples,nRareSamples),
             ntree=NTREE,mtry=MTRy,importance=TRUE)


    # rf.strata2 = randomForest(as.factor(response) ~ .,
    #                          data=train[,-which(colnames(train)=='Q.diff')],
    #                          ntree=NTREE,mtry=MTRy,importance=TRUE)

    # rf.strata = randomForest(as.factor(response) ~ .,data=train,strata=train$response,
    #      sampsize=c(nRareSamples,nRareSamples),importance=TRUE,
    #      ntree=NTREE,mtry=MTRy)

    # save output of random forest

    # prediction of test, train, dummy test
    ### Check how well these complex model behave
    # # for stratified data
    Backtesting<- predict(rf.strata, train)
    Prediction <- predict(rf.strata, test)
    RPrediction <- predict(rf.strata, RandomTest)
    ptr<-length(which(Backtesting==train$response))/length(train$response)
    pts<-length(which(Prediction==test$response))/length(test$response)
    prts<-length(which(RPrediction==RandomTest$response))/length(test$response)
    #
    # PREDICT[track,]=round(c(ptr,pts,prts),digits=2)


    # Backtesting2<- predict(rf.strata2, train)
    # Prediction2 <- predict(rf.strata2, test)
    # RPrediction2 <- predict(rf.strata2, RandomTest)
    # ptr2<-length(which(Backtesting2==train$response))/length(train$response)
    # pts2<-length(which(Prediction2==test$response))/length(test$response)
    # prts2<-length(which(RPrediction2==RandomTest$response))/length(test$response)
    #
    # PREDICT[track,]=round(c(ptr2,pts2,prts2),digits=2)

#     # get AUC, roc, specificity, sensitifity
    # AUC[track,]=round(c(
    # 	auc(roc(rf.strata$votes[,2],as.factor(train$response))),
    # 	auc(specificity(rf.strata$votes[,2],as.factor(train$response))),
    # 	auc(sensitivity(rf.strata$votes[,2],as.factor(train$response)))
    # 	),digits=2)


round(c(
      	auc(roc(rf.strata$votes[,2],as.factor(train$response))),
      	auc(specificity(rf.strata$votes[,2],as.factor(train$response))),
      	auc(sensitivity(rf.strata$votes[,2],as.factor(train$response)))
      	),digits=2)





# # 
# #     
# # 
# #     
# #     
# #     
# #     ci(roc(rf.strata$votes,as.factor(train$response)),of = c("auc"))
# #     # maybe here see if you can plot curves
# # 
# #     # now get importance of each region
    im2=as.matrix(importance(rf.strata,scale=T,type=1))
    imp2.sort =  as.matrix(im2[order(-im2[,'MeanDecreaseAccuracy']),])
    iMP2=data.frame(rownames(imp2.sort),imp2.sort[,1])

    IMP.name[track,]=rownames(imp2.sort)
    IMP.Daccuracy[track,]=round(imp2.sort[,1],digits=2)
# # 
# #     # now also save the confusion matrix
# #     conf[[track]]=rf.strata$confusion
# #     print(paste('done' ,seed))
# # 
# # }# end seed loop
# # 
# # names(conf)=paste0('seed_',crusor[block]:(crusor[block]+99))
# # 
# # save(list=c('conf','IMP.name','IMP.Daccuracy','AUC','PREDICT'),
# #     file=paste('./RF/RF_forests/PAUC_FORESTN43_onetraintest','_ntree',NTREE,'_block',block,
# #         '_seeds',SEEDS, '_MTRY',MTRy,'type_',Type,'_prox',PROX,'.Rdat',sep=''))
# 
# 
# 
# 
# 
# look at relationships predict and Q
LPprob=predict(rf.strata, train,type='prob')
Lprob=data.frame(correct=LPprob[,1],incorrect=LPprob[,2],Uncertainty=LPprob[,1]-LPprob[,2])
cor(Lprob,train[,'Q.diff'])
# dev.new()
# plot(Lprob[,1],train[,'Q.diff'],ylab='Q.diff=Q.chosen-Q.unchosen',xlab='P forest predicting optimal on Trial[i]')
# dev.off()

TPprob=predict(rf.strata, test,type='prob')
Tprob=data.frame(correct=TPprob[,1],incorrect=TPprob[,2],Uncertainty=TPprob[,1]-TPprob[,2])
cor(Tprob,test[,'Q.diff'])

lm(test[test[,'Q.diff']>0,'Q.diff']~Tprob[test[,'Q.diff']>0,1])->Qplus
lm(test[test[,'Q.diff']<0,'Q.diff']~Tprob[test[,'Q.diff']<0,1])->Qmin
lm(abs(test[,'Q.diff'])~Tprob[,1])->Qabs

summary(Qabs)
summary(Qplus)
summary(Qmin)
# 
# 
# 
# setwd('/Users/sarajahfari/Spinoza/NEW/RF/plots/Output/')
# pdf("/Users/sarajahfari/Spinoza/NEW/RF/plots/Output/Qdiff_traintest_good218_subjectonly.pdf")
# 
# 
# 
# 
# 
# # plot(Lprob[,1],train[,'Q.diff'],
# #      ylab='Delta Value',xlab='P_choice(optimal) [forest]',
# #      main='Training set',col=ifelse(train[,'Q.diff']>0,'darkgreen','red'),
# #      xlim=c(0,1),ylim=c(-0.8,0.8))
# # abline(h=0,col='red',lty=2)
# # abline(v=0.5,col='red',lty=2)
# 
# #z <- lm(train[,'Q.diff'] ~ Lprob[,1])
# #abline(z)
# 
# 
# 
#
#dev.new()

layout(matrix(1:2,nrow=1))

par(cex= 1, cex.main=1,mar = c(5, 4, 4, 2)+0.1, mgp = c(3, 1, 0),
    cex.lab = 1, cex.axis = 0.8, bty = 'n', las=1, 
    font.axis=3,font.lab=3,font=3,lwd=1, bg='white',pch=20)


plot(Tprob[,1],test[,'Q.diff'],
     ylab='Delta Value',xlab='P_choice(optimal) [forest]',
     main='Validation set',col=ifelse(test[,'Q.diff']>0,'darkgreen','red'),
     xlim=c(0,1),ylim=c(-1,1),bg='gray',pch=21,axes=F)
lim <- par("usr")
rect(0, lim[3]-1, 0.5, lim[4]-lim[4], col= rgb(1,0,0,alpha=0.2),border='white')
rect(0.5, lim[4]-lim[4], 1, lim[4]+1, col= rgb(0,1,0,alpha=0.2),border='white')
points(Tprob[,1],test[,'Q.diff'],col=ifelse(test[,'Q.diff']>0,'darkgreen','red'),pch=21,bg='gray')
axis(1,seq(0,1,0.1))
axis(2,seq(-1,1,0.5))
abline(Qplus,col='black',lwd=4)
abline(Qmin,col='black',lwd=4)
abline(Qplus,col='green',lwd=1)
abline(Qmin,col='red',lwd=1)
text(0.2,0.9,paste0('R2 = ',round(summary(Qplus)$adj.r.squared,digits=2),' ***'),font=3)
text(0.6,-0.9,paste0('R2 = ',round(summary(Qmin)$adj.r.squared,digits=2),' ***'),font=3)




plot(Tprob[,1],abs(test[,'Q.diff']),
     ylab='Delta Value (abs)',xlab='P_choice(optimal) [forest]',
     main='Validation set',col='gray',pch=21,bg='gray',
     xlim=c(0,1),ylim=c(0,1),axes=F)
lim <- par("usr")
rect(0, lim[3]-1, 0.5, lim[4]+1, col= rgb(1,0,0,alpha=0.2),border='white')
rect(0.5, lim[3]-1, 1, lim[4]+1, col= rgb(0,1,0,alpha=0.2),border='white')
points(Tprob[,1],abs(test[,'Q.diff']),col='black',pch=21,bg='gray')
axis(1,seq(0,1,0.1))
axis(2,seq(0,1,0.2))
abline(Qabs,col='white',lwd=2)
round(summary(Qabs)$adj.r.squared,digits=2) #***
text(0.2,0.9,paste0('R2 = ',round(summary(Qabs)$adj.r.squared,digits=2),' ***'),font=3)
#
#


# dev.off()
# 
# 
# 
# 
# # example for ROC curve plot best model
# pred1=predict(rf.strata,type = "prob")
# 
# library(ROCR)
# perf = prediction(pred1[,2], train$response)
# 
# # # 1. Area under curve
# auc = performance(perf, "auc")
# # auc
# 
# # # 2. True Positive and Negative Rate
# pred3 = performance(perf, "tpr","fpr")
# #pred3 = performance(perf, "sens","spec")
# 
# # # 3. Plot the ROC curve
# plot(pred3,main="ROC Curve for Random Forest",col='darkred',lwd=3)
# abline(a=0,b=1,lwd=2,lty=2,col="gray")
# 
# 
# library(pROC)
# 
# ci(roc(as.factor(train$response),rf.strata$votes[,2]))->CI.auc
# rocobj <-roc(as.factor(train$response),rf.strata$votes[,2])
# 
# plot.roc(as.factor(train$response),rf.strata$votes[,2], grid=c(0.1, 0.2),col='darkgreen',
#                grid.col=c("green", "red"),lwd=2,ci=T,of="thresholds")
# 
# 
# 
# plot.roc(smooth(rocobj), add=TRUE, col="blue")
# legend("bottomright", legend=c("Empirical", "Smoothed"),
#        col=c(par("fg"), "blue"), lwd=2)
# 
# # 
# # plot(rocobj, print.auc=TRUE, grid=c(0.1, 0.2),
# #      grid.col=c("green", "red"), max.auc.polygon=TRUE,
# #      print.thres=TRUE,las=1)
# ci(roc(rf.strata$votes,as.factor(train$response)),of = c("auc"))
# 
# 
